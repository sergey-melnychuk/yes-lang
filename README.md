yes-lang
========

### Yet anothEr Scripting Language

Simple interpreter in Rust:
- "manual" tokenization
- single-/multi-line comments
- prefix & infix operators
- Pratt parser impl, errors (also runtime ones) with line:column
- tree-walking interpreter, and a compiler of the AST into closures (the one that runs)
- lexically scoped bindings
- functions as a first class citizens (closures, recursion)
- arbitrary-precision integers ([num-bigint](https://crates.io/crates/num-bigint))
- strings with escapes (`"a\"b\n"`) and raw hex bytes (`b"a9059cbb"`)
- indexing `x[i]`, slicing `x[a..b]` (bounds are clamped), field access `x.name` / `x["name"]`
- lists `[1, 2]`, records `{to: b"01", "any key": 2}`, membership `x in [1, 2]` / `"ell" in "hello"` / `"to" in tx`, `else if`
- builtins: `len`, `int`, `bytes`, `keccak`, `selector`
- JSON binding (`--bind:NAME=...`, `--stream:NAME`), `"0x..."` strings become bytes
- lexer, parser, interpreter & REPL in <2k LoC (excluding tests)

Goals:
- learn by doing
- easy to understand
- easy to implement
- easy to follow
- test coverage

Non-goals:
- IO
- stdlib
- safety
- type-safety
- production-readiness
- usability in general

#### Build

`cargo build`

`cargo test`

CI runs `cargo fmt --check`, `cargo clippy -- -D warnings`, the tests on stable and on the
minimum supported version (1.88). Fuzzing: see [fuzz](fuzz/README.md).

### Run

Example: [fib.yes](examples/fib.yes).

```shell
$ cargo run --release examples/fib.yes
[...]
true
```

### Example

```
fn fib(n) {
  if n <= 1 { return n; }
  fib(n - 1) + fib(n - 2)
}

// records are provided by the host, e.g. `tx` (see below)
tx.to in [b"dac17f958d2ee523a2206206994597c13d831ec7", b"a0b86991c6218b36c1d19d4a2e9eb0ce3606eb48"]
  && tx.input[..4] == selector("transfer(address,uint256)")
  && int(tx.input[36..68]) >= 1000 * 1000000
```

Builtins:

| builtin | |
|---|---|
| `len(x)` | number of characters of a string, bytes, list items or record fields |
| `int(x)` | integer from big-endian bytes (e.g. an ABI word), a decimal or a `"0x..."` string |
| `bytes(x)` | bytes from a `"0x..."` hex string, or UTF-8 bytes of any other string |
| `keccak(x)` | Keccak-256 of bytes or of a UTF-8 string |
| `selector(s)` | function selector: `keccak(s)[..4]` |
| `map(list, f)` | the list of `f(item)` for each item |
| `filter(list, f)` | the items for which `f(item)` is `true` |
| `any(list, f)`, `all(list, f)` | whether `f(item)` is `true` for any / all items (stop at the first that decides) |

A builtin is used only when nothing is bound to its name, so `let len = 42;` is fine.

Statements are separated by `;`, which is optional for the last statement of a block.
`&&` and `||` short-circuit. Function calls are limited to 10k nested levels.
Values of different types are never equal (`1 == "1"` is `false`, with a warning).
A missing record field is `null`.

More examples: [closures](examples/closures.yes), [return](examples/return.yes),
[logic](examples/logic.yes), [bigint](examples/bigint.yes), [strings](examples/strings.yes),
[bytes](examples/bytes.yes), [equality](examples/equality.yes), [lists](examples/lists.yes),
[records](examples/records.yes), [transfers](examples/transfers.yes) and [large](examples/large.yes)
(ERC-20 transfers from the logs of [receipts](examples/receipts.ndjson)),
[erc20](examples/erc20.yes), [errors](examples/errors).
All of them are run by `cargo test`, with both the tree-walking interpreter and the compiled closures.

### JSON & Ethereum transactions

JSON is converted into records, lists, strings, numbers (arbitrary precision) and `null`.
Every string starting with `0x` that is valid hex becomes bytes (an odd number of digits gets
a leading zero: `"0x1"` is `b"01"`), so addresses, hashes and calldata can be compared with
`b"..."` literals, and hex numbers are read with `int(...)`: `int(tx.value) > 0`.

```shell
# run a script with JSON bound to a name, from a file (@PATH) or inline
$ cargo run -q -- examples/erc20.yes --bind:tx=@examples/tx.json
true
$ cargo run -q -- examples/usdt.yes '--bind:tx={"to": "0xdac17f958d2ee523a2206206994597c13d831ec7", "input": "0xa9059cbb"}'
true

# print the lines of a JSON stream (one JSON per line) matching a predicate
$ cargo run -q -- examples/usdt.yes --stream:tx < examples/block.ndjson
{"accessList":[],"blockHash":"0x0d40...","to":"0xdac17f958d2ee523a2206206994597c13d831ec7",...}
[...]
```

with [usdt.yes](examples/usdt.yes):

```
tx.to == b"dac17f958d2ee523a2206206994597c13d831ec7"
&& tx.input[..4] == selector("transfer(address,uint256)")
```

`--stream:NAME` binds each line to `NAME` (here `tx`, the name the program uses) and runs
the program, which is parsed and compiled once: `true` prints the line, `false` or `null`
skips it, any other value is printed as JSON (bytes as `"0x..."`), so a program can also
select and transform:

```shell
$ echo 'if tx.input[..4] == selector("transfer(address,uint256)") { [tx.hash, int(tx.input[36..])] }' > amounts.yes
$ cargo run -q -- amounts.yes --stream:tx < examples/block.ndjson
["0xbd88e1dbfa28ec178478eadf91b560818fa739907a1802acc30f45a7255da176",6000000]
[...]
$ cargo run -q -- examples/usdt.yes --stream:tx --count < examples/block.ndjson
7
```

Other bindings are visible to the program, e.g.
`--bind:tokens='["0xdac17f958d2ee523a2206206994597c13d831ec7"]'` for `tx.to in tokens`.
An error on a line is reported and the stream goes on.

Limits: `--max-depth:N` of nested calls (default 10000), `--max-calls:N` per run (per line
of a stream, no limit by default), so that a program from somebody else cannot hang the tool.
Neither deep recursion nor deeply nested code can overflow the stack: it grows on the heap as
needed ([stacker](https://crates.io/crates/stacker)), and nesting is limited to 200 levels.

Receipts work the same way, e.g. decoding the transfers of USDC and USDT from their logs with
[transfers.yes](examples/transfers.yes):

```shell
$ cargo run -q -- examples/transfers.yes --stream:receipt < examples/receipts.ndjson
[{"amount":6000000,"from":"0x171c402553e571bf052208aee5bb694b6608a884","to":"0x043e54b4eff4433c7e3b472cbff31bfd28b8a86c","token":"USDC"}]
[...]
```

Exit code, like `grep`: 0 if the result is not `false` (a stream printed something), 1 if it
is `false` (a stream printed nothing), 2 on errors.

### Rust

```rust
use yes_lang::{Context, Limits, Object, Program};

let predicate = Program::compile("tx.to == b\"dac17f958d2ee523a2206206994597c13d831ec7\"")?;
let ctx = Context::default();
// JSON text, or a serde_json::Value
ctx.bind("tx", r#"{"to": "0xdac17f958d2ee523a2206206994597c13d831ec7"}"#)?;
assert_eq!(predicate.run(&ctx)?, Object::Bool(true));

// each run can be limited
let limits = Limits { max_depth: 100, max_calls: Some(10_000) };
predicate.run_with(&ctx, limits)?;

// for many inputs: bind each in a new scope, which is released after the run
for tx in [r#"{"to": "0x01"}"#, r#"{"to": "0x02"}"#] {
    let scope = ctx.fork();
    scope.bind("tx", tx)?;
    assert_eq!(predicate.run_once(scope, limits)?, Object::Bool(false));
}
```

`Program::run` and `run_with` put the bindings made by the program (`let`, `fn`) into the
context, like a REPL. `Program::run_once` releases the context after the run: use it for
the items of a stream, so that nothing but the results is kept (a function bound in the
scope it captures is a cycle, which is freed at the end of each run).
`yes_lang::to_json` converts a result back to JSON.

`cargo test --release bench -- --ignored --nocapture` measures streaming on
[block.ndjson](examples/block.ndjson): converting a transaction from JSON takes ~3.6µs
(JSON is deserialized directly into objects, bytes are shared, not copied, by slices and
field access), running `usdt.yes` on it ~230ns (~390ns with the tree-walking interpreter).
The command line tool streams ~230k transactions per second on one core.

Blocks from an RPC node can be streamed in directly, e.g.:
`cast block latest --full --json | jq -c '.transactions[]' | yes-lang usdt.yes --stream:tx`.

### REPL

```shell
$ cargo run
[...]
REPL: enter :q to quit.
> let f = fn(x) { fn(y) { x + y } };
null
> let hi = f("hello ");
null
> hi("world")
"hello world"
> f(40)(2)
42
> :load examples/fib.yes
true
> fib(10)
55
> :bind tx=@examples/tx.json
bound 'tx'
> tx.input[..4] == selector("transfer(address,uint256)")
true
> int(tx.input[36..])
30000000
> let x = ;
Error: Unexpected 'Delimiter(';')' at 1:9
> :q
$
```
