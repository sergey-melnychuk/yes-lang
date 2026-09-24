yes-lang
========

### Yet anothEr Scripting Language

A small scripting language for rules over data, made to be embedded in Rust: predicates,
filters and transforms that are safe to run even when somebody else wrote them. It comes with
a command line tool for JSON (streams of Ethereum transactions and receipts, for example):

```shell
$ yes-lang examples/usdt.yes --stream:tx < examples/block.ndjson   # print the USDT transfers
```

The language is described in [LANG.md](LANG.md), ideas and plans are in [NOTES.md](NOTES.md).

**The language**: lexically scoped closures and recursion, integers of arbitrary precision,
floats, strings, bytes (`b"a9059cbb"`), lists and records (`{to: b"01", amount: 2}`), indexing,
slicing and field access, membership (`x in [1, 2]`), `map`/`filter`/`any`/`all`, immutable
values, no loops. Errors, also at runtime, point at their line and column.

**Safe to run**: every run is limited (nested calls, calls per run, nesting of the source),
the stack grows as needed so neither recursion nor nesting can crash the host, the memory of
a run is freed after it (also cycles of closures), no IO, deterministic.

**Data and Ethereum** (optional features): JSON in and out (`0x...` strings as bytes, or
not), Keccak-256, selectors, ABI decoding of calldata (`abi`) and event logs (`event`).

**Embedding**: compile once, run many times, bind JSON or values, add natives in Rust
(`Native::pure`, `Native::new`); the language alone depends only on `num-bigint` and
`stacker`.

**Implementation**: a hand-written lexer, a Pratt parser, a tree-walking interpreter and a
compiler of the syntax tree into closures (the one that runs): every test and every fuzzed
program runs with both, and they must agree. ~3.3k lines of Rust without tests, ~7k with.

Goals:
- easy to understand and to follow: the code is meant to be read
- safe to run programs written by somebody else: bounded, deterministic, no crashes
- easy to embed in Rust
- correct: tests, differential testing of the two interpreters, fuzzing
- useful for rules over data, Ethereum data in particular

Non-goals:
- IO in the language: the host reads and writes, programs compute
- a general-purpose language: no loops or mutation (see [NOTES.md](NOTES.md) for why and what
  could change)
- speed at any cost: no JIT, the time goes to JSON parsing anyway
- a standard library of everything: builtins are few, the host adds natives

#### Build

`cargo build`

`cargo test`

CI runs `cargo fmt --check`, `cargo clippy -- -D warnings`, the tests on stable, for the
language alone and with each optional feature (`json`, `eth`), and on the minimum supported
version (1.88). Fuzzing: see [fuzz](fuzz/README.md).

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
let transfer = abi(tx.input, "transfer(address to, uint256 amount)"); // null for other calls

tx.to in [b"dac17f958d2ee523a2206206994597c13d831ec7", b"a0b86991c6218b36c1d19d4a2e9eb0ce3606eb48"]
  && transfer != null && transfer.amount >= 1000 * 1000000
```

Builtins, in every program:

| builtin | |
|---|---|
| `len(x)` | number of characters of a string, bytes, list items or record fields |
| `int(x)` | integer from big-endian bytes (e.g. an ABI word), a decimal or a `"0x..."` string |
| `bytes(x)` | bytes from a `"0x..."` hex string, or UTF-8 bytes of any other string |
| `hex(b)` | bytes as a `"0x..."` hex string, e.g. to look up an address in a record |
| `map(list, f)` | the list of `f(item)` for each item |
| `filter(list, f)` | the items for which `f(item)` is `true` |
| `any(list, f)`, `all(list, f)` | whether `f(item)` is `true` for any / all items (stop at the first that decides) |

A builtin is used only when nothing is bound to its name, so `let len = 42;` is fine.

Ethereum natives, with the `eth` feature (defined by the command line tool):

| native | |
|---|---|
| `keccak(x)` | Keccak-256 of bytes or of a UTF-8 string |
| `selector(s)` | function selector: `keccak(s)[..4]` |
| `abi(calldata, "transfer(address to, uint256 amount)")` | the arguments of a call: `{to: b"...", amount: 30000000}`, `null` if the selector differs |
| `abi_decode(data, "(address,uint256)")` | values encoded with the ABI |
| `event(log, "Transfer(address indexed from, address indexed to, uint256 value)")` | the values of an event, `null` if the log is another event |

Values with names in the signature are decoded into a record, without names into a list:
all the types of the ABI (`address`, `bool`, `uint<N>`, `int<N>`, `bytes<N>`, `bytes`,
`string`, arrays `T[]`, `T[k]`, tuples), nested in any way. Decoding is strict: invalid
data (out of bounds, not padded, out of range) is an error, not a guess.

Statements are separated by `;`, which is optional for the last statement of a block.
`&&` and `||` short-circuit. Function calls are limited to 10k nested levels.
Values of different types are never equal (`1 == "1"` is `false`, with a warning).
A missing record field is `null`.

More examples: [closures](examples/closures.yes), [return](examples/return.yes),
[logic](examples/logic.yes), [bigint](examples/bigint.yes), [strings](examples/strings.yes),
[bytes](examples/bytes.yes), [equality](examples/equality.yes), [lists](examples/lists.yes),
[records](examples/records.yes), [transfers](examples/transfers.yes) and [large](examples/large.yes)
(ERC-20 transfers from the logs of [receipts](examples/receipts.ndjson), with `event`),
[erc20](examples/erc20.yes), [errors](examples/errors).
All of them are run by `cargo test`, with both the tree-walking interpreter and the compiled closures.

### JSON & Ethereum transactions

JSON is converted into records, lists, strings, numbers (arbitrary precision) and `null`.
By default, the command line tool makes every string starting with `0x` that is valid hex
bytes, as Ethereum JSON-RPC writes them (an odd number of digits gets a leading zero: `"0x1"`
is `b"01"`), so addresses, hashes and calldata can be compared with `b"..."` literals, and hex
numbers are read with `int(...)`: `int(tx.value) > 0`. With `--json:plain`, strings stay strings.

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
of a stream: 1000000 by default, no limit for a single run), so that a program from somebody
else cannot hang the tool: without loops, calls bound both the time and the memory of a run.
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

The language itself depends only on `num-bigint` and `stacker`. JSON (`json`) and Ethereum
(`eth`) are optional modules, enabled by default: `default-features = false` for the language
alone. Both are built on the same API a host can use to add its own natives.

```rust
use yes_lang::json::Json;
use yes_lang::{Context, EvalError, Limits, Native, Object, Program, eth};

let predicate = Program::compile("tx.to == b\"dac17f958d2ee523a2206206994597c13d831ec7\"")?;
let ctx = Context::default();
// JSON text, or a serde_json::Value: 'Json::hex' makes "0x..." strings bytes
ctx.bind("tx", Json::hex(r#"{"to": "0xdac17f958d2ee523a2206206994597c13d831ec7"}"#))?;
assert_eq!(predicate.run(&ctx)?, Object::Bool(true));

// natives: of Ethereum ('keccak', 'abi', ...), or your own
eth::define(&ctx);
ctx.define(Native::pure("double", 1, |args| match &args[0] {
    Object::Int(x) => Ok(Object::Int(x * 2)),
    arg => Err(EvalError::Argument("double", arg.clone())),
}));
assert_eq!(Program::compile("double(21)")?.run(&ctx)?, Object::Int(42.into()));

// each run can be limited
let limits = Limits { max_depth: 100, max_calls: Some(10_000) };
predicate.run_with(&ctx, limits)?;

// for many inputs: bind each in a new scope, which is released after the run
for tx in [r#"{"to": "0x01"}"#, r#"{"to": "0x02"}"#] {
    let scope = ctx.fork();
    scope.bind("tx", Json::hex(tx))?;
    assert_eq!(predicate.run_once(scope, limits)?, Object::Bool(false));
}
```

`Program::run` and `run_with` put the bindings made by the program (`let`, `fn`) into the
context, like a REPL. `Program::run_once` releases the context after the run: use it for
the items of a stream, so that nothing but the results is kept (a function bound in the
scope it captures is a cycle, which is freed at the end of each run).
`yes_lang::json::to_json` converts a result back to JSON. `Native::pure` is for natives whose
result depends only on the arguments: a call with literal arguments is evaluated once.
`Native::new` gets a `Caller` to call functions passed to it (like `map`).

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
