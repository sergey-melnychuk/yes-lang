yes-lang reference
==================

yes-lang is a small, dynamically typed scripting language, made to be embedded: a host
(the command line tool, or a Rust program) binds values (usually JSON) to names, runs a
program, and uses its result, e.g. as a predicate over Ethereum transactions:

```
let transfer = abi(tx.input, "transfer(address to, uint256 amount)");
tx.to == b"dac17f958d2ee523a2206206994597c13d831ec7" && transfer != null && transfer.amount > 1000
```

A program is a sequence of statements, its result is the value of the last one. Values are
immutable, there are no loops (recursion and `map`/`filter`/`any`/`all` instead), and every
run is limited, so that a program from somebody else cannot hang or crash its host.

Contents: [lexical structure](#lexical-structure), [values](#values), [expressions](#expressions),
[statements](#statements), [functions and scopes](#functions-and-scopes), [builtins](#builtins),
[errors and warnings](#errors-and-warnings), [limits](#limits), [the host](#the-host),
[grammar](#grammar), [not in the language](#not-in-the-language).

## Lexical structure

- **Comments**: `// to the end of the line`, `/* over lines */` (not nested).
- **Whitespace** separates tokens, line breaks are whitespace too.
- **Identifiers**: a letter or `_`, then letters, digits and `_`: `x`, `tx_hash`, `_1`.
- **Keywords**: `let`, `fn`, `if`, `else`, `return`, `true`, `false`, `null`, `in`.
- **Integers**: decimal digits, of any size: `0`, `42`, `115792089237316195423570985008687907853269984665640564039457584007913129639935`.
  Negative numbers are negated literals: `-1`. There are no hex, octal or `_` in integers.
- **Floats**: digits, a dot, digits: `1.5`, `0.25`. Not `1.`, `.5` or `1e3`.
- **Strings**: `"..."`, with the escapes `\"`, `\\`, `\n`, `\t`, `\r` (other escapes are errors).
- **Bytes**: `b"..."` with an even number of hex digits, `0x` is allowed, the case of the
  digits does not matter: `b"a9059cbb"`, `b"0xA9059CBB"`, `b""`.

## Values

| type | literals | |
|---|---|---|
| `Null` | `null` | nothing: a missing field, a function without a result |
| `Bool` | `true`, `false` | |
| `Int` | `42`, `-1` | an integer of arbitrary precision: never overflows |
| `Float` | `1.5` | a 64-bit floating point number |
| `Str` | `"text"` | a string of Unicode characters |
| `Bytes` | `b"a905"` | a sequence of bytes: addresses, hashes, calldata |
| `List` | `[1, "a", [2]]` | a sequence of any values |
| `Record` | `{to: b"01", "any key": 2}` | values by names (strings), ordered by the names: `{b: 1, a: 2}` is `{a: 2, b: 1}` |
| `Func` | `fn(x) { x + 1 }` | a function of the program |
| `Native` | (e.g. `len`) | a function implemented by the host, see [builtins](#builtins) |

All values are immutable: operations make new values. Bytes, lists and records are shared,
not copied, when they are passed around, and slices of bytes share the bytes they are taken
from. Conditions must be booleans: there is no "truthiness", `if 1 { ... }` is an error.

Values print as they are written: `"a\"b"`, `b"ff"`, `1.0`, `[1, {a: null}]`. Functions
print as `<function>` and `<native len>`.

## Expressions

From the loosest to the tightest binding:

| operators | | |
|---|---|---|
| `\|\|` | or | left to right |
| `&&` | and | left to right |
| `==` `!=` | equality | left to right |
| `<` `<=` `>` `>=` `in` | comparison, membership | left to right |
| `+` `-` | addition, subtraction, concatenation | left to right |
| `*` `/` `%` | multiplication, division, remainder | left to right |
| `-x` `!x` | negation, not | |
| `f(a, b)` `x[i]` `x[a..b]` `x.name` | call, index, slice, field | left to right |

`( )` groups. So `!a.b[0]` is `!((a.b)[0])`, and `a || b && c` is `a || (b && c)`.
Comparisons do not chain: `1 < x < 3` compares a boolean with 3, use `1 < x && x < 3`.

**Arithmetic** (`+ - * / %`):
- `Int` with `Int` gives an `Int`: `/` truncates towards zero (`-7 / 2` is `-3`), `%` has the
  sign of the dividend (`-7 % 2` is `-1`), dividing by zero is an error.
- A `Float` with an `Int` or a `Float` gives a `Float`, by the rules of IEEE 754
  (`1.0 / 0` is `inf`).
- `+` concatenates strings (`"a" + "b"`) and bytes (`b"01" + b"02"`), not lists or records.
- `-x` negates a number, `!x` a boolean. Anything else is an error.

**Comparison** (`< <= > >=`): numbers by value (`1 < 1.5`), strings by their bytes (`"B" < "a"`).
Anything else, e.g. bytes or booleans, is an error.

**Equality** (`== !=`) works for any two values:
- numbers are equal by value (`1 == 1.0`), NaN is not equal to anything;
- lists are equal when their items are (by the same rules), records when they have the same
  names with equal values, functions only to themselves;
- values of different types are never equal: `1 == "1"` is `false`, and a warning is reported
  (except for comparisons with `null`, see [warnings](#errors-and-warnings)).

**Logic** (`&& ||`): both sides must be booleans. The right side is evaluated only when the left
does not decide the result, so `x != null && x.a > 1` is safe, and `false && 1 / 0` is `false`.

**Membership** (`a in b`):
- an item of a list: `2 in [1, 2]`, compared like with `==` (so `2.0 in [1, 2]`);
- a substring: `"ell" in "hello"`, bytes in bytes: `b"059c" in b"a9059cbb"`;
- a name of a record: `"to" in tx`.

**Calls** `f(a, b)`: any expression that is a function can be called, `f()(1)` or `(fn(x) { x })(1)`.
The number of arguments must match. Arguments are evaluated from left to right before the call.

**Indexing** `x[i]`:
- `Bytes[i]` is the byte as an `Int`, `Str[i]` the character as a `Str`, `List[i]` the item;
  `i` must be an `Int` within `0..len(x)`, otherwise it is an error (no negative indices);
- `Record["name"]` is a field, `null` if it is missing.

**Slicing** `x[a..b]`, `x[a..]`, `x[..b]`, `x[..]`, of `Bytes`, `Str` (by characters) and `List`:
the bounds are clamped to the length, so slicing never fails: `b""[..4]` is `b""`, and a range
with `a >= b` is empty.

**Fields** `x.name` of a record: `null` if it is missing. A field of anything else is an error,
also of `null`: `tx.to.x` fails if `to` is missing, check with `tx.to != null && ...`.

**Lists and records**: `[a, b,]` and `{name: a, "any key": b,}`, a trailing comma is allowed.
A record cannot have a name twice.

**Function literals**: `fn(a, b) { a + b }`, see [functions](#functions-and-scopes).

## Statements

```
let x = 1 + 2;              // binds a name
fn add(a, b) { a + b }      // defines a function
if x > 2 { "big" } else if x > 0 { "small" } else { "none" }
return x;                   // leaves the function (or the program), with a value
x * 2                       // an expression
```

- Statements are separated by `;`, which can be left out after the last statement of a
  block (before `}`) or of the program, and after `if` and `fn` statements.
- A block `{ ... }`, a function body or the program gives the value of its last statement.
  `let` and `fn` statements give `null`, as does an empty block.
- `if` is a statement: its value is the value of the block that ran (`null` without `else`),
  it cannot be used inside an expression.
- `return` needs a value: `return null;`, not `return;`. It leaves the innermost function
  from any depth of blocks, at the top level it ends the program.

## Functions and scopes

```
fn adder(n) { fn(x) { x + n } }     // a closure: keeps 'n' of its call
let add10 = adder(10);
add10(1)                            // 11
```

- Scoping is lexical: a function sees the names of the scope it is defined in, not of the
  scope it is called from.
- A named function `fn f() {}` is bound in the scope it is defined in, and can call itself
  and other functions of that scope, also ones defined after it (mutual recursion): names
  are looked up when a call runs. A function must be defined before it is called.
- `let` binds a name in the current scope. Binding the same name again in the same scope
  replaces it, also for the closures of that scope (they see the scope, not a copy).
- Each call and each block of an `if` has its own scope: `let` in a block is not visible
  after it.
- Functions are values: they can be passed, returned, kept in lists and records.

## Builtins

In every program, unless the name is bound (`let len = 1;` shadows `len`):

| builtin | |
|---|---|
| `len(x)` | characters of a string, bytes, items of a list, fields of a record |
| `int(x)` | an `Int` from big-endian bytes (e.g. an ABI word), a decimal or `"0x..."` string |
| `bytes(x)` | `Bytes` from a `"0x..."` hex string (an odd number of digits gets a leading zero), or the UTF-8 of any other string |
| `hex(b)` | bytes as a `"0x..."` string, e.g. to look up an address in a record |
| `map(list, f)` | the list of `f(item)` |
| `filter(list, f)` | the items for which `f(item)` is `true` |
| `any(list, f)` | whether `f(item)` is `true` for any item, stops at the first |
| `all(list, f)` | whether `f(item)` is `true` for all items, stops at the first `false` |

Ethereum natives, defined by the command line tool (in Rust: `eth::define`):

| native | |
|---|---|
| `keccak(x)` | Keccak-256 of bytes or of the UTF-8 of a string |
| `selector(s)` | `keccak(s)[..4]`: `selector("transfer(address,uint256)") == b"a9059cbb"` |
| `abi(calldata, "transfer(address to, uint256 amount)")` | the arguments of a call, `null` if the selector differs |
| `abi_decode(data, "(address,uint256)")` | values encoded with the ABI (without a selector) |
| `event(log, "Transfer(address indexed from, address indexed to, uint256 value)")` | the values of an event from a log `{topics, data}`, `null` if the log is another event |

ABI decoding supports `address`, `bool`, `uint<N>`/`int<N>` (`uint` is `uint256`), `bytes<N>`,
`bytes`, `string`, arrays `T[]` and `T[k]`, tuples `(T, ...)` (or `tuple(T, ...)`), nested in
any way; names and `indexed` are optional, `memory`/`calldata` are ignored. Values with names
become a record, without names a list; addresses and `bytes<N>` become bytes, integers `Int`.
An indexed value of a dynamic type (`string`, `bytes`, arrays, tuples) is its hash (bytes).
Invalid data (too short, not padded, out of range) is an error.

A host can add its own natives, see [the host](#the-host).

## Errors and warnings

An error stops the run, and says where it happened: `Division by zero at 3:3`,
`Not found: 'y' at 2:10`, `Unexpected 'Delimiter(';')' at 2:6`. There are no exceptions in the
language: an error cannot be caught. Common runtime errors:

- a name that is not bound (`Not found`), calling what is not a function, a wrong number of
  arguments;
- an operator or builtin applied to values of the wrong types, a condition that is not a
  boolean;
- dividing an integer by zero, an index out of bounds, a field of a value that is not a record;
- invalid data for `abi`/`event`, a value that cannot be converted to JSON (a function, `inf`);
- exceeding a [limit](#limits).

A warning does not stop the run: comparing values of different types with `==` or `!=`
(always `false`/`true`) warns once per pair of types, e.g.
`Comparing Str with Int is always false`, except for comparisons with `null`.

## Limits

- **Nested calls**: at most 10000 by default (`Stack overflow: call depth exceeds 10000`).
- **Calls per run**: no limit by default, 1000000 per line of a stream in the command line
  tool (`--max-calls:N`). Without loops, calls bound both the time and the memory of a run.
- **Nesting** of expressions and blocks in the source: at most 200 levels
  (`Nesting too deep`), e.g. `((((...` or a very long `1 + 1 + ... + 1`.

The stack grows as needed, so neither recursion nor nesting can crash the host.

## The host

**JSON** is converted into values: `null`, booleans, numbers (integers of any size, others as
floats), strings, arrays as lists, objects as records. With "hex strings" (the default of the
command line tool, `Json::hex` in Rust), a string starting with `0x` that is valid hex becomes
bytes, as Ethereum JSON-RPC writes addresses, hashes, calldata and numbers (`int(tx.value)`);
`--json:plain` (`Json::plain`) keeps strings as strings.

**Results** are converted back to JSON by the tool: bytes as `"0x..."` strings, integers
exactly, records as objects. Functions, `inf` and NaN cannot be converted.

**The command line tool** runs a file, or a stream of JSON lines (`--stream:NAME`): each line
is bound to `NAME` in its own scope, and the result decides the output: `true` prints the line,
`false` and `null` skip it, any other value is printed as JSON.

```shell
yes-lang predicate.yes --bind:tx=@tx.json          # a single value
yes-lang predicate.yes --stream:tx < txs.ndjson    # a stream
```

**From Rust**: `Program::compile(source)` once, then `run`/`run_with` (bindings stay in the
context, like a REPL) or `run_once` (the context is released: for each item of a stream);
`Context::bind(name, value)` binds values (`Json::hex(text)`, `Json::plain(value)`, `Object`),
`Context::define(Native::pure(name, arity, |args| ...))` adds a native, `Native::new` for one
that calls functions it gets (like `map`). See the [README](README.md).

## Grammar

```
program     = block
block       = { statement | ";" }
statement   = "let" IDENT "=" expr end
            | "return" expr end
            | "if" expr braced [ "else" ( "if" ... | braced ) ]
            | "fn" IDENT params braced
            | expr end
end         = ";" | (before "}" or the end of the input)
braced      = "{" block "}"
params      = "(" [ IDENT { "," IDENT } ] ")"

expr        = prefix { postfix | INFIX expr }        (by precedence, see the table above)
prefix      = ( "-" | "!" ) expr
            | INT | FLOAT | STRING | BYTES | "true" | "false" | "null" | IDENT
            | "(" expr ")"
            | "[" [ expr { "," expr } [","] ] "]"
            | "{" [ key ":" expr { "," key ":" expr } [","] ] "}"
            | "fn" params braced
key         = IDENT | STRING
postfix     = "(" [ expr { "," expr } [","] ] ")"
            | "[" expr "]"
            | "[" [ expr ] ".." [ expr ] "]"
            | "." IDENT
INFIX       = "||" | "&&" | "==" | "!=" | "<" | "<=" | ">" | ">=" | "in"
            | "+" | "-" | "*" | "/" | "%"
```

## Not in the language

On purpose or not yet (see the development notes in [NOTES.md](NOTES.md)): loops, assignment
and mutation, exceptions, types and type annotations, modules and imports, string formatting,
hex integer literals and exponents in floats, `\u` escapes, `return` without a value,
chained comparisons, `if` as an expression.
