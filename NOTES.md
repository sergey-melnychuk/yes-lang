Notes & ideas
=============

### Development vectors of the language (2026-09-24)

Where the language could go, roughly by value for its use as a filter/scripting language
embedded in tools, with what each costs (see [LANG.md](LANG.md) for what it is now).

**Small, high value** (no design risk, mostly builtins):
- Library functions: `reduce(list, init, f)`, `range(n)`, `sum`, `min`/`max`/`abs`, `sort`,
  `reverse`, `keys`/`values` of records, list concatenation with `+`, string functions
  (`lower`, `upper`, `split`, `starts_with`, `ends_with`, `replace`, `format`), bytes and
  numbers (`to_bytes32(int)`, signed `int` from bytes). With `reduce` and `range`, most uses
  of loops are covered without loops.
- Null-safe access and defaults: `tx.receipt?.status`, `x ?? 0` (now `null.x` is an error).
- `if` as an expression (`let size = if n > 10 { "big" } else { "small" };`), `return;`.
- Error messages: show the source line with a caret under the position.

**Medium** (some design):
- Destructuring and pattern matching: `let {to, amount} = abi(...)`, `match` on values and
  shapes of records, which also reads better than chains of `&&`.
- Recoverable errors: `try(expr, default)`, or a `?`-like operator, so that a predicate can
  treat invalid data as `false` instead of failing the line.
- Modules: `import "erc20.yes"`, or libraries provided by the host as namespaces (`eth.abi`),
  so that common decoders and constants can be shared.
- Ethereum: contract ABIs from JSON files (`let erc20 = abi_json(@"erc20.json");
  erc20.decode(tx.input)`), decoding by the 4-byte selector database, traces, fetching
  blocks and receipts from an RPC node in the tool (`--rpc URL --blocks N..M`), which the
  language itself does not need (IO stays in the host).

**Large, with consequences** (they change what the language is):
- **Loops** (`for x in list { ... }`, `while`): useful mostly together with mutation, and they
  change the safety story: the budget must count steps, not only calls (`max_calls` bounds
  time and memory only because there are no loops), and garbage can then grow within a run,
  which needs a collector running during a run (see "Memory" below). `reduce`/`range` first,
  loops only if they are not enough.
- **Mutation**: rebinding (`x = x + 1`) is simple; mutable lists and records are not (sharing,
  aliasing, cycles through data). Keeping values immutable is a feature for predicates.
- **Types**: optional annotations with a static check (`fn f(tx: Tx) -> Bool`), and shapes of
  records (e.g. of a transaction) known to the host, would catch typos like `tx.too` before
  running, and make programs faster (specialized code, no checks at runtime). They are also
  what compiling predicates to circuits needs (see "ZK-provable predicates").
- **Performance**: a bytecode VM with resolved variables (slots instead of names) and interned
  names of fields; parallel streams (a `Program` is not `Send` because of `Rc`: compile one
  per thread, or make compiled programs `Send`); faster JSON (`simd-json`). JSON parsing is
  most of the time of a stream now, not the program.
- **Other hosts**: Python (PyO3), JavaScript (WASM, see the playground), a C API.

My order: the library functions, null-safe access, `if` as an expression and better error
messages first (small, used by every program), then destructuring, `try` and ABIs from JSON,
and types only if programs get big or the ZK idea is pursued; loops last, if ever.

### Database queries (2026-09-24)

What makes yes-lang unusual (bounded, deterministic, no side effects) is what queries written
by users and run by a server need. Three levels, by effort:

1. **Filter rows in the application** (works now): stream rows as JSON through `--stream`, or
   `Program::run_once` per row from Rust. Fine for moderate volumes, but every row leaves the
   database first.
2. **A user-defined function in the database**: register yes-lang in SQLite, DuckDB or
   Postgres (`pgrx`), e.g.
   `SELECT * FROM txs WHERE yes('abi(tx.input, "transfer(address to, uint256 amount)") != null', tx)`.
   The same interpreter, so the semantics stay exact; the limits and the growing stack mean
   that filters submitted by customers of a multi-tenant system (an indexer, hosted analytics)
   cannot hang or crash the database. DuckDB is the easiest start: an extension API in Rust,
   columnar data, fast scans.
3. **Translate predicates into SQL** (pushdown): the simple part (fields, comparisons,
   `&&`/`||`, `in`, literals) becomes a real `WHERE` that uses indexes; what cannot be
   translated (calls, `abi`, recursion) is evaluated in-process or by the function of 2.
   The semantics must stay exact, which is subtle:
   - SQL has three-valued logic: `NULL = x` is unknown, in yes-lang `null == x` is `false`, so
     `!=` and `!` have to be translated, not copied;
   - integers of any size map to `NUMERIC`, bytes to `BYTEA`, comparisons of different types
     must keep the rules of yes-lang (never equal, ordering is an error);
   - the translator needs the types of the columns (the schema), another use of the optional
     types among the development vectors above.

   A differential test like the one of the two interpreters keeps it honest: run each predicate
   translated to SQL and in the interpreter over the same rows, the results must be the same.

For chain data: subscription filters ("notify me when..."), queries of indexers, analytics over
decoded transactions and events, like Dune or The Graph, but with safe, bounded filters that
users write themselves.

Order: a DuckDB extension first (quick, proves the idea, exact semantics), then pushdown of the
translatable part, with the differential test of SQL against the interpreter.

### A playground in the browser: WASM (2026-09-24)

The language (without the tool) is pure Rust without IO, so it can be compiled to WebAssembly
(`wasm32-unknown-unknown`) and run in a web page:
- a page with an editor (e.g. CodeMirror), a field for the input JSON (a transaction, a
  receipt), and the result or the error (with the position marked in the editor);
- the examples to pick from (`usdt.yes`, `transfers.yes`, ...), sharing a program and its
  input by the URL;
- fetching a transaction or a receipt by its hash from a public RPC node in the browser, to
  try a predicate on live data;
- an API with `wasm-bindgen`: `compile(source) -> Program | Error`, `run(program, bindings
  JSON) -> result JSON | Error` (with the `Strings` mode), hosted as a static page (GitHub
  Pages).

Checked: the library with all its features compiles for `wasm32-unknown-unknown` as it is
(`cargo check --lib --target wasm32-unknown-unknown`), `stacker` (growing the stack) has
support for `wasm32` through `psm`. Still to check at runtime, in a browser: growing the
stack in WASM (if it does not work there, lower the default depth and nesting limits for
WASM, the stack of a module is fixed), and the size of the module.

### Memory: reference counting and cycles (2026-09-24)

Values and scopes are reference-counted (`Rc`). A function keeps the scope it was defined
in, and a named function is bound in that very scope (`fn f() {}` puts `f` into the scope
`f` keeps), so reference counting alone never frees them: a cycle.

Now: the scopes created during a run are tracked, and at the end of the run, those that
neither the result nor the kept context (`Program::run`) reach are cleared, which breaks
the cycles (`eval::run_collecting`). Only scopes that a closure can capture are tracked:
calls of functions and blocks that create no functions (checked once, statically) cannot be
a part of a cycle and are freed as soon as they are done; blocks that bind nothing get no
scope at all. The garbage of a run is freed only at its end, but without loops it is bounded
by the calls of the run (`Limits::max_calls`, 1M per line of a stream by default).

Later, if the language gets loops or long-running scripts, garbage could grow without calls
and a run could last forever. Then a real collector is needed, collecting during a run:
- roots are then also the values the interpreter holds while evaluating (arguments being
  evaluated, a list being built, the function `map` is calling), not only the result;
- scopes in an arena (indices instead of `Rc`), with mark & sweep when the arena grows by
  some factor since the last collection; values reference scopes by index;
- the roots: the scopes of the active calls (a shadow stack, pushed and popped around calls
  and blocks), plus the temporaries, e.g. registered in a "handle scope" during a call,
  like in V8, or evaluation redesigned around an explicit value stack (a bytecode VM),
  where the roots are simply the stack;
- the latter comes naturally with a bytecode VM, which would also be the next step for speed.

### ZK-provable predicates (2026-09-23)

Idea: a yes-lang predicate over a tx (`tx.to == ... && tx.data[0..4] == b"..."`) is a pure,
deterministic computation, so its result can potentially be proven in zero knowledge:
"tx T satisfies predicate P" without revealing T, or P, or both.

Two routes:

1. **Run the interpreter in a zkVM** (SP1, RISC Zero, ...): compile the existing Rust
   interpreter to RISC-V and prove its execution. No language changes, works today;
   the cost is interpreter overhead (tree-walking, BigInt, allocations) in every proof.
2. **Compile predicates to a circuit** (R1CS / PLONKish / AIR), like Noir or Circom do:
   much cheaper proofs, but the language must fit the circuit model:
   - bounded execution: no unbounded recursion (loops/calls unrolled to a fixed depth);
   - fixed sizes: bytes/lists with a max length, slice bounds known or bounded;
   - native `Field` type (mod p) is cheap, but integer semantics differ: `<`, integer `/`,
     `%` and u256 need range checks / bit decomposition / limbs, so keep bounded `Int`
     types alongside `Field`;
   - `&&`, `||`, `==` become arithmetic constraints; short-circuit has no benefit
     (both sides are always constrained);
   - no floats; determinism everywhere (records already use `BTreeMap`, keep it so).

Open questions:
- **Provenance** is what makes a proof useful: the tx must be bound to the chain,
  e.g. keccak(RLP(tx)) == tx hash, plus inclusion in the block's tx trie under a known
  block hash. Keccak and MPT proofs are expensive in circuits (zkVMs have precompiles).
- **Completeness**: proving that *no* tx in block N matches (or *all* matches were
  reported) requires iterating over every tx of the block inside the proof.
- What stays private: the tx (prove compliance of private flow against a public rule),
  or the predicate (prove a match against a secret filter), or both.
- A pure-expression "filter" subset of the language (no `fn`/recursion) is a natural
  target: it always terminates and maps directly onto a circuit.
