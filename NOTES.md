Notes & ideas
=============

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
