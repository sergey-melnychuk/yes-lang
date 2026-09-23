Notes & ideas
=============

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
