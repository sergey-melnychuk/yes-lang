yes-lang
========

### Yet anothEr Scripting Language

Simple interpreter in Rust:
- "manual" tokenization
- single-/multi-line comments
- prefix & infix operators
- Pratt parser impl
- tree-walking interpreter
- immutable scope-based bindings
- functions as a first class citizens
- lexer, parser, interpreter & REPL in <2k LoC

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
- collections
- performance
- type-safety
- production-readiness
- usability in general

#### Build

`cargo build --release`

`cargo test --release`

### Run

Example: [fib.yes](examples/fib.yes).

```shell
$ cargo run --release examples/fib.yes
[...]
true
```

### REPL

```shell
$ cargo run
[...]
REPL: enter :q to quit.
> let f = (fn (x) { fn(y) { x + y } });
<function>
> let hi = f("hello ");
<function>
> hi("world")
hello world
> f(40)(2)
42
> :load examples/fib.yes
true
> fib(10)
55
> :q
$
```
