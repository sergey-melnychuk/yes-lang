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

### REPL

```shell
$ cargo run
[...]
REPL: enter :q to quit.
> let hi = fn(name) { "hello " + name };
[AST]
<function>
> hi("world")
[AST]
hello world
> :q
$
```

### Run

```rust
// examples/fib.yes

fn fib1(n, a, b) {
    if n == 0 { a+b }
    else { fib1(n-1, a+b, a) }
}

fn fib(n) {
    if n <= 0 { 0 }
    else {
        if n <= 2 { 1 }
        else {
            fib1(n-2, 1, 0)
        }
    }
}

fib(42) == 267914296 && fib(84) == 160500643816367088

```

```shell
$ cargo run examples/fib.yes
[...]
true
```
