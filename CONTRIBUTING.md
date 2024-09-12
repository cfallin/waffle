# Contributing to waffle

## Code of Conduct

waffle is a [Bytecode Alliance] project. It follows the Bytecode Alliance's [Code
of Conduct] and [Organizational Code of Conduct].

[Bytecode Alliance]: https://bytecodealliance.org/
[Code of Conduct]: CODE_OF_CONDUCT.md
[Organizational Code of Conduct]: ORG_CODE_OF_CONDUCT.md

## Building

```
$ cargo build
```

## Testing

```
$ cargo test
```

## Fuzzing

```
$ cargo fuzz run roundtrip
$ cargo fuzz run differential
$ cargo fuzz run irreducible
```
