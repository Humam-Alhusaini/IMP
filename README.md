# IMP

A minimal interpreter for [Software Foundation's IMP language](https://softwarefoundations.cis.upenn.edu/plf-current/Imp.html),
written in OCaml.

## Features

- Arithmetic and boolean expressions
- While loops, `if`/`elif`/`else` conditionals, `def` variable definitions, and `print`
- An interactive REPL and a file runner
- Debugging output for lexed tokens and the parsed AST

## Build

Requires [dune](https://dune.build) and OCaml.

```sh
dune build
```

## Run

Interactive REPL:

```sh
dune exec Lang
```

Run a `.mdc` program file:

```sh
dune exec Lang -- file.mdc
```

Print lexed tokens (`-tokens`), the parsed AST (`-ast`), or both:

```sh
dune exec Lang -- -tokens -ast file.mdc
```

## Tests

```sh
dune runtest
```

## Project layout

- `lib/lexer.ml` — tokenizes source into tokens
- `lib/parser.ml` — builds an abstract syntax tree from tokens
- `lib/interp.ml` — interprets the AST against a context
- `lib/printer.ml` — pretty-printing for tokens, AST, and the context
- `lib/ctx.ml` — variable context (a map from names to values)
- `bin/main.ml` — REPL / file entry point
- `test/` — example program (`test.mdc`) run on `dune runtest`
