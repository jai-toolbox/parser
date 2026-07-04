# Parser

This module turns Jai source text into a syntax tree.

It does the job in two stages. First, the lexer reads the raw source string and
produces a flat stream of tokens. Tokens keep their original text and source
span, so later errors can point back to a line and column in the input file.

Second, the parser consumes that token stream and builds an AST. The parser is
hand-written recursive descent: top-level declarations dispatch to specialized
parsers for functions, structs, and constant declarations; statements dispatch
to declaration, assignment, and return parsers; expressions use a precedence
climbing parser so operators bind in the expected order. Postfix expression
forms such as calls, field access, indexing, and aggregate literals are parsed
after primary expressions.

The public entry points are:

- `lex(input)` for tokenization only.
- `parse(input)` for lexing and parsing in one call.
- `parse_tokens(tokens)` for parsing an existing token stream.

Each entry point returns a result struct with `success`, payload data, and an
error field. The parser stops at the first error for now.

## Coverage Gaps

The parser is not a complete Jai parser yet. Important missing areas include:

- imports, load directives, and compiler directives like `#run`
- control flow such as `if`, `for`, `while`, `case`, and `ifx`
- procedure bodies containing expression statements that are not assignments
- declarations with attributes, notes, or calling conventions
- procedure overload sets and polymorphic procedure declarations beyond simple
  `$T/interface Constraint` parameter types
- full type syntax such as arrays, slices, procedure types, and type literals
- operators beyond the small arithmetic/assignment set currently tokenized
- comments other than `//` line comments
- semantic analysis, name resolution, and type checking
