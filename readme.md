# Parser

This module turns Jai source text into a syntax tree and can run a lightweight semantic analysis pass over that tree.

How it works: 

1. The lexer reads the raw source string and produces a flat stream of tokens. Tokens keep their original text and source span, so later errors can point back to a line and column in the input file.

2. The parser consumes that token stream and builds an AST. The parser is hand-written recursive descent: top-level declarations dispatch to specialized parsers for functions, structs, and constant declarations; statements dispatch to declaration, assignment, and return parsers; expressions use a precedence climbing parser so operators bind in the expected order. Postfix expression forms such as calls, field access, indexing, and aggregate literals are parsed after primary expressions.

3. The semantic pass walks the AST and builds declaration tables for structs and functions. It also resolves local variables and expression types when the current subset has enough information. Expression types are stored in a side table keyed by AST node pointer, so the AST remains purely syntactic.

The procedures that you need to care about are:

- `lex(input)` for tokenization only.
- `parse(input)` for lexing and parsing in one call.
- `parse_tokens(tokens)` for parsing an existing token stream.
- `analyze(root)` for semantic analysis of an AST root.
- `analyze_source(input)` for lexing, parsing, and semantic analysis in one call.


## Remaining Logic

The parser is still not a complete Jai parser, but it currently parses every
`.jai` file under `C:\Users\ccn\projects\jai\modules` in the local toolchain
snapshot: 547 passed, 0 failed. Many areas are still shallow or syntactic-only:

- imports, `#load`, and compiler directives are mostly skipped or represented
  coarsely rather than modeled as rich AST nodes
- several constructs are parsed conservatively for documentation extraction,
  but are not semantically analyzed in depth
- semantic analysis remains partial: no complete name resolution, overload
  resolution, compile-time execution model, or type checking
