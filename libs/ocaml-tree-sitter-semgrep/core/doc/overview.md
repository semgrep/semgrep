ocaml-tree-sitter overview
==

This document is targeted at technical contributors.

[tree-sitter](https://tree-sitter.github.io/tree-sitter/) generates C
parsers from grammars specified in json via JavaScript, with optional
help from custom C code.

The tree-sitter project also comes with grammars for a variety of
programming languages. Our goal is to take advantage of these
grammars and parsers from OCaml, rather than writing and
maintaining our own.

Definitions
--

CST: Concrete Syntax Tree. Also known as Parse Tree, it is the
     result of parsing a source file without trying to apply
     simplifications or normalizations. For example, an OCaml list of
     two elements could be written `[1; 2]` or `1 :: 2 :: []`. Both
     have the same meaning but result in different CSTs. Tree-sitter
     parsers produce CSTs. CSTs are useful as an initial step toward
     creating an AST for program interpretation. They're also useful
     for syntax highlighting in text editors.

AST: Abstract Syntax Tree. They're a simplified version of a CST.
     In the example of OCaml lists above, both lists `[1; 2]` and
     `1 :: 2 :: []` would have the same AST.

Flow
--

With the example of Ruby, here's what tree-sitter and a
tree-sitter-ruby produce:

```
(1)               tree-sitter + grammar.js → grammar.json
(2) tree-sitter + grammar.json + scanner.c → tree-sitter Ruby parser
(3)   tree-sitter Ruby parser + example.rb → example.json
```

Steps (1) and (2) are done once and for all for the target programming
language, Ruby in the example. The input files `grammar.js` and `scanner.c`
constitute all the input tree-sitter needs to generate a parser for
the target language. Step (3) consists in parsing a file in
the target language (`example.rb`), resulting in a json CST
(`example.json`).

At this stage, the json output (`example.json`) must be converted into
a typed OCaml CST for further analysis. The type of the CST is derived
from the grammar in json format (`grammar.json`) by ocaml-tree-sitter. The
steps are:

```
(4)                ocaml-tree-sitter + grammar.json → OCaml source
(5) tree-sitter Ruby parser C source + OCaml source → C/OCaml Ruby parser
(6)                C/OCaml Ruby parser + example.rb → OCaml Ruby-CST object
```

Like steps (1) and (2), steps (4) and (5) consist in code generation
and compilation, which is done once and for all for the target
language.

Most of the work in the ocaml-tree-sitter project consists in translating a
grammar specification (`grammar.json`) into:

* a convenient, OCaml-friendly, stable CST type
* a parser that interprets tree-sitter's parser output into such CST

Code organization
--

* [`/scripts`](../scripts): various shell scripts used for building,
  testing, etc.
* [`/src`](../src): OCaml source code
* [`/src/gen`](../src/gen): source code in charge of code generation
* [`/src/run`](../src/run): runtime library for generated parsers
* [`/tests`](../src/tests): small tests, each involving generating a
  parser and running it on one or more inputs
* [`/lang`](../lang): parsers for real programming languages derived from
  tree-sitter subprojects e.g.
  [tree-sitter-ruby](https://github.com/tree-sitter/tree-sitter-ruby)

Deep dive
--

* [Generating good CST type definitions](cst.md)
* [Interpreting the output of tree-sitter](parsing.md)
