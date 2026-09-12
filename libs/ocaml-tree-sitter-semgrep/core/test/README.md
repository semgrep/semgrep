This directory contains tests comprising various tree sitter grammars and
corpuses. We expect to be able to
- generate a parser using tree-sitter from the grammar.js file
- parse the grammar.json generated as part of this with ocaml-tree-sitter
- generate ocaml bindings to use the generated parser with ocaml-tree-sitter
- parse the examples in the provided corpus via an OCaml binary consuming these
  bindings

## Adding a new test

To create a new test, say for a "language" called `foo`, make a directory
`foo/`. Then copy the dune template, replacing `%%DUNELANGNAME%%`. You can use
`sed` for this:
```
sed s/%%DUNELANGNAME%%/foo/g dune.template > foo/dune
```
Note that the template doesn't use an external scanner. If your language uses
one, you may need to modify the rule to build the parser. See externals/dune
for an example.
