# Requirements grammar updates

Grammar-stage guidance for this wrapper. Paths below are relative to the
ocaml-tree-sitter-semgrep tree unless stated otherwise.

Requirements has no Semgrep rule-pattern language suite. Prefer focused cases
in this wrapper's `test/corpus/` for changed option / URL forms (short `-i`,
long `--index-url` / `--extra-index-url`, quoting, line continuation, and
interpolation).

Grammar repair and `test-grammar` stop at CST, scanner, and corpus work.
Missing Semgrep `Lang` parsing/pattern suites or helper `--language` filters
are expected here and are not grammar-stage blockers. Converter validation is
a later pipeline stage.
