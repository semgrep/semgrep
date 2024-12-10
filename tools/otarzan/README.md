# otarzan

Derive OCaml source code for iterators from OCaml type definitions.

This is a "port" of ocamltarzan[1] to use tree-sitter-ocaml and ast_ml.ml
instead of camlp4 to generate OCaml boilerplate code.

See the README[2] of ocamltarzan for some old motivations for this
kind of metaprogramming technique. Nowaday, you should use
ppx deriving[3] or ppx visitors[4] to generate automatically boilerplate
code, but there is also a need and place for tools that generate
boilerplate that a human can start from and would like to further edit
(e.g., for AST_to_IL.ml in semgrep, for AST_to_SAST.ml in semgrep-pro).
Otarzan is such a tool.

See https://github.com/semgrep/semgrep/issues/6440 for more
context on this work.

alternatives:

- use ocamltarzan? but ocamltarzan is a bit difficult to install and
  setup (it requires a specific version of OCaml and camlp4). Moreover,
  it requires some special comments in the code next to the
  type definitions '(_ tarzan _)' (it was written before ppx attributes
  existed) so this makes the whole process quite hacky.
- port ocamltarzan to use ppx? ppx is great for generating automatically
  some boilerplate code, but this generated code is not easily
  accessible and it's not meant to be further edited (it could not
  be used for example to help writing AST_to_IL.ml).
- use ATD? This would require to transform our OCaml type definitions
  to ATD.
- use compiler-libs? the OCaml AST in compiler-libs is really complex,
  and it changes frequently, so using ast_ml.ml seems simpler.

Thanks to our work on semgrep, we actually have now easy access to an
OCaml parser and a pretty simple AST, so we can just simply write code
that traverses the OCaml AST and output code from it.

TODO:

- a generate_boilerplate_generic_vs_generic

TODO?

- We could define an intermediate AST just for the type definitions,
  with just the constructs we handle (e.g., tuples, list, variants).
  At the same time we might want to do more complex metaprogramming
  so maybe simpler to give access to the full AST.
- we could have based the generator on traversing the generic AST instead
  of ast_ml.ml, which would open the way for metaprogramming not only
  for generating OCaml boilerplate code but also for the other languages.
  We could even generate code in a typed way by not generating strings
  but generating code as AST_generic elements (a bit tedious though).
  Then we could rely on a general pretty-printer to output the code.

TODO LATER:

- at some point we may want to release otarzan as a separate tool. This
  would require to make OPAM packages for tree-sitter-ocaml and
  Parse_ocaml_tree_sitter.ml

References:

- [1] https://github.com/aryx/ocamltarzan
- [2] https://github.com/aryx/ocamltarzan/blob/master/readme.txt
- [3] https://github.com/ocaml-ppx/ppx_deriving
- [4] https://gitlab.inria.fr/fpottier/visitors
