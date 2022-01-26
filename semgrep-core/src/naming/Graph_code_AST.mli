(* Build the codegraph for any language thanks to the generic AST.
 *
 * alt: we could compute the list of targets in build() itself, but
 * better to separate concerns I think. Targeting is actually tricky.
 * See Find_generic.ml
 *
 * precondition: the AST_generic.program must have been annotated
 * by Naming_AST.ml, so use Parse_generic.parse_and_resolve_name to
 * generate those ASTs.
 *)
val build :
  root:Common.dirname ->
  Lang.t ->
  (Common.filename * AST_generic.program) list ->
  Graph_code.t * Graph_code.statistics
