(*
type hooks = {
  on_def: (Graph_code.node * AST_generic.definition) -> unit;
}
*)

val default_hooks : Graph_code_AST_env.hooks

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
  hooks:Graph_code_AST_env.hooks ->
  Lang.t ->
  (Common.filename * AST_generic.program) list ->
  Graph_code.t * Graph_code.statistics
