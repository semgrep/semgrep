(* Note that you probably should use AST_generic.show_any, which is
 * auto-generated rather than this file to display debug information.
 * We keep Meta_AST.ml just because we use it to generate the JSON
 * from the OCaml.v type of the generic AST in semgrep (and use that
 * JSON in the semgrep Web UI).
 *)

val vof_any : AST_generic.any -> OCaml.v

(* internals used by other dumpers, e.g., Meta_IL.ml *)
val vof_literal : AST_generic.literal -> OCaml.v
val vof_type_ : AST_generic.type_ -> OCaml.v
val vof_arithmetic_operator : AST_generic.operator -> OCaml.v
val vof_function_definition : AST_generic.function_definition -> OCaml.v
val vof_class_definition : AST_generic.class_definition -> OCaml.v
val vof_definition : AST_generic.definition -> OCaml.v
val vof_directive : AST_generic.directive -> OCaml.v
val vof_expr : AST_generic.expr -> OCaml.v
val vof_stmt : AST_generic.stmt -> OCaml.v

(* reused in other dumpers *)
val vof_incr_decr : AST_generic.incr_decr -> OCaml.v
val vof_inc_dec : AST_generic.incr_decr * AST_generic.prefix_postfix -> OCaml.v
val vof_prepost : AST_generic.prefix_postfix -> OCaml.v
