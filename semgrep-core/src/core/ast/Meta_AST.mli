(*s: pfff/lang_GENERIC/parsing/Meta_AST.mli *)

(* Note that you probably should use AST_generic.show_any, which is
 * auto-generated rather than this file to display debug information.
 * We keep Meta_AST.ml just because we use it to generate the JSON
 * from the OCaml.v type of the generic AST in semgrep (and use that
 * JSON in the semgrep Web UI).
 *)

(*s: signature [[Meta_AST.vof_any]] *)
val vof_any : AST_generic.any -> OCaml.v

(*e: signature [[Meta_AST.vof_any]] *)

(* internals used by other dumpers, e.g., Meta_IL.ml *)
(*s: signature [[Meta_AST.vof_literal]] *)
val vof_literal : AST_generic.literal -> OCaml.v

(*e: signature [[Meta_AST.vof_literal]] *)
(*s: signature [[Meta_AST.vof_type_]] *)
val vof_type_ : AST_generic.type_ -> OCaml.v

(*e: signature [[Meta_AST.vof_type_]] *)
(*s: signature [[Meta_AST.vof_arithmetic_operator]] *)
val vof_arithmetic_operator : AST_generic.operator -> OCaml.v

(*e: signature [[Meta_AST.vof_arithmetic_operator]] *)
(*s: signature [[Meta_AST.vof_function_definition]] *)
val vof_function_definition : AST_generic.function_definition -> OCaml.v

(*e: signature [[Meta_AST.vof_function_definition]] *)
(*s: signature [[Meta_AST.vof_class_definition]] *)
val vof_class_definition : AST_generic.class_definition -> OCaml.v

(*e: signature [[Meta_AST.vof_class_definition]] *)
(*s: signature [[Meta_AST.vof_definition]] *)
val vof_definition : AST_generic.definition -> OCaml.v

(*e: signature [[Meta_AST.vof_definition]] *)
(*s: signature [[Meta_AST.vof_directive]] *)
val vof_directive : AST_generic.directive -> OCaml.v

(*e: signature [[Meta_AST.vof_directive]] *)
(*s: signature [[Meta_AST.vof_expr]] *)
val vof_expr : AST_generic.expr -> OCaml.v

(*e: signature [[Meta_AST.vof_expr]] *)
(*s: signature [[Meta_AST.vof_stmt]] *)
val vof_stmt : AST_generic.stmt -> OCaml.v

(*e: signature [[Meta_AST.vof_stmt]] *)

(* reused in other dumpers *)
val vof_incr_decr : AST_generic.incr_decr -> OCaml.v

val vof_inc_dec : AST_generic.incr_decr * AST_generic.prefix_postfix -> OCaml.v

val vof_prepost : AST_generic.prefix_postfix -> OCaml.v

(*e: pfff/lang_GENERIC/parsing/Meta_AST.mli *)
