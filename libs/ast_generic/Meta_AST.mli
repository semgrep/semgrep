(* Deprecated: you should use AST_generic.show_any, which is
 * auto-generated, rather than this file to debug ASTs.
 *
 * We keep Meta_AST.ml mainly because we use it to generate the JSON
 * from the OCaml.v type of the generic AST in Semgrep.
 * TODO: we don't display ASTs anymore in the Semgrep playground,
 * and semgrep --dump-ast is deprecated, so we should remove this file.
 * Morever in case we need JSON from ASTs we can use ast_generic_v1.atd
 * instead.
 *)
val vof_any : AST_generic.any -> OCaml.v

(* used by Trace_matching.ml which leverage OCaml.string_of_v ~max_depth *)
val vof_literal : AST_generic.literal -> OCaml.v
val vof_expr : AST_generic.expr -> OCaml.v
val vof_stmt : AST_generic.stmt -> OCaml.v
val vof_type_ : AST_generic.type_ -> OCaml.v
val vof_arithmetic_operator : AST_generic.operator -> OCaml.v
val vof_function_definition : AST_generic.function_definition -> OCaml.v
val vof_class_definition : AST_generic.class_definition -> OCaml.v
val vof_definition : AST_generic.definition -> OCaml.v
val vof_directive : AST_generic.directive -> OCaml.v
val vof_argument : AST_generic.argument -> OCaml.v
val vof_arguments : AST_generic.arguments -> OCaml.v
val vof_raw_tree : AST_generic.raw_tree -> OCaml.v

(* -full_token_info *)
val cmdline_flags_precision : unit -> Arg_.flag_spec list
