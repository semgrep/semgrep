
(* fill the s_bf field in the program statements *)
val annotate_program: AST_generic.program -> unit

(* used to compute the bloom of a pattern, skipping Ellispis
 * and metavariables.
*)
val bloom_of_stmt: AST_generic.stmt -> Bloom_filter.t

val bloom_of_expr: AST_generic.expr -> Bloom_filter.t

(* used internally *)
val extract_strings: AST_generic.any -> string list
