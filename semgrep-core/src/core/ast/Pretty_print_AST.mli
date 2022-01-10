(* old: This module used to be part of semgrep_synthesizing under the name of
 * Pretty_print_generic. We moved it to semgrep_core so that it could be used
 * by -dfg_svalue (Test_analyze_generic). *)

val expr_to_string : Lang.t -> AST_generic.expr -> string

val svalue_to_string : Lang.t -> AST_generic.svalue -> string

val stmt_to_string : Lang.t -> AST_generic.stmt -> string

val arguments_to_string : Lang.t -> AST_generic.argument list -> string
