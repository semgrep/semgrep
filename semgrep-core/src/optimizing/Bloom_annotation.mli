(* fill the s_bf field in the program statements *)
val annotate_program : AST_generic.program -> unit

(* used to compute the bloom of a pattern, skipping Ellispis
 * and metavariables.
 *)
val set_of_pattern_strings : ?lang:Lang.t -> AST_generic.any -> string Set_.t

(* used internally *)
(* val extract_strings: AST_generic.any -> string list *)
