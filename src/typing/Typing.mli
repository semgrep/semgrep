(* Returns possibly the inferred type of the expression, as well as an ident
 * option that can then be used to query LSP to get the type of the ident. *)
val type_of_expr :
  Lang.t ->
  AST_generic.expr ->
  AST_generic.name Type.t * AST_generic.ident option

val type_of_ast_generic_type :
  Lang.t -> AST_generic.type_ -> AST_generic.name Type.t

val resolved_type_of_id_info :
  Lang.t -> AST_generic.id_info -> AST_generic.name Type.t

val check_program : Lang.t -> AST_generic.program -> unit

(* Guesses the type of a dotaccess using language-specific heuristics.
 *
 * Exposed for use in Pro Engine. *)
val guess_type_of_dotaccess :
  Lang.t -> (string * 'a Type.type_argument list) option -> string -> 'a Type.t

val pro_hook_type_of_expr :
  (Lang.t -> AST_generic.expr -> AST_generic.name Type.t option) option ref
