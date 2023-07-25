(* Extraction of the first token of an AST construct, used for its
   location info.
*)

val expression_loc : AST_bash.expression -> Tok_range.t
val test_expression_loc : AST_bash.test_expression -> Tok_range.t
val assignment_loc : AST_bash.assignment -> Tok_range.t
val pipeline_loc : AST_bash.pipeline -> Tok_range.t
val blist_loc : AST_bash.blist -> Tok_range.t
val redirect_loc : AST_bash.redirect -> Tok_range.t
val command_loc : AST_bash.command -> Tok_range.t
val cmd_redir_loc : AST_bash.cmd_redir -> Tok_range.t
val variable_name_loc : AST_bash.variable_name -> Tok_range.t
val eq_op_loc : AST_bash.eq_op -> Tok_range.t
val right_eq_operand_loc : AST_bash.right_eq_operand -> Tok_range.t
val string_fragment_loc : AST_bash.string_fragment -> Tok_range.t
val wrap_loc : 'a * Tok.t -> Tok_range.t
val bracket_loc : Tok.t * 'a * Tok.t -> Tok_range.t
