type token =
  | T_UAMPER of Tok.t
  | T_AMPER of Tok.t
  | T_TILDE of Tok.t
  | T_BANG of Tok.t
  | T_VBAR of Tok.t
  | T_CARROT of Tok.t
  | T_RARROW of Tok.t
  | T_PERCENT of Tok.t
  | T_SLASH of Tok.t
  | T_USTAR of Tok.t
  | T_STAR of Tok.t
  | T_OP_ASGN of (string * Tok.t)
  | T_RSHFT of Tok.t
  | T_LSHFT of Tok.t
  | RDots of Tok.t
  | LDots of Tok.t
  | T_DOT3 of Tok.t
  | T_DOT2 of Tok.t
  | T_NMATCH of Tok.t
  | T_MATCH of Tok.t
  | T_OROP of Tok.t
  | T_ANDOP of Tok.t
  | T_GT of Tok.t
  | T_LT of Tok.t
  | T_LEQ of Tok.t
  | T_GEQ of Tok.t
  | T_NEQ of Tok.t
  | T_EQQ of Tok.t
  | T_EQ of Tok.t
  | T_ASSIGN of Tok.t
  | T_CMP of Tok.t
  | T_POW of Tok.t
  | T_UMINUS of Tok.t
  | T_MINUS of Tok.t
  | T_UPLUS of Tok.t
  | T_PLUS of Tok.t
  | T_USCOPE of Tok.t
  | T_SCOPE of Tok.t
  | T_SEMICOLON of Tok.t
  | T_COLON of Tok.t
  | T_QUESTION of Tok.t
  | T_RBRACE of Tok.t
  | T_LBRACE_ARG of Tok.t
  | T_LBRACE of Tok.t
  | T_RBRACK of Tok.t
  | T_LBRACK of Tok.t
  | T_LBRACK_ARG of Tok.t
  | T_RPAREN of Tok.t
  | T_LPAREN_ARG of Tok.t
  | T_LPAREN of Tok.t
  | T_ASSOC of Tok.t
  | T_COMMA of Tok.t
  | T_DOT of Tok.t
  | K_FALSE of Tok.t
  | K_TRUE of Tok.t
  | K_SUPER of Tok.t
  | K_SELF of Tok.t
  | K_YIELD of Tok.t
  | K_NIL of Tok.t
  | K_lEND of Tok.t
  | K_lBEGIN of Tok.t
  | K_NOT of Tok.t
  | K_OR of Tok.t
  | K_AND of Tok.t
  | K_RETURN of Tok.t
  | K_DO of Tok.t
  | K_IN of Tok.t
  | K_FOR of Tok.t
  | K_UNTIL of Tok.t
  | K_WHILE of Tok.t
  | K_WHEN of Tok.t
  | K_CASE of Tok.t
  | K_ELSE of Tok.t
  | K_ELSIF of Tok.t
  | K_THEN of Tok.t
  | K_UNLESS of Tok.t
  | K_IF of Tok.t
  | K_ENSURE of Tok.t
  | K_RESCUE of Tok.t
  | K_UNDEF of Tok.t
  | K_ALIAS of Tok.t
  | K_END of Tok.t
  | K_BEGIN of Tok.t
  | K_DEF of Tok.t
  | K_MODULE of Tok.t
  | K_CLASS of Tok.t
  | T_REGEXP_MOD of (string * Tok.t)
  | T_REGEXP_BEG of Tok.t
  | T_USER_BEG of (string * Tok.t)
  | T_TICK_BEG of Tok.t
  | T_INTERP_END of (string * Tok.t)
  | T_INTERP_STR of (string * Tok.t)
  | T_DOUBLE_BEG of Tok.t
  | T_SINGLE_STRING of (string * Tok.t)
  | T_ATOM_BEG of Tok.t
  | T_ATOM of (Tok.t * (string * Tok.t))
  | T_FLOAT of (string * Tok.t)
  | T_NUM of (string * Tok.t)
  | T_CLASS_VAR of (string * Tok.t)
  | T_INST_VAR of (string * Tok.t)
  | T_GLOBAL_VAR of (string * Tok.t)
  | T_LID of (string * Tok.t)
  | T_UID of (string * Tok.t)
  | T_EOL of Tok.t
  | T_UNKNOWN of Tok.t
  | T_EOF of Tok.t
  | T_COMMENT of Tok.t
  | T_SPACE of Tok.t

val pp :
  unit ->
  ( token,
    [> `Lexeme_matched of string
    | `Obj_K_ALIAS of Tok.t
    | `Obj_K_AND of Tok.t
    | `Obj_K_BEGIN of Tok.t
    | `Obj_K_CASE of Tok.t
    | `Obj_K_CLASS of Tok.t
    | `Obj_K_DEF of Tok.t
    | `Obj_K_DO of Tok.t
    | `Obj_K_ELSE of Tok.t
    | `Obj_K_ELSIF of Tok.t
    | `Obj_K_END of Tok.t
    | `Obj_K_ENSURE of Tok.t
    | `Obj_K_FALSE of Tok.t
    | `Obj_K_FOR of Tok.t
    | `Obj_K_IF of Tok.t
    | `Obj_K_IN of Tok.t
    | `Obj_K_MODULE of Tok.t
    | `Obj_K_NIL of Tok.t
    | `Obj_K_NOT of Tok.t
    | `Obj_K_OR of Tok.t
    | `Obj_K_RESCUE of Tok.t
    | `Obj_K_RETURN of Tok.t
    | `Obj_K_SELF of Tok.t
    | `Obj_K_SUPER of Tok.t
    | `Obj_K_THEN of Tok.t
    | `Obj_K_TRUE of Tok.t
    | `Obj_K_UNDEF of Tok.t
    | `Obj_K_UNLESS of Tok.t
    | `Obj_K_UNTIL of Tok.t
    | `Obj_K_WHEN of Tok.t
    | `Obj_K_WHILE of Tok.t
    | `Obj_K_YIELD of Tok.t
    | `Obj_K_lBEGIN of Tok.t
    | `Obj_K_lEND of Tok.t
    | `Obj_LDots of Tok.t
    | `Obj_RDots of Tok.t
    | `Obj_T_AMPER of Tok.t
    | `Obj_T_ANDOP of Tok.t
    | `Obj_T_ASSIGN of Tok.t
    | `Obj_T_ASSOC of Tok.t
    | `Obj_T_ATOM of Tok.t * (string * Tok.t)
    | `Obj_T_ATOM_BEG of Tok.t
    | `Obj_T_BANG of Tok.t
    | `Obj_T_CARROT of Tok.t
    | `Obj_T_CLASS_VAR of string * Tok.t
    | `Obj_T_CMP of Tok.t
    | `Obj_T_COLON of Tok.t
    | `Obj_T_COMMA of Tok.t
    | `Obj_T_COMMENT of Tok.t
    | `Obj_T_DOT of Tok.t
    | `Obj_T_DOT2 of Tok.t
    | `Obj_T_DOT3 of Tok.t
    | `Obj_T_DOUBLE_BEG of Tok.t
    | `Obj_T_EOF of Tok.t
    | `Obj_T_EOL of Tok.t
    | `Obj_T_EQ of Tok.t
    | `Obj_T_EQQ of Tok.t
    | `Obj_T_FLOAT of string * Tok.t
    | `Obj_T_GEQ of Tok.t
    | `Obj_T_GLOBAL_VAR of string * Tok.t
    | `Obj_T_GT of Tok.t
    | `Obj_T_INST_VAR of string * Tok.t
    | `Obj_T_INTERP_END of string * Tok.t
    | `Obj_T_INTERP_STR of string * Tok.t
    | `Obj_T_LBRACE of Tok.t
    | `Obj_T_LBRACE_ARG of Tok.t
    | `Obj_T_LBRACK of Tok.t
    | `Obj_T_LBRACK_ARG of Tok.t
    | `Obj_T_LEQ of Tok.t
    | `Obj_T_LID of string * Tok.t
    | `Obj_T_LPAREN of Tok.t
    | `Obj_T_LPAREN_ARG of Tok.t
    | `Obj_T_LSHFT of Tok.t
    | `Obj_T_LT of Tok.t
    | `Obj_T_MATCH of Tok.t
    | `Obj_T_MINUS of Tok.t
    | `Obj_T_NEQ of Tok.t
    | `Obj_T_NMATCH of Tok.t
    | `Obj_T_NUM of string * Tok.t
    | `Obj_T_OP_ASGN of string * Tok.t
    | `Obj_T_OROP of Tok.t
    | `Obj_T_PERCENT of Tok.t
    | `Obj_T_PLUS of Tok.t
    | `Obj_T_POW of Tok.t
    | `Obj_T_QUESTION of Tok.t
    | `Obj_T_RARROW of Tok.t
    | `Obj_T_RBRACE of Tok.t
    | `Obj_T_RBRACK of Tok.t
    | `Obj_T_REGEXP_BEG of Tok.t
    | `Obj_T_REGEXP_MOD of string * Tok.t
    | `Obj_T_RPAREN of Tok.t
    | `Obj_T_RSHFT of Tok.t
    | `Obj_T_SCOPE of Tok.t
    | `Obj_T_SEMICOLON of Tok.t
    | `Obj_T_SINGLE_STRING of string * Tok.t
    | `Obj_T_SLASH of Tok.t
    | `Obj_T_SPACE of Tok.t
    | `Obj_T_STAR of Tok.t
    | `Obj_T_TICK_BEG of Tok.t
    | `Obj_T_TILDE of Tok.t
    | `Obj_T_UAMPER of Tok.t
    | `Obj_T_UID of string * Tok.t
    | `Obj_T_UMINUS of Tok.t
    | `Obj_T_UNKNOWN of Tok.t
    | `Obj_T_UPLUS of Tok.t
    | `Obj_T_USCOPE of Tok.t
    | `Obj_T_USER_BEG of string * Tok.t
    | `Obj_T_USTAR of Tok.t
    | `Obj_T_VBAR of Tok.t
    | `Obj_any_LPAREN of Ast_ruby.tok
    | `Obj_arg of Ast_ruby.expr
    | `Obj_arg_comma_list_trail of Ast_ruby.arguments
    | `Obj_arg_comma_nonempty_list of Ast_ruby.arguments
    | `Obj_arg_comma_star_list of Ast_ruby.arguments
    | `Obj_argument of Ast_ruby.argument
    | `Obj_array of Ast_ruby.expr
    | `Obj_array_body of Ast_ruby.program
    | `Obj_array_body_rest of Ast_ruby.program
    | `Obj_array_item of Ast_ruby.expr
    | `Obj_assign_op of Ast_ruby.binary_op * Tok.t
    | `Obj_bin_op of Ast_ruby.binary_op * Ast_ruby.tok
    | `Obj_body_exn of Ast_ruby.body_exn
    | `Obj_brace_codeblock of Ast_ruby.expr
    | `Obj_call_args of Ast_ruby.arguments
    | `Obj_case_else of (Ast_ruby.tok * Ast_ruby.stmts) option
    | `Obj_class_definition of Ast_ruby.expr
    | `Obj_class_inheritance of (Ast_ruby.tok * Ast_ruby.expr) option
    | `Obj_code_block of Ast_ruby.expr
    | `Obj_code_block_body of Ast_ruby.formal_param list option * Ast_ruby.stmts
    | `Obj_command of Ast_ruby.expr
    | `Obj_command_codeblock of Ast_ruby.expr
    | `Obj_command_name of Ast_ruby.expr
    | `Obj_constant of Ast_ruby.expr
    | `Obj_cst_or_scoped_id of Ast_ruby.class_or_module_name
    | `Obj_do_codeblock of Ast_ruby.expr
    | `Obj_do_sep of unit
    | `Obj_ensure_clause of (Ast_ruby.tok * Ast_ruby.stmts) option
    | `Obj_eol_or_semi of unit
    | `Obj_eols of unit
    | `Obj_expr of Ast_ruby.expr
    | `Obj_formal_arg of Ast_ruby.formal_param
    | `Obj_formal_arg_list of Ast_ruby.formal_param list
    | `Obj_formal_arg_nonempty_list of Ast_ruby.formal_param list
    | `Obj_func of Ast_ruby.expr
    | `Obj_hash of Ast_ruby.expr
    | `Obj_hash_elem of Ast_ruby.expr
    | `Obj_hash_elem_list of Ast_ruby.expr list
    | `Obj_identifier of Ast_ruby.ident
    | `Obj_if_else_clauses of (Ast_ruby.tok * Ast_ruby.stmts) option
    | `Obj_interp_code of Ast_ruby.interp list
    | `Obj_interp_str of Ast_ruby.interp list
    | `Obj_interp_str_work of Ast_ruby.interp list
    | `Obj_keyword_as_id of Ast_ruby.ident
    | `Obj_lambda of Ast_ruby.expr
    | `Obj_lambda_args of Ast_ruby.formal_param list option
    | `Obj_lambda_body of Ast_ruby.stmts
    | `Obj_lhs of Ast_ruby.expr
    | `Obj_lhs_assign_op of Ast_ruby.expr * Ast_ruby.binary_op * Tok.t
    | `Obj_lhs_prune_binop of Ast_ruby.expr
    | `Obj_lhs_pruned_assign_op of
      Ast_ruby.expr * Ast_ruby.binary_op * Ast_ruby.tok
    | `Obj_main of Ast_ruby.program
    | `Obj_message_identifier of Ast_ruby.method_name
    | `Obj_meth_or_atom of Ast_ruby.method_name
    | `Obj_meth_or_atom_list of Ast_ruby.method_name list
    | `Obj_method_definition of Ast_ruby.expr
    | `Obj_method_formals of Ast_ruby.formal_param list
    | `Obj_method_kind of Ast_ruby.method_kind
    | `Obj_method_name of Ast_ruby.method_name
    | `Obj_mlhs of Ast_ruby.program
    | `Obj_mlhs_assign_op of
      Ast_ruby.program * Ast_ruby.binary_op * Ast_ruby.tok
    | `Obj_mlhs_clean of Ast_ruby.program
    | `Obj_mlhs_item of Ast_ruby.expr
    | `Obj_mlhs_rest of Ast_ruby.expr list
    | `Obj_module_definition of Ast_ruby.expr
    | `Obj_mrhs of Ast_ruby.program
    | `Obj_one_string of Ast_ruby.expr
    | `Obj_pair of Ast_ruby.ident * Ast_ruby.tok * Ast_ruby.expr
    | `Obj_primary of Ast_ruby.expr
    | `Obj_rescue_clause of Ast_ruby.rescue_clause
    | `Obj_rescue_list of Ast_ruby.rescue_clause list
    | `Obj_scope_BEGIN of Ast_ruby.tok
    | `Obj_scope_CLASS of Ast_ruby.tok
    | `Obj_scope_DEF of Ast_ruby.tok
    | `Obj_scope_END of Ast_ruby.tok
    | `Obj_scope_MODULE of Ast_ruby.tok
    | `Obj_seen_id of Ast_ruby.lhs
    | `Obj_sgrep_spatch_pattern of Ast_ruby.any
    | `Obj_some_eols of unit
    | `Obj_star_amper of Ast_ruby.program
    | `Obj_stmt of Ast_ruby.expr
    | `Obj_stmt_list of Ast_ruby.program
    | `Obj_string of Ast_ruby.expr
    | `Obj_then_sep of unit
    | `Obj_topcall of Ast_ruby.expr
    | `Obj_unary_op of Ast_ruby.unary_msg * Ast_ruby.tok
    | `Obj_var_or_scoped_id of Ast_ruby.expr
    | `Obj_variable of Ast_ruby.variable
    | `Obj_variable2 of Ast_ruby.variable
    | `Obj_when_clauses of
      (Ast_ruby.tok * Ast_ruby.pattern list * Ast_ruby.stmts) list ],
    unit,
    unit,
    Lexing.lexbuf )
  Dyp.parser_pilot

val sgrep_spatch_pattern :
  ?global_data:unit ->
  ?local_data:unit ->
  (Lexing.lexbuf -> token) ->
  Lexing.lexbuf ->
  (Ast_ruby.any * string) list

val main :
  ?global_data:unit ->
  ?local_data:unit ->
  (Lexing.lexbuf -> token) ->
  Lexing.lexbuf ->
  (Ast_ruby.program * string) list
