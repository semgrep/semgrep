
type token =
    T_UAMPER of Parse_info.t
  | T_AMPER of Parse_info.t
  | T_TILDE of Parse_info.t
  | T_BANG of Parse_info.t
  | T_VBAR of Parse_info.t
  | T_CARROT of Parse_info.t
  | T_RARROW of Parse_info.t
  | T_PERCENT of Parse_info.t
  | T_SLASH of Parse_info.t
  | T_USTAR of Parse_info.t
  | T_STAR of Parse_info.t
  | T_OP_ASGN of (string * Parse_info.t)
  | T_RSHFT of Parse_info.t
  | T_LSHFT of Parse_info.t
  | RDots of Parse_info.t
  | LDots of Parse_info.t
  | T_DOT3 of Parse_info.t
  | T_DOT2 of Parse_info.t
  | T_NMATCH of Parse_info.t
  | T_MATCH of Parse_info.t
  | T_OROP of Parse_info.t
  | T_ANDOP of Parse_info.t
  | T_GT of Parse_info.t
  | T_LT of Parse_info.t
  | T_LEQ of Parse_info.t
  | T_GEQ of Parse_info.t
  | T_NEQ of Parse_info.t
  | T_EQQ of Parse_info.t
  | T_EQ of Parse_info.t
  | T_ASSIGN of Parse_info.t
  | T_CMP of Parse_info.t
  | T_POW of Parse_info.t
  | T_UMINUS of Parse_info.t
  | T_MINUS of Parse_info.t
  | T_UPLUS of Parse_info.t
  | T_PLUS of Parse_info.t
  | T_USCOPE of Parse_info.t
  | T_SCOPE of Parse_info.t
  | T_SEMICOLON of Parse_info.t
  | T_COLON of Parse_info.t
  | T_QUESTION of Parse_info.t
  | T_RBRACE of Parse_info.t
  | T_LBRACE_ARG of Parse_info.t
  | T_LBRACE of Parse_info.t
  | T_RBRACK of Parse_info.t
  | T_LBRACK of Parse_info.t
  | T_LBRACK_ARG of Parse_info.t
  | T_RPAREN of Parse_info.t
  | T_LPAREN_ARG of Parse_info.t
  | T_LPAREN of Parse_info.t
  | T_ASSOC of Parse_info.t
  | T_COMMA of Parse_info.t
  | T_DOT of Parse_info.t
  | K_FALSE of Parse_info.t
  | K_TRUE of Parse_info.t
  | K_SUPER of Parse_info.t
  | K_SELF of Parse_info.t
  | K_YIELD of Parse_info.t
  | K_NIL of Parse_info.t
  | K_lEND of Parse_info.t
  | K_lBEGIN of Parse_info.t
  | K_NOT of Parse_info.t
  | K_OR of Parse_info.t
  | K_AND of Parse_info.t
  | K_RETURN of Parse_info.t
  | K_DO of Parse_info.t
  | K_IN of Parse_info.t
  | K_FOR of Parse_info.t
  | K_UNTIL of Parse_info.t
  | K_WHILE of Parse_info.t
  | K_WHEN of Parse_info.t
  | K_CASE of Parse_info.t
  | K_ELSE of Parse_info.t
  | K_ELSIF of Parse_info.t
  | K_THEN of Parse_info.t
  | K_UNLESS of Parse_info.t
  | K_IF of Parse_info.t
  | K_ENSURE of Parse_info.t
  | K_RESCUE of Parse_info.t
  | K_UNDEF of Parse_info.t
  | K_ALIAS of Parse_info.t
  | K_END of Parse_info.t
  | K_BEGIN of Parse_info.t
  | K_DEF of Parse_info.t
  | K_MODULE of Parse_info.t
  | K_CLASS of Parse_info.t
  | T_REGEXP_MOD of (string * Parse_info.t)
  | T_REGEXP_BEG of Parse_info.t
  | T_USER_BEG of (string * Parse_info.t)
  | T_TICK_BEG of Parse_info.t
  | T_INTERP_END of (string * Parse_info.t)
  | T_INTERP_STR of (string * Parse_info.t)
  | T_DOUBLE_BEG of Parse_info.t
  | T_SINGLE_STRING of (string * Parse_info.t)
  | T_ATOM_BEG of Parse_info.t
  | T_ATOM of (Parse_info.t * (string * Parse_info.t))
  | T_FLOAT of (string * Parse_info.t)
  | T_NUM of (string * Parse_info.t)
  | T_CLASS_VAR of (string * Parse_info.t)
  | T_INST_VAR of (string * Parse_info.t)
  | T_GLOBAL_VAR of (string * Parse_info.t)
  | T_LID of (string * Parse_info.t)
  | T_UID of (string * Parse_info.t)
  | T_EOL of Parse_info.t
  | T_UNKNOWN of Parse_info.t
  | T_EOF of Parse_info.t
  | T_COMMENT of Parse_info.t
  | T_SPACE of Parse_info.t

val pp :
  unit ->
  (token,
   [> `Lexeme_matched of string
   | `Obj_K_ALIAS of Parse_info.t
   | `Obj_K_AND of Parse_info.t
   | `Obj_K_BEGIN of Parse_info.t
   | `Obj_K_CASE of Parse_info.t
   | `Obj_K_CLASS of Parse_info.t
   | `Obj_K_DEF of Parse_info.t
   | `Obj_K_DO of Parse_info.t
   | `Obj_K_ELSE of Parse_info.t
   | `Obj_K_ELSIF of Parse_info.t
   | `Obj_K_END of Parse_info.t
   | `Obj_K_ENSURE of Parse_info.t
   | `Obj_K_FALSE of Parse_info.t
   | `Obj_K_FOR of Parse_info.t
   | `Obj_K_IF of Parse_info.t
   | `Obj_K_IN of Parse_info.t
   | `Obj_K_MODULE of Parse_info.t
   | `Obj_K_NIL of Parse_info.t
   | `Obj_K_NOT of Parse_info.t
   | `Obj_K_OR of Parse_info.t
   | `Obj_K_RESCUE of Parse_info.t
   | `Obj_K_RETURN of Parse_info.t
   | `Obj_K_SELF of Parse_info.t
   | `Obj_K_SUPER of Parse_info.t
   | `Obj_K_THEN of Parse_info.t
   | `Obj_K_TRUE of Parse_info.t
   | `Obj_K_UNDEF of Parse_info.t
   | `Obj_K_UNLESS of Parse_info.t
   | `Obj_K_UNTIL of Parse_info.t
   | `Obj_K_WHEN of Parse_info.t
   | `Obj_K_WHILE of Parse_info.t
   | `Obj_K_YIELD of Parse_info.t
   | `Obj_K_lBEGIN of Parse_info.t
   | `Obj_K_lEND of Parse_info.t
   | `Obj_LDots of Parse_info.t
   | `Obj_RDots of Parse_info.t
   | `Obj_T_AMPER of Parse_info.t
   | `Obj_T_ANDOP of Parse_info.t
   | `Obj_T_ASSIGN of Parse_info.t
   | `Obj_T_ASSOC of Parse_info.t
   | `Obj_T_ATOM of Parse_info.t * (string * Parse_info.t)
   | `Obj_T_ATOM_BEG of Parse_info.t
   | `Obj_T_BANG of Parse_info.t
   | `Obj_T_CARROT of Parse_info.t
   | `Obj_T_CLASS_VAR of string * Parse_info.t
   | `Obj_T_CMP of Parse_info.t
   | `Obj_T_COLON of Parse_info.t
   | `Obj_T_COMMA of Parse_info.t
   | `Obj_T_COMMENT of Parse_info.t
   | `Obj_T_DOT of Parse_info.t
   | `Obj_T_DOT2 of Parse_info.t
   | `Obj_T_DOT3 of Parse_info.t
   | `Obj_T_DOUBLE_BEG of Parse_info.t
   | `Obj_T_EOF of Parse_info.t
   | `Obj_T_EOL of Parse_info.t
   | `Obj_T_EQ of Parse_info.t
   | `Obj_T_EQQ of Parse_info.t
   | `Obj_T_FLOAT of string * Parse_info.t
   | `Obj_T_GEQ of Parse_info.t
   | `Obj_T_GLOBAL_VAR of string * Parse_info.t
   | `Obj_T_GT of Parse_info.t
   | `Obj_T_INST_VAR of string * Parse_info.t
   | `Obj_T_INTERP_END of string * Parse_info.t
   | `Obj_T_INTERP_STR of string * Parse_info.t
   | `Obj_T_LBRACE of Parse_info.t
   | `Obj_T_LBRACE_ARG of Parse_info.t
   | `Obj_T_LBRACK of Parse_info.t
   | `Obj_T_LBRACK_ARG of Parse_info.t
   | `Obj_T_LEQ of Parse_info.t
   | `Obj_T_LID of string * Parse_info.t
   | `Obj_T_LPAREN of Parse_info.t
   | `Obj_T_LPAREN_ARG of Parse_info.t
   | `Obj_T_LSHFT of Parse_info.t
   | `Obj_T_LT of Parse_info.t
   | `Obj_T_MATCH of Parse_info.t
   | `Obj_T_MINUS of Parse_info.t
   | `Obj_T_NEQ of Parse_info.t
   | `Obj_T_NMATCH of Parse_info.t
   | `Obj_T_NUM of string * Parse_info.t
   | `Obj_T_OP_ASGN of string * Parse_info.t
   | `Obj_T_OROP of Parse_info.t
   | `Obj_T_PERCENT of Parse_info.t
   | `Obj_T_PLUS of Parse_info.t
   | `Obj_T_POW of Parse_info.t
   | `Obj_T_QUESTION of Parse_info.t
   | `Obj_T_RARROW of Parse_info.t
   | `Obj_T_RBRACE of Parse_info.t
   | `Obj_T_RBRACK of Parse_info.t
   | `Obj_T_REGEXP_BEG of Parse_info.t
   | `Obj_T_REGEXP_MOD of string * Parse_info.t
   | `Obj_T_RPAREN of Parse_info.t
   | `Obj_T_RSHFT of Parse_info.t
   | `Obj_T_SCOPE of Parse_info.t
   | `Obj_T_SEMICOLON of Parse_info.t
   | `Obj_T_SINGLE_STRING of string * Parse_info.t
   | `Obj_T_SLASH of Parse_info.t
   | `Obj_T_SPACE of Parse_info.t
   | `Obj_T_STAR of Parse_info.t
   | `Obj_T_TICK_BEG of Parse_info.t
   | `Obj_T_TILDE of Parse_info.t
   | `Obj_T_UAMPER of Parse_info.t
   | `Obj_T_UID of string * Parse_info.t
   | `Obj_T_UMINUS of Parse_info.t
   | `Obj_T_UNKNOWN of Parse_info.t
   | `Obj_T_UPLUS of Parse_info.t
   | `Obj_T_USCOPE of Parse_info.t
   | `Obj_T_USER_BEG of string * Parse_info.t
   | `Obj_T_USTAR of Parse_info.t
   | `Obj_T_VBAR of Parse_info.t
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
   | `Obj_assign_op of Ast_ruby.binary_op * Parse_info.t
   | `Obj_bin_op of Ast_ruby.binary_op * Ast_ruby.tok
   | `Obj_body_exn of Ast_ruby.body_exn
   | `Obj_brace_codeblock of Ast_ruby.expr
   | `Obj_call_args of Ast_ruby.arguments
   | `Obj_case_else of (Ast_ruby.tok * Ast_ruby.stmts) option
   | `Obj_class_definition of Ast_ruby.expr
   | `Obj_class_inheritance of
        (Ast_ruby.tok * Ast_ruby.expr) option
   | `Obj_code_block of Ast_ruby.expr
   | `Obj_code_block_body of
        Ast_ruby.formal_param list option * Ast_ruby.stmts
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
   | `Obj_lhs_assign_op of
        Ast_ruby.expr * Ast_ruby.binary_op * Parse_info.t
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
        (Ast_ruby.tok * Ast_ruby.pattern list * Ast_ruby.stmts) list
   ], unit, unit, Lexing.lexbuf)
    Dyp.parser_pilot

val sgrep_spatch_pattern :
  ?global_data:unit ->
  ?local_data:unit ->
  (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast_ruby.any * string) list

val main :
  ?global_data:unit ->
  ?local_data:unit ->
  (Lexing.lexbuf -> token) ->
  Lexing.lexbuf -> (Ast_ruby.program * string) list
