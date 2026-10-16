(**
   Boilerplate to be used as a template when mapping the lua CST
   to another type of tree.
*)

module R = Tree_sitter_run.Raw_tree

(* Disable warnings against unused variables *)
[@@@warning "-26-27"]

(* Disable warning against unused 'rec' *)
[@@@warning "-39"]

type env = unit

let token (env : env) (tok : Tree_sitter_run.Token.t) =
  R.Token tok

let blank (env : env) () =
  R.Tuple []

let map_field_sep (env : env) (x : CST.field_sep) =
  (match x with
  | `COMMA tok -> R.Case ("COMMA",
      (* "," *) token env tok
    )
  | `SEMI tok -> R.Case ("SEMI",
      (* ";" *) token env tok
    )
  )

let map_identifier (env : env) (tok : CST.identifier) =
  (* identifier *) token env tok

let map_block_comment_end (env : env) (tok : CST.block_comment_end) =
  (* block_comment_end *) token env tok

let map_block_string_end (env : env) (tok : CST.block_string_end) =
  (* block_string_end *) token env tok

let map_block_comment_content (env : env) (tok : CST.block_comment_content) =
  (* block_comment_content *) token env tok

let map_hash_bang_line (env : env) (tok : CST.hash_bang_line) =
  (* pattern "#(?:![^\\r\\n\\u2028\\u2029]*|[ \\t\\f\\v]+[^\\$\"'\\[{(\\.\\r\\n\\u2028\\u2029][^\\r\\n\\u2028\\u2029]*\
  )" *) token env tok

let map_pat_f630af3 (env : env) (tok : CST.pat_f630af3) =
  (* pattern [^\r\n]* *) token env tok

let map_imm_tok_bslash_choice_cr (env : env) (tok : CST.imm_tok_bslash_choice_cr) =
  (* imm_tok_bslash_choice_cr *) token env tok

let map_block_string_start (env : env) (tok : CST.block_string_start) =
  (* block_string_start *) token env tok

let map_imm_tok_bslash_choice_pat_4504a10 (env : env) (tok : CST.imm_tok_bslash_choice_pat_4504a10) =
  (* imm_tok_bslash_choice_pat_4504a10 *) token env tok

let map_imm_tok_prec_p1_pat_3a2a380 (env : env) (tok : CST.imm_tok_prec_p1_pat_3a2a380) =
  (* pattern "[^\"\\\\]+" *) token env tok

let map_block_comment_start (env : env) (tok : CST.block_comment_start) =
  (* block_comment_start *) token env tok

let map_number (env : env) (tok : CST.number) =
  (* number *) token env tok

let map_block_string_content (env : env) (tok : CST.block_string_content) =
  (* block_string_content *) token env tok

let map_imm_tok_prec_p1_pat_dc28280 (env : env) (tok : CST.imm_tok_prec_p1_pat_dc28280) =
  (* pattern "[^'\\\\]+" *) token env tok

let map_label_statement (env : env) ((v1, v2, v3) : CST.label_statement) =
  let v1 = (* "::" *) token env v1 in
  let v2 = (* identifier *) token env v2 in
  let v3 = (* "::" *) token env v3 in
  R.Tuple [v1; v2; v3]

let map_name_list (env : env) ((v1, v2) : CST.name_list) =
  let v1 = (* identifier *) token env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = (* identifier *) token env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  R.Tuple [v1; v2]

let map_goto_statement (env : env) ((v1, v2) : CST.goto_statement) =
  let v1 = (* "goto" *) token env v1 in
  let v2 = (* identifier *) token env v2 in
  R.Tuple [v1; v2]

let map_attrib (env : env) ((v1, v2, v3) : CST.attrib) =
  let v1 = (* "<" *) token env v1 in
  let v2 = (* identifier *) token env v2 in
  let v3 = (* ">" *) token env v3 in
  R.Tuple [v1; v2; v3]

let rec map_function_name_prefix_expression (env : env) (x : CST.function_name_prefix_expression) =
  (match x with
  | `Id tok -> R.Case ("Id",
      (* identifier *) token env tok
    )
  | `Func_name_dot_index_exp (v1, v2, v3) -> R.Case ("Func_name_dot_index_exp",
      let v1 = map_function_name_prefix_expression env v1 in
      let v2 = (* "." *) token env v2 in
      let v3 = (* identifier *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

let map_escape_sequence (env : env) (x : CST.escape_sequence) =
  (match x with
  | `Imm_tok_bslash_choice_pat_4504a10 x -> R.Case ("Imm_tok_bslash_choice_pat_4504a10",
      map_imm_tok_bslash_choice_pat_4504a10 env x
    )
  | `Imm_tok_bslash_choice_cr x -> R.Case ("Imm_tok_bslash_choice_cr",
      map_imm_tok_bslash_choice_cr env x
    )
  )

let map_vararg_parameter (env : env) ((v1, v2) : CST.vararg_parameter) =
  let v1 = (* "..." *) token env v1 in
  let v2 =
    (match v2 with
    | Some tok -> R.Option (Some (
        (* identifier *) token env tok
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2]

let map_att_name_list (env : env) ((v1, v2, v3, v4) : CST.att_name_list) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        map_attrib env x
      ))
    | None -> R.Option None)
  in
  let v2 = (* identifier *) token env v2 in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_attrib env x
      ))
    | None -> R.Option None)
  in
  let v4 =
    R.List (List.map (fun (v1, v2, v3) ->
      let v1 = (* "," *) token env v1 in
      let v2 = (* identifier *) token env v2 in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_attrib env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3]
    ) v4)
  in
  R.Tuple [v1; v2; v3; v4]

let map_doublequote_string_content (env : env) (xs : CST.doublequote_string_content) =
  R.List (List.map (fun x ->
    (match x with
    | `Imm_tok_prec_p1_pat_3a2a380 x -> R.Case ("Imm_tok_prec_p1_pat_3a2a380",
        map_imm_tok_prec_p1_pat_3a2a380 env x
      )
    | `Esc_seq x -> R.Case ("Esc_seq",
        map_escape_sequence env x
      )
    )
  ) xs)

let map_singlequote_string_content (env : env) (xs : CST.singlequote_string_content) =
  R.List (List.map (fun x ->
    (match x with
    | `Imm_tok_prec_p1_pat_dc28280 x -> R.Case ("Imm_tok_prec_p1_pat_dc28280",
        map_imm_tok_prec_p1_pat_dc28280 env x
      )
    | `Esc_seq x -> R.Case ("Esc_seq",
        map_escape_sequence env x
      )
    )
  ) xs)

let map_parameter_list (env : env) (x : CST.parameter_list) =
  (match x with
  | `Choice_id_rep_COMMA_id_opt_COMMA_vararg_param x -> R.Case ("Choice_id_rep_COMMA_id_opt_COMMA_vararg_param",
      (match x with
      | `Id_rep_COMMA_id_opt_COMMA_vararg_param (v1, v2, v3) -> R.Case ("Id_rep_COMMA_id_opt_COMMA_vararg_param",
          let v1 = (* identifier *) token env v1 in
          let v2 =
            R.List (List.map (fun (v1, v2) ->
              let v1 = (* "," *) token env v1 in
              let v2 = (* identifier *) token env v2 in
              R.Tuple [v1; v2]
            ) v2)
          in
          let v3 =
            (match v3 with
            | Some (v1, v2) -> R.Option (Some (
                let v1 = (* "," *) token env v1 in
                let v2 = map_vararg_parameter env v2 in
                R.Tuple [v1; v2]
              ))
            | None -> R.Option None)
          in
          R.Tuple [v1; v2; v3]
        )
      | `Vararg_param x -> R.Case ("Vararg_param",
          map_vararg_parameter env x
        )
      )
    )
  | `Vararg_exp_rep1_COMMA_id_opt_COMMA_vararg_param (v1, v2, v3) -> R.Case ("Vararg_exp_rep1_COMMA_id_opt_COMMA_vararg_param",
      let v1 = (* "..." *) token env v1 in
      let v2 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = (* "," *) token env v1 in
          let v2 = (* identifier *) token env v2 in
          R.Tuple [v1; v2]
        ) v2)
      in
      let v3 =
        (match v3 with
        | Some (v1, v2) -> R.Option (Some (
            let v1 = (* "," *) token env v1 in
            let v2 = map_vararg_parameter env v2 in
            R.Tuple [v1; v2]
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3]
    )
  | `Vararg_exp_COMMA_vararg_param (v1, v2, v3) -> R.Case ("Vararg_exp_COMMA_vararg_param",
      let v1 = (* "..." *) token env v1 in
      let v2 = (* "," *) token env v2 in
      let v3 = map_vararg_parameter env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

let map_function_name (env : env) (x : CST.function_name) =
  (match x with
  | `Func_name_prefix_exp x -> R.Case ("Func_name_prefix_exp",
      map_function_name_prefix_expression env x
    )
  | `Func_name_meth_index_exp (v1, v2, v3) -> R.Case ("Func_name_meth_index_exp",
      let v1 = map_function_name_prefix_expression env v1 in
      let v2 = (* ":" *) token env v2 in
      let v3 = (* identifier *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

let map_quote_string (env : env) (x : CST.quote_string) =
  (match x with
  | `DQUOT_opt_doub_str_content_DQUOT (v1, v2, v3) -> R.Case ("DQUOT_opt_doub_str_content_DQUOT",
      let v1 = (* "\"" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_doublequote_string_content env x
          ))
        | None -> R.Option None)
      in
      let v3 = (* "\"" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `SQUOT_opt_sing_str_content_SQUOT (v1, v2, v3) -> R.Case ("SQUOT_opt_sing_str_content_SQUOT",
      let v1 = (* "'" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_singlequote_string_content env x
          ))
        | None -> R.Option None)
      in
      let v3 = (* "'" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

let map_parameters (env : env) ((v1, v2, v3) : CST.parameters) =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_parameter_list env x
      ))
    | None -> R.Option None)
  in
  let v3 = (* ")" *) token env v3 in
  R.Tuple [v1; v2; v3]

let map_string_ (env : env) (x : CST.string_) =
  (match x with
  | `Quote_str x -> R.Case ("Quote_str",
      map_quote_string env x
    )
  | `Blk_str (v1, v2, v3) -> R.Case ("Blk_str",
      let v1 = (* block_string_start *) token env v1 in
      let v2 = (* block_string_content *) token env v2 in
      let v3 = (* block_string_end *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

let rec map_anon_choice_att_name_list_26e8983 (env : env) (x : CST.anon_choice_att_name_list_26e8983) =
  (match x with
  | `Att_name_list x -> R.Case ("Att_name_list",
      map_att_name_list env x
    )
  | `Var_assign (v1, v2, v3) -> R.Case ("Var_assign",
      let v1 = map_att_name_list env v1 in
      let v2 = (* "=" *) token env v2 in
      let v3 = map_variable_assignment_explist env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_arguments (env : env) (x : CST.arguments) =
  (match x with
  | `LPAR_opt_exp_rep_COMMA_exp_RPAR (v1, v2, v3) -> R.Case ("LPAR_opt_exp_rep_COMMA_exp_RPAR",
      let v1 = (* "(" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_variable_assignment_explist env x
          ))
        | None -> R.Option None)
      in
      let v3 = (* ")" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Table_cons x -> R.Case ("Table_cons",
      map_table_constructor env x
    )
  | `Str x -> R.Case ("Str",
      map_string_ env x
    )
  )

and map_assignment_statement (env : env) ((v1, v2, v3) : CST.assignment_statement) =
  let v1 = map_variable_assignment_varlist env v1 in
  let v2 = (* "=" *) token env v2 in
  let v3 = map_variable_assignment_explist env v3 in
  R.Tuple [v1; v2; v3]

and map_binary_expression (env : env) (x : CST.binary_expression) =
  (match x with
  | `Exp_or_exp (v1, v2, v3) -> R.Case ("Exp_or_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "or" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_and_exp (v1, v2, v3) -> R.Case ("Exp_and_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "and" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_LT_exp (v1, v2, v3) -> R.Case ("Exp_LT_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "<" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_LTEQ_exp (v1, v2, v3) -> R.Case ("Exp_LTEQ_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "<=" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_EQEQ_exp (v1, v2, v3) -> R.Case ("Exp_EQEQ_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "==" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_TILDEEQ_exp (v1, v2, v3) -> R.Case ("Exp_TILDEEQ_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "~=" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_GTEQ_exp (v1, v2, v3) -> R.Case ("Exp_GTEQ_exp",
      let v1 = map_expression env v1 in
      let v2 = (* ">=" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_GT_exp (v1, v2, v3) -> R.Case ("Exp_GT_exp",
      let v1 = map_expression env v1 in
      let v2 = (* ">" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_BAR_exp (v1, v2, v3) -> R.Case ("Exp_BAR_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "|" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_TILDE_exp (v1, v2, v3) -> R.Case ("Exp_TILDE_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "~" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_AMP_exp (v1, v2, v3) -> R.Case ("Exp_AMP_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "&" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_LTLT_exp (v1, v2, v3) -> R.Case ("Exp_LTLT_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "<<" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_GTGT_exp (v1, v2, v3) -> R.Case ("Exp_GTGT_exp",
      let v1 = map_expression env v1 in
      let v2 = (* ">>" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_PLUS_exp (v1, v2, v3) -> R.Case ("Exp_PLUS_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "+" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_DASH_exp (v1, v2, v3) -> R.Case ("Exp_DASH_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "-" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_STAR_exp (v1, v2, v3) -> R.Case ("Exp_STAR_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "*" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_SLASH_exp (v1, v2, v3) -> R.Case ("Exp_SLASH_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "/" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_SLASHSLASH_exp (v1, v2, v3) -> R.Case ("Exp_SLASHSLASH_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "//" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_PERC_exp (v1, v2, v3) -> R.Case ("Exp_PERC_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "%" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_DOTDOT_exp (v1, v2, v3) -> R.Case ("Exp_DOTDOT_exp",
      let v1 = map_expression env v1 in
      let v2 = (* ".." *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_HAT_exp (v1, v2, v3) -> R.Case ("Exp_HAT_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "^" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_block (env : env) (x : CST.block) =
  (match x with
  | `Rep1_stmt_opt_ret_stmt (v1, v2) -> R.Case ("Rep1_stmt_opt_ret_stmt",
      let v1 = R.List (List.map (map_statement env) v1) in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_return_statement env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2]
    )
  | `Rep_stmt_ret_stmt (v1, v2) -> R.Case ("Rep_stmt_ret_stmt",
      let v1 = R.List (List.map (map_statement env) v1) in
      let v2 = map_return_statement env v2 in
      R.Tuple [v1; v2]
    )
  )

and map_declaration (env : env) (x : CST.declaration) =
  (match x with
  | `Func_decl (v1, v2, v3) -> R.Case ("Func_decl",
      let v1 = (* "function" *) token env v1 in
      let v2 = map_function_name env v2 in
      let v3 = map_function_body env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Local_func_decl (v1, v2, v3, v4) -> R.Case ("Local_func_decl",
      let v1 = (* "local" *) token env v1 in
      let v2 = (* "function" *) token env v2 in
      let v3 = (* identifier *) token env v3 in
      let v4 = map_function_body env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Var_decl (v1, v2) -> R.Case ("Var_decl",
      let v1 = (* "local" *) token env v1 in
      let v2 = map_anon_choice_att_name_list_26e8983 env v2 in
      R.Tuple [v1; v2]
    )
  | `Global_func_decl (v1, v2, v3, v4) -> R.Case ("Global_func_decl",
      let v1 = (* "global" *) token env v1 in
      let v2 = (* "function" *) token env v2 in
      let v3 = (* identifier *) token env v3 in
      let v4 = map_function_body env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Global_var_decl (v1, v2) -> R.Case ("Global_var_decl",
      let v1 = (* "global" *) token env v1 in
      let v2 = map_anon_choice_att_name_list_26e8983 env v2 in
      R.Tuple [v1; v2]
    )
  | `Global_impl_var_decl (v1, v2, v3) -> R.Case ("Global_impl_var_decl",
      let v1 = (* "global" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_attrib env x
          ))
        | None -> R.Option None)
      in
      let v3 = (* "*" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_do_statement (env : env) ((v1, v2, v3) : CST.do_statement) =
  let v1 = (* "do" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_block env x
      ))
    | None -> R.Option None)
  in
  let v3 = (* "end" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_else_statement (env : env) ((v1, v2) : CST.else_statement) =
  let v1 = (* "else" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_block env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2]

and map_elseif_statement (env : env) ((v1, v2, v3, v4) : CST.elseif_statement) =
  let v1 = (* "elseif" *) token env v1 in
  let v2 = map_expression env v2 in
  let v3 = (* "then" *) token env v3 in
  let v4 =
    (match v4 with
    | Some x -> R.Option (Some (
        map_block env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3; v4]

and map_expression (env : env) (x : CST.expression) =
  (match x with
  | `Nil tok -> R.Case ("Nil",
      (* "nil" *) token env tok
    )
  | `False tok -> R.Case ("False",
      (* "false" *) token env tok
    )
  | `True tok -> R.Case ("True",
      (* "true" *) token env tok
    )
  | `Num tok -> R.Case ("Num",
      (* number *) token env tok
    )
  | `Str x -> R.Case ("Str",
      map_string_ env x
    )
  | `Vararg_exp tok -> R.Case ("Vararg_exp",
      (* "..." *) token env tok
    )
  | `Func_defi (v1, v2) -> R.Case ("Func_defi",
      let v1 = (* "function" *) token env v1 in
      let v2 = map_function_body env v2 in
      R.Tuple [v1; v2]
    )
  | `Var x -> R.Case ("Var",
      map_variable env x
    )
  | `Func_call x -> R.Case ("Func_call",
      map_function_call env x
    )
  | `Paren_exp x -> R.Case ("Paren_exp",
      map_parenthesized_expression env x
    )
  | `Table_cons x -> R.Case ("Table_cons",
      map_table_constructor env x
    )
  | `Bin_exp x -> R.Case ("Bin_exp",
      map_binary_expression env x
    )
  | `Un_exp (v1, v2) -> R.Case ("Un_exp",
      let v1 =
        (match v1 with
        | `Not tok -> R.Case ("Not",
            (* "not" *) token env tok
          )
        | `HASH tok -> R.Case ("HASH",
            (* "#" *) token env tok
          )
        | `DASH tok -> R.Case ("DASH",
            (* "-" *) token env tok
          )
        | `TILDE tok -> R.Case ("TILDE",
            (* "~" *) token env tok
          )
        )
      in
      let v2 = map_expression env v2 in
      R.Tuple [v1; v2]
    )
  )

and map_expression_list (env : env) ((v1, v2) : CST.expression_list) =
  let v1 = map_expression env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_expression env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  R.Tuple [v1; v2]

and map_field (env : env) (x : CST.field) =
  (match x with
  | `LBRACK_exp_RBRACK_EQ_exp (v1, v2, v3, v4, v5) -> R.Case ("LBRACK_exp_RBRACK_EQ_exp",
      let v1 = (* "[" *) token env v1 in
      let v2 = map_expression env v2 in
      let v3 = (* "]" *) token env v3 in
      let v4 = (* "=" *) token env v4 in
      let v5 = map_expression env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `Choice_cont_kw_EQ_exp (v1, v2, v3) -> R.Case ("Choice_cont_kw_EQ_exp",
      let v1 =
        (match v1 with
        | `Cont_kw tok -> R.Case ("Cont_kw",
            (* "global" *) token env tok
          )
        | `Id tok -> R.Case ("Id",
            (* identifier *) token env tok
          )
        )
      in
      let v2 = (* "=" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp x -> R.Case ("Exp",
      map_expression env x
    )
  )

and map_field_list (env : env) ((v1, v2, v3) : CST.field_list) =
  let v1 = map_field env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = map_field_sep env v1 in
      let v2 = map_field env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_field_sep env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3]

and map_for_generic_clause (env : env) ((v1, v2, v3) : CST.for_generic_clause) =
  let v1 = map_name_list env v1 in
  let v2 = (* "in" *) token env v2 in
  let v3 = map_expression_list env v3 in
  R.Tuple [v1; v2; v3]

and map_for_numeric_clause (env : env) ((v1, v2, v3, v4, v5, v6) : CST.for_numeric_clause) =
  let v1 = (* identifier *) token env v1 in
  let v2 = (* "=" *) token env v2 in
  let v3 = map_expression env v3 in
  let v4 = (* "," *) token env v4 in
  let v5 = map_expression env v5 in
  let v6 =
    (match v6 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = (* "," *) token env v1 in
        let v2 = map_expression env v2 in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3; v4; v5; v6]

and map_for_statement (env : env) ((v1, v2, v3, v4, v5) : CST.for_statement) =
  let v1 = (* "for" *) token env v1 in
  let v2 =
    (match v2 with
    | `For_gene_clause x -> R.Case ("For_gene_clause",
        map_for_generic_clause env x
      )
    | `For_nume_clause x -> R.Case ("For_nume_clause",
        map_for_numeric_clause env x
      )
    )
  in
  let v3 = (* "do" *) token env v3 in
  let v4 =
    (match v4 with
    | Some x -> R.Option (Some (
        map_block env x
      ))
    | None -> R.Option None)
  in
  let v5 = (* "end" *) token env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

and map_function_body (env : env) ((v1, v2, v3) : CST.function_body) =
  let v1 = map_parameters env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_block env x
      ))
    | None -> R.Option None)
  in
  let v3 = (* "end" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_function_call (env : env) ((v1, v2) : CST.function_call) =
  let v1 =
    (match v1 with
    | `Prefix_exp x -> R.Case ("Prefix_exp",
        map_prefix_expression env x
      )
    | `Meth_index_exp x -> R.Case ("Meth_index_exp",
        map_method_index_expression env x
      )
    )
  in
  let v2 = map_arguments env v2 in
  R.Tuple [v1; v2]

and map_if_statement (env : env) ((v1, v2, v3, v4, v5, v6, v7) : CST.if_statement) =
  let v1 = (* "if" *) token env v1 in
  let v2 = map_expression env v2 in
  let v3 = (* "then" *) token env v3 in
  let v4 =
    (match v4 with
    | Some x -> R.Option (Some (
        map_block env x
      ))
    | None -> R.Option None)
  in
  let v5 = R.List (List.map (map_elseif_statement env) v5) in
  let v6 =
    (match v6 with
    | Some x -> R.Option (Some (
        map_else_statement env x
      ))
    | None -> R.Option None)
  in
  let v7 = (* "end" *) token env v7 in
  R.Tuple [v1; v2; v3; v4; v5; v6; v7]

and map_method_index_expression (env : env) ((v1, v2, v3) : CST.method_index_expression) =
  let v1 = map_prefix_expression env v1 in
  let v2 = (* ":" *) token env v2 in
  let v3 = (* identifier *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_parenthesized_expression (env : env) ((v1, v2, v3) : CST.parenthesized_expression) =
  let v1 = (* "(" *) token env v1 in
  let v2 = map_expression env v2 in
  let v3 = (* ")" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_prefix_expression (env : env) (x : CST.prefix_expression) =
  (match x with
  | `Var x -> R.Case ("Var",
      map_variable env x
    )
  | `Func_call x -> R.Case ("Func_call",
      map_function_call env x
    )
  | `Paren_exp x -> R.Case ("Paren_exp",
      map_parenthesized_expression env x
    )
  )

and map_repeat_statement (env : env) ((v1, v2, v3, v4) : CST.repeat_statement) =
  let v1 = (* "repeat" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_block env x
      ))
    | None -> R.Option None)
  in
  let v3 = (* "until" *) token env v3 in
  let v4 = map_expression env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_return_statement (env : env) ((v1, v2, v3) : CST.return_statement) =
  let v1 = (* "return" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_expression_list env x
      ))
    | None -> R.Option None)
  in
  let v3 =
    (match v3 with
    | Some tok -> R.Option (Some (
        (* ";" *) token env tok
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3]

and map_statement (env : env) (x : CST.statement) =
  (match x with
  | `Choice_empty_stmt x -> R.Case ("Choice_empty_stmt",
      (match x with
      | `Empty_stmt tok -> R.Case ("Empty_stmt",
          (* ";" *) token env tok
        )
      | `Assign_stmt x -> R.Case ("Assign_stmt",
          map_assignment_statement env x
        )
      | `Func_call x -> R.Case ("Func_call",
          map_function_call env x
        )
      | `Label_stmt x -> R.Case ("Label_stmt",
          map_label_statement env x
        )
      | `Brk_stmt tok -> R.Case ("Brk_stmt",
          (* "break" *) token env tok
        )
      | `Goto_stmt x -> R.Case ("Goto_stmt",
          map_goto_statement env x
        )
      | `Do_stmt x -> R.Case ("Do_stmt",
          map_do_statement env x
        )
      | `While_stmt x -> R.Case ("While_stmt",
          map_while_statement env x
        )
      | `Repeat_stmt x -> R.Case ("Repeat_stmt",
          map_repeat_statement env x
        )
      | `If_stmt x -> R.Case ("If_stmt",
          map_if_statement env x
        )
      | `For_stmt x -> R.Case ("For_stmt",
          map_for_statement env x
        )
      | `Decl x -> R.Case ("Decl",
          map_declaration env x
        )
      )
    )
  | `Exp x -> R.Case ("Exp",
      map_expression env x
    )
  )

and map_table_constructor (env : env) ((v1, v2, v3) : CST.table_constructor) =
  let v1 = (* "{" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_field_list env x
      ))
    | None -> R.Option None)
  in
  let v3 = (* "}" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_variable (env : env) (x : CST.variable) =
  (match x with
  | `Cont_kw tok -> R.Case ("Cont_kw",
      (* "global" *) token env tok
    )
  | `Id tok -> R.Case ("Id",
      (* identifier *) token env tok
    )
  | `Brac_index_exp (v1, v2, v3, v4) -> R.Case ("Brac_index_exp",
      let v1 = map_prefix_expression env v1 in
      let v2 = (* "[" *) token env v2 in
      let v3 = map_expression env v3 in
      let v4 = (* "]" *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Dot_index_exp (v1, v2, v3) -> R.Case ("Dot_index_exp",
      let v1 = map_prefix_expression env v1 in
      let v2 = (* "." *) token env v2 in
      let v3 = (* identifier *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_variable_assignment_explist (env : env) ((v1, v2) : CST.variable_assignment_explist) =
  let v1 = map_expression env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_expression env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  R.Tuple [v1; v2]

and map_variable_assignment_varlist (env : env) ((v1, v2) : CST.variable_assignment_varlist) =
  let v1 = map_variable env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_variable env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  R.Tuple [v1; v2]

and map_while_statement (env : env) ((v1, v2, v3, v4, v5) : CST.while_statement) =
  let v1 = (* "while" *) token env v1 in
  let v2 = map_expression env v2 in
  let v3 = (* "do" *) token env v3 in
  let v4 =
    (match v4 with
    | Some x -> R.Option (Some (
        map_block env x
      ))
    | None -> R.Option None)
  in
  let v5 = (* "end" *) token env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

let map_chunk (env : env) ((v1, v2, v3) : CST.chunk) =
  let v1 =
    (match v1 with
    | Some tok -> R.Option (Some (
        (* pattern "#(?:![^\\r\\n\\u2028\\u2029]*|[ \\t\\f\\v]+[^\\$\"'\\[{(\\.\\r\\n\\u2028\\u2029][^\\r\\n\\u2028\\u2029]*\
  )" *) token env tok
      ))
    | None -> R.Option None)
  in
  let v2 = R.List (List.map (map_statement env) v2) in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_return_statement env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3]

let map_comment (env : env) (x : CST.comment) =
  (match x with
  | `DASHDASH_pat_f630af3 (v1, v2) -> R.Case ("DASHDASH_pat_f630af3",
      let v1 = (* "--" *) token env v1 in
      let v2 = map_pat_f630af3 env v2 in
      R.Tuple [v1; v2]
    )
  | `Blk_comm_start_blk_comm_content_blk_comm_end (v1, v2, v3) -> R.Case ("Blk_comm_start_blk_comm_content_blk_comm_end",
      let v1 = (* block_comment_start *) token env v1 in
      let v2 = (* block_comment_content *) token env v2 in
      let v3 = (* block_comment_end *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

let dump_tree root =
  map_chunk () root
  |> Tree_sitter_run.Raw_tree.to_channel stdout

let map_extra (env : env) (x : CST.extra) =
  match x with
  | `Comment (_loc, x) -> ("comment", "comment", map_comment env x)

let dump_extras (extras : CST.extras) =
  List.iter (fun extra ->
    let ts_rule_name, ocaml_type_name, raw_tree = map_extra () extra in
    let details =
      if ocaml_type_name <> ts_rule_name then
        Printf.sprintf " (OCaml type '%s')" ocaml_type_name
      else
        ""
    in
    Printf.printf "%s%s:\n" ts_rule_name details;
    Tree_sitter_run.Raw_tree.to_channel stdout raw_tree
  ) extras
