(**
   Boilerplate to be used as a template when mapping the jsonnet CST
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

let map_semgrep_metavariable (env : env) (tok : CST.semgrep_metavariable) =
  (* semgrep_metavariable *) token env tok

let map_string_start (env : env) (tok : CST.string_start) =
  (* string_start *) token env tok

let map_escape_sequence (env : env) (tok : CST.escape_sequence) =
  (* escape_sequence *) token env tok

let map_number (env : env) (tok : CST.number) =
  (* number *) token env tok

let map_ident (env : env) (tok : CST.ident) =
  (* pattern [_a-zA-Z][_a-zA-Z0-9]* *) token env tok

let map_string_content (env : env) (tok : CST.string_content) =
  (* string_content *) token env tok

let map_string_end (env : env) (tok : CST.string_end) =
  (* string_end *) token env tok

let map_imm_tok_prec_p1_pat_59587ce (env : env) (tok : CST.imm_tok_prec_p1_pat_59587ce) =
  (* pattern "[^\\\\'\\n]+" *) token env tok

let map_h (env : env) (x : CST.h) =
  (match x with
  | `COLON tok -> R.Case ("COLON",
      (* ":" *) token env tok
    )
  | `COLONCOLON tok -> R.Case ("COLONCOLON",
      (* "::" *) token env tok
    )
  | `COLONCOLONCOLON tok -> R.Case ("COLONCOLONCOLON",
      (* ":::" *) token env tok
    )
  )

let map_imm_tok_prec_p1_pat_c7f65b4 (env : env) (tok : CST.imm_tok_prec_p1_pat_c7f65b4) =
  (* pattern "[^\\\\\"\\n]+" *) token env tok

let map_unaryop (env : env) (x : CST.unaryop) =
  (match x with
  | `DASH tok -> R.Case ("DASH",
      (* "-" *) token env tok
    )
  | `PLUS tok -> R.Case ("PLUS",
      (* "+" *) token env tok
    )
  | `BANG tok -> R.Case ("BANG",
      (* "!" *) token env tok
    )
  | `TILDE tok -> R.Case ("TILDE",
      (* "~" *) token env tok
    )
  )

let map_id (env : env) (x : CST.id) =
  (match x with
  | `Id tok -> R.Case ("Id",
      (* pattern [_a-zA-Z][_a-zA-Z0-9]* *) token env tok
    )
  | `Semg_meta tok -> R.Case ("Semg_meta",
      (* semgrep_metavariable *) token env tok
    )
  )

let map_str_single (env : env) (xs : CST.str_single) =
  R.List (List.map (fun x ->
    (match x with
    | `Imm_tok_prec_p1_pat_59587ce x -> R.Case ("Imm_tok_prec_p1_pat_59587ce",
        map_imm_tok_prec_p1_pat_59587ce env x
      )
    | `Esc_seq tok -> R.Case ("Esc_seq",
        (* escape_sequence *) token env tok
      )
    )
  ) xs)

let map_str_double (env : env) (xs : CST.str_double) =
  R.List (List.map (fun x ->
    (match x with
    | `Imm_tok_prec_p1_pat_c7f65b4 x -> R.Case ("Imm_tok_prec_p1_pat_c7f65b4",
        map_imm_tok_prec_p1_pat_c7f65b4 env x
      )
    | `Esc_seq tok -> R.Case ("Esc_seq",
        (* escape_sequence *) token env tok
      )
    )
  ) xs)

let map_string_ (env : env) (x : CST.string_) =
  (match x with
  | `Opt_AT_single_single (v1, v2, v3) -> R.Case ("Opt_AT_single_single",
      let v1 =
        (match v1 with
        | Some tok -> R.Option (Some (
            (* "@" *) token env tok
          ))
        | None -> R.Option None)
      in
      let v2 = (* "'" *) token env v2 in
      let v3 = (* "'" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Opt_AT_single_str_single_single (v1, v2, v3, v4) -> R.Case ("Opt_AT_single_str_single_single",
      let v1 =
        (match v1 with
        | Some tok -> R.Option (Some (
            (* "@" *) token env tok
          ))
        | None -> R.Option None)
      in
      let v2 = (* "'" *) token env v2 in
      let v3 = map_str_single env v3 in
      let v4 = (* "'" *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Opt_AT_double_double (v1, v2, v3) -> R.Case ("Opt_AT_double_double",
      let v1 =
        (match v1 with
        | Some tok -> R.Option (Some (
            (* "@" *) token env tok
          ))
        | None -> R.Option None)
      in
      let v2 = (* "\"" *) token env v2 in
      let v3 = (* "\"" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Opt_AT_double_str_double_double (v1, v2, v3, v4) -> R.Case ("Opt_AT_double_str_double_double",
      let v1 =
        (match v1 with
        | Some tok -> R.Option (Some (
            (* "@" *) token env tok
          ))
        | None -> R.Option None)
      in
      let v2 = (* "\"" *) token env v2 in
      let v3 = map_str_double env v3 in
      let v4 = (* "\"" *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Opt_AT_str_start_str_content_str_end (v1, v2, v3, v4) -> R.Case ("Opt_AT_str_start_str_content_str_end",
      let v1 =
        (match v1 with
        | Some tok -> R.Option (Some (
            (* "@" *) token env tok
          ))
        | None -> R.Option None)
      in
      let v2 = (* string_start *) token env v2 in
      let v3 = (* string_content *) token env v3 in
      let v4 = (* string_end *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  )

let map_importstr (env : env) ((v1, v2) : CST.importstr) =
  let v1 = (* "importstr" *) token env v1 in
  let v2 = map_string_ env v2 in
  R.Tuple [v1; v2]

let map_import (env : env) ((v1, v2) : CST.import) =
  let v1 = (* "import" *) token env v1 in
  let v2 = map_string_ env v2 in
  R.Tuple [v1; v2]

let rec map_anonymous_function (env : env) ((v1, v2, v3, v4, v5) : CST.anonymous_function) =
  let v1 = (* "function" *) token env v1 in
  let v2 = (* "(" *) token env v2 in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_params env x
      ))
    | None -> R.Option None)
  in
  let v4 = (* ")" *) token env v4 in
  let v5 = map_document env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

and map_args (env : env) (x : CST.args) =
  (match x with
  | `Expr_rep_COMMA_expr_rep_COMMA_named_arg_opt_COMMA (v1, v2, v3, v4) -> R.Case ("Expr_rep_COMMA_expr_rep_COMMA_named_arg_opt_COMMA",
      let v1 = map_document env v1 in
      let v2 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = (* "," *) token env v1 in
          let v2 = map_document env v2 in
          R.Tuple [v1; v2]
        ) v2)
      in
      let v3 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = (* "," *) token env v1 in
          let v2 = map_named_argument env v2 in
          R.Tuple [v1; v2]
        ) v3)
      in
      let v4 =
        (match v4 with
        | Some tok -> R.Option (Some (
            (* "," *) token env tok
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Named_arg_rep_COMMA_named_arg_opt_COMMA (v1, v2, v3) -> R.Case ("Named_arg_rep_COMMA_named_arg_opt_COMMA",
      let v1 = map_named_argument env v1 in
      let v2 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = (* "," *) token env v1 in
          let v2 = map_named_argument env v2 in
          R.Tuple [v1; v2]
        ) v2)
      in
      let v3 =
        (match v3 with
        | Some tok -> R.Option (Some (
            (* "," *) token env tok
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3]
    )
  )

and map_assert_ (env : env) ((v1, v2, v3) : CST.assert_) =
  let v1 = (* "assert" *) token env v1 in
  let v2 = map_document env v2 in
  let v3 =
    (match v3 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = (* ":" *) token env v1 in
        let v2 = map_document env v2 in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3]

and map_binary_expr (env : env) (x : CST.binary_expr) =
  (match x with
  | `Expr_choice_STAR_expr (v1, v2, v3) -> R.Case ("Expr_choice_STAR_expr",
      let v1 = map_document env v1 in
      let v2 =
        (match v2 with
        | `STAR tok -> R.Case ("STAR",
            (* "*" *) token env tok
          )
        | `SLASH tok -> R.Case ("SLASH",
            (* "/" *) token env tok
          )
        | `PERC tok -> R.Case ("PERC",
            (* "%" *) token env tok
          )
        )
      in
      let v3 = map_document env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Expr_choice_PLUS_expr (v1, v2, v3) -> R.Case ("Expr_choice_PLUS_expr",
      let v1 = map_document env v1 in
      let v2 =
        (match v2 with
        | `PLUS tok -> R.Case ("PLUS",
            (* "+" *) token env tok
          )
        | `DASH tok -> R.Case ("DASH",
            (* "-" *) token env tok
          )
        )
      in
      let v3 = map_document env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Expr_choice_LTLT_expr (v1, v2, v3) -> R.Case ("Expr_choice_LTLT_expr",
      let v1 = map_document env v1 in
      let v2 =
        (match v2 with
        | `LTLT tok -> R.Case ("LTLT",
            (* "<<" *) token env tok
          )
        | `GTGT tok -> R.Case ("GTGT",
            (* ">>" *) token env tok
          )
        )
      in
      let v3 = map_document env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Expr_choice_LT_expr (v1, v2, v3) -> R.Case ("Expr_choice_LT_expr",
      let v1 = map_document env v1 in
      let v2 =
        (match v2 with
        | `LT tok -> R.Case ("LT",
            (* "<" *) token env tok
          )
        | `LTEQ tok -> R.Case ("LTEQ",
            (* "<=" *) token env tok
          )
        | `GT tok -> R.Case ("GT",
            (* ">" *) token env tok
          )
        | `GTEQ tok -> R.Case ("GTEQ",
            (* ">=" *) token env tok
          )
        )
      in
      let v3 = map_document env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Expr_choice_EQEQ_expr (v1, v2, v3) -> R.Case ("Expr_choice_EQEQ_expr",
      let v1 = map_document env v1 in
      let v2 =
        (match v2 with
        | `EQEQ tok -> R.Case ("EQEQ",
            (* "==" *) token env tok
          )
        | `BANGEQ tok -> R.Case ("BANGEQ",
            (* "!=" *) token env tok
          )
        )
      in
      let v3 = map_document env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Expr_AMP_expr (v1, v2, v3) -> R.Case ("Expr_AMP_expr",
      let v1 = map_document env v1 in
      let v2 = (* "&" *) token env v2 in
      let v3 = map_document env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Expr_HAT_expr (v1, v2, v3) -> R.Case ("Expr_HAT_expr",
      let v1 = map_document env v1 in
      let v2 = (* "^" *) token env v2 in
      let v3 = map_document env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Expr_BAR_expr (v1, v2, v3) -> R.Case ("Expr_BAR_expr",
      let v1 = map_document env v1 in
      let v2 = (* "|" *) token env v2 in
      let v3 = map_document env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Expr_AMPAMP_expr (v1, v2, v3) -> R.Case ("Expr_AMPAMP_expr",
      let v1 = map_document env v1 in
      let v2 = (* "&&" *) token env v2 in
      let v3 = map_document env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Expr_BARBAR_expr (v1, v2, v3) -> R.Case ("Expr_BARBAR_expr",
      let v1 = map_document env v1 in
      let v2 = (* "||" *) token env v2 in
      let v3 = map_document env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_bind (env : env) (x : CST.bind) =
  (match x with
  | `Id_EQ_expr x -> R.Case ("Id_EQ_expr",
      map_named_argument env x
    )
  | `Id_LPAR_opt_params_RPAR_EQ_expr (v1, v2, v3, v4, v5, v6) -> R.Case ("Id_LPAR_opt_params_RPAR_EQ_expr",
      let v1 = map_id env v1 in
      let v2 = (* "(" *) token env v2 in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_params env x
          ))
        | None -> R.Option None)
      in
      let v4 = (* ")" *) token env v4 in
      let v5 = (* "=" *) token env v5 in
      let v6 = map_document env v6 in
      R.Tuple [v1; v2; v3; v4; v5; v6]
    )
  )

and map_compspec (env : env) (xs : CST.compspec) =
  R.List (List.map (fun x ->
    (match x with
    | `Fors x -> R.Case ("Fors",
        map_forspec env x
      )
    | `Ifspec x -> R.Case ("Ifspec",
        map_ifspec env x
      )
    )
  ) xs)

and map_document (env : env) (x : CST.document) =
  map_expr env x

and map_expr (env : env) (x : CST.expr) =
  (match x with
  | `Semg_ellips tok -> R.Case ("Semg_ellips",
      (* "..." *) token env tok
    )
  | `Deep_ellips (v1, v2, v3) -> R.Case ("Deep_ellips",
      let v1 = (* "<..." *) token env v1 in
      let v2 = map_document env v2 in
      let v3 = (* "...>" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Choice_null x -> R.Case ("Choice_null",
      (match x with
      | `Null tok -> R.Case ("Null",
          (* "null" *) token env tok
        )
      | `True tok -> R.Case ("True",
          (* "true" *) token env tok
        )
      | `False tok -> R.Case ("False",
          (* "false" *) token env tok
        )
      | `Self tok -> R.Case ("Self",
          (* "self" *) token env tok
        )
      | `Dollar tok -> R.Case ("Dollar",
          (* "$" *) token env tok
        )
      | `Str x -> R.Case ("Str",
          map_string_ env x
        )
      | `Num tok -> R.Case ("Num",
          (* number *) token env tok
        )
      | `LCURL_opt_choice_member_rep_COMMA_member_opt_COMMA_RCURL (v1, v2, v3) -> R.Case ("LCURL_opt_choice_member_rep_COMMA_member_opt_COMMA_RCURL",
          let v1 = (* "{" *) token env v1 in
          let v2 =
            (match v2 with
            | Some x -> R.Option (Some (
                map_objinside env x
              ))
            | None -> R.Option None)
          in
          let v3 = (* "}" *) token env v3 in
          R.Tuple [v1; v2; v3]
        )
      | `LBRACK_opt_expr_rep_COMMA_expr_opt_COMMA_RBRACK (v1, v2, v3) -> R.Case ("LBRACK_opt_expr_rep_COMMA_expr_opt_COMMA_RBRACK",
          let v1 = (* "[" *) token env v1 in
          let v2 =
            (match v2 with
            | Some (v1, v2, v3) -> R.Option (Some (
                let v1 = map_document env v1 in
                let v2 =
                  R.List (List.map (fun (v1, v2) ->
                    let v1 = (* "," *) token env v1 in
                    let v2 = map_document env v2 in
                    R.Tuple [v1; v2]
                  ) v2)
                in
                let v3 =
                  (match v3 with
                  | Some tok -> R.Option (Some (
                      (* "," *) token env tok
                    ))
                  | None -> R.Option None)
                in
                R.Tuple [v1; v2; v3]
              ))
            | None -> R.Option None)
          in
          let v3 = (* "]" *) token env v3 in
          R.Tuple [v1; v2; v3]
        )
      | `LBRACK_expr_opt_COMMA_fors_opt_comp_RBRACK (v1, v2, v3, v4, v5, v6) -> R.Case ("LBRACK_expr_opt_COMMA_fors_opt_comp_RBRACK",
          let v1 = (* "[" *) token env v1 in
          let v2 = map_document env v2 in
          let v3 =
            (match v3 with
            | Some tok -> R.Option (Some (
                (* "," *) token env tok
              ))
            | None -> R.Option None)
          in
          let v4 = map_forspec env v4 in
          let v5 =
            (match v5 with
            | Some x -> R.Option (Some (
                map_compspec env x
              ))
            | None -> R.Option None)
          in
          let v6 = (* "]" *) token env v6 in
          R.Tuple [v1; v2; v3; v4; v5; v6]
        )
      | `Expr_DOT_id (v1, v2, v3) -> R.Case ("Expr_DOT_id",
          let v1 = map_document env v1 in
          let v2 = (* "." *) token env v2 in
          let v3 = map_id env v3 in
          R.Tuple [v1; v2; v3]
        )
      | `Super_DOT_id (v1, v2, v3) -> R.Case ("Super_DOT_id",
          let v1 = (* "super" *) token env v1 in
          let v2 = (* "." *) token env v2 in
          let v3 = map_id env v3 in
          R.Tuple [v1; v2; v3]
        )
      | `Expr_LBRACK_opt_expr_opt_COLON_opt_expr_opt_COLON_opt_expr_RBRACK (v1, v2, v3, v4, v5) -> R.Case ("Expr_LBRACK_opt_expr_opt_COLON_opt_expr_opt_COLON_opt_expr_RBRACK",
          let v1 = map_document env v1 in
          let v2 = (* "[" *) token env v2 in
          let v3 =
            (match v3 with
            | Some x -> R.Option (Some (
                map_document env x
              ))
            | None -> R.Option None)
          in
          let v4 =
            (match v4 with
            | Some (v1, v2, v3) -> R.Option (Some (
                let v1 = (* ":" *) token env v1 in
                let v2 =
                  (match v2 with
                  | Some x -> R.Option (Some (
                      map_document env x
                    ))
                  | None -> R.Option None)
                in
                let v3 =
                  (match v3 with
                  | Some (v1, v2) -> R.Option (Some (
                      let v1 = (* ":" *) token env v1 in
                      let v2 =
                        (match v2 with
                        | Some x -> R.Option (Some (
                            map_document env x
                          ))
                        | None -> R.Option None)
                      in
                      R.Tuple [v1; v2]
                    ))
                  | None -> R.Option None)
                in
                R.Tuple [v1; v2; v3]
              ))
            | None -> R.Option None)
          in
          let v5 = (* "]" *) token env v5 in
          R.Tuple [v1; v2; v3; v4; v5]
        )
      | `Super_LBRACK_expr_RBRACK (v1, v2, v3, v4) -> R.Case ("Super_LBRACK_expr_RBRACK",
          let v1 = (* "super" *) token env v1 in
          let v2 = (* "[" *) token env v2 in
          let v3 = map_document env v3 in
          let v4 = (* "]" *) token env v4 in
          R.Tuple [v1; v2; v3; v4]
        )
      | `Expr_LPAR_opt_args_RPAR_opt_tail (v1, v2, v3, v4, v5) -> R.Case ("Expr_LPAR_opt_args_RPAR_opt_tail",
          let v1 = map_document env v1 in
          let v2 = (* "(" *) token env v2 in
          let v3 =
            (match v3 with
            | Some x -> R.Option (Some (
                map_args env x
              ))
            | None -> R.Option None)
          in
          let v4 = (* ")" *) token env v4 in
          let v5 =
            (match v5 with
            | Some tok -> R.Option (Some (
                (* "tailstrict" *) token env tok
              ))
            | None -> R.Option None)
          in
          R.Tuple [v1; v2; v3; v4; v5]
        )
      | `Id x -> R.Case ("Id",
          map_id env x
        )
      | `Local_bind x -> R.Case ("Local_bind",
          map_local_bind env x
        )
      | `If_expr_then_expr_opt_else_expr (v1, v2, v3, v4, v5) -> R.Case ("If_expr_then_expr_opt_else_expr",
          let v1 = (* "if" *) token env v1 in
          let v2 = map_document env v2 in
          let v3 = (* "then" *) token env v3 in
          let v4 = map_document env v4 in
          let v5 =
            (match v5 with
            | Some (v1, v2) -> R.Option (Some (
                let v1 = (* "else" *) token env v1 in
                let v2 = map_document env v2 in
                R.Tuple [v1; v2]
              ))
            | None -> R.Option None)
          in
          R.Tuple [v1; v2; v3; v4; v5]
        )
      | `Bin_expr x -> R.Case ("Bin_expr",
          map_binary_expr env x
        )
      | `Unar_expr (v1, v2) -> R.Case ("Unar_expr",
          let v1 = map_unaryop env v1 in
          let v2 = map_document env v2 in
          R.Tuple [v1; v2]
        )
      | `Expr_LCURL_choice_member_rep_COMMA_member_opt_COMMA_RCURL (v1, v2, v3, v4) -> R.Case ("Expr_LCURL_choice_member_rep_COMMA_member_opt_COMMA_RCURL",
          let v1 = map_document env v1 in
          let v2 = (* "{" *) token env v2 in
          let v3 = map_objinside env v3 in
          let v4 = (* "}" *) token env v4 in
          R.Tuple [v1; v2; v3; v4]
        )
      | `Anon_func x -> R.Case ("Anon_func",
          map_anonymous_function env x
        )
      | `Assert_SEMI_expr (v1, v2, v3) -> R.Case ("Assert_SEMI_expr",
          let v1 = map_assert_ env v1 in
          let v2 = (* ";" *) token env v2 in
          let v3 = map_document env v3 in
          R.Tuple [v1; v2; v3]
        )
      | `Import x -> R.Case ("Import",
          map_import env x
        )
      | `Impo x -> R.Case ("Impo",
          map_importstr env x
        )
      | `Expr_error x -> R.Case ("Expr_error",
          map_expr_error env x
        )
      | `Expr_in_super (v1, v2, v3) -> R.Case ("Expr_in_super",
          let v1 = map_document env v1 in
          let v2 = (* "in" *) token env v2 in
          let v3 = (* "super" *) token env v3 in
          R.Tuple [v1; v2; v3]
        )
      | `LPAR_expr_RPAR (v1, v2, v3) -> R.Case ("LPAR_expr_RPAR",
          let v1 = (* "(" *) token env v1 in
          let v2 = map_document env v2 in
          let v3 = (* ")" *) token env v3 in
          R.Tuple [v1; v2; v3]
        )
      )
    )
  )

and map_expr_error (env : env) ((v1, v2) : CST.expr_error) =
  let v1 = (* "error" *) token env v1 in
  let v2 = map_document env v2 in
  R.Tuple [v1; v2]

and map_field (env : env) (x : CST.field) =
  (match x with
  | `Semg_ellips tok -> R.Case ("Semg_ellips",
      (* "..." *) token env tok
    )
  | `Choice_fiel_opt_PLUS_choice_COLON_expr x -> R.Case ("Choice_fiel_opt_PLUS_choice_COLON_expr",
      (match x with
      | `Fiel_opt_PLUS_choice_COLON_expr (v1, v2, v3, v4) -> R.Case ("Fiel_opt_PLUS_choice_COLON_expr",
          let v1 = map_fieldname env v1 in
          let v2 =
            (match v2 with
            | Some tok -> R.Option (Some (
                (* "+" *) token env tok
              ))
            | None -> R.Option None)
          in
          let v3 = map_h env v3 in
          let v4 = map_document env v4 in
          R.Tuple [v1; v2; v3; v4]
        )
      | `Fiel_LPAR_opt_params_RPAR_choice_COLON_expr (v1, v2, v3, v4, v5, v6) -> R.Case ("Fiel_LPAR_opt_params_RPAR_choice_COLON_expr",
          let v1 = map_fieldname env v1 in
          let v2 = (* "(" *) token env v2 in
          let v3 =
            (match v3 with
            | Some x -> R.Option (Some (
                map_params env x
              ))
            | None -> R.Option None)
          in
          let v4 = (* ")" *) token env v4 in
          let v5 = map_h env v5 in
          let v6 = map_document env v6 in
          R.Tuple [v1; v2; v3; v4; v5; v6]
        )
      )
    )
  )

and map_fieldname (env : env) (x : CST.fieldname) =
  (match x with
  | `Id x -> R.Case ("Id",
      map_id env x
    )
  | `Str x -> R.Case ("Str",
      map_string_ env x
    )
  | `LBRACK_expr_RBRACK (v1, v2, v3) -> R.Case ("LBRACK_expr_RBRACK",
      let v1 = (* "[" *) token env v1 in
      let v2 = map_document env v2 in
      let v3 = (* "]" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_forspec (env : env) ((v1, v2, v3, v4) : CST.forspec) =
  let v1 = (* "for" *) token env v1 in
  let v2 = map_id env v2 in
  let v3 = (* "in" *) token env v3 in
  let v4 = map_document env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_ifspec (env : env) ((v1, v2) : CST.ifspec) =
  let v1 = (* "if" *) token env v1 in
  let v2 = map_document env v2 in
  R.Tuple [v1; v2]

and map_local_bind (env : env) ((v1, v2, v3, v4, v5) : CST.local_bind) =
  let v1 = (* "local" *) token env v1 in
  let v2 = map_bind env v2 in
  let v3 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_bind env v2 in
      R.Tuple [v1; v2]
    ) v3)
  in
  let v4 = (* ";" *) token env v4 in
  let v5 = map_document env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

and map_member (env : env) (x : CST.member) =
  (match x with
  | `Objl x -> R.Case ("Objl",
      map_objlocal env x
    )
  | `Assert x -> R.Case ("Assert",
      map_assert_ env x
    )
  | `Field x -> R.Case ("Field",
      map_field env x
    )
  )

and map_named_argument (env : env) ((v1, v2, v3) : CST.named_argument) =
  let v1 = map_id env v1 in
  let v2 = (* "=" *) token env v2 in
  let v3 = map_document env v3 in
  R.Tuple [v1; v2; v3]

and map_objinside (env : env) (x : CST.objinside) =
  (match x with
  | `Member_rep_COMMA_member_opt_COMMA (v1, v2, v3) -> R.Case ("Member_rep_COMMA_member_opt_COMMA",
      let v1 = map_member env v1 in
      let v2 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = (* "," *) token env v1 in
          let v2 = map_member env v2 in
          R.Tuple [v1; v2]
        ) v2)
      in
      let v3 =
        (match v3 with
        | Some tok -> R.Option (Some (
            (* "," *) token env tok
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3]
    )
  | `Rep_objl_COMMA_LBRACK_expr_RBRACK_COLON_expr_rep_COMMA_objl_opt_COMMA_fors_opt_comp (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10) -> R.Case ("Rep_objl_COMMA_LBRACK_expr_RBRACK_COLON_expr_rep_COMMA_objl_opt_COMMA_fors_opt_comp",
      let v1 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = map_objlocal env v1 in
          let v2 = (* "," *) token env v2 in
          R.Tuple [v1; v2]
        ) v1)
      in
      let v2 = (* "[" *) token env v2 in
      let v3 = map_document env v3 in
      let v4 = (* "]" *) token env v4 in
      let v5 = (* ":" *) token env v5 in
      let v6 = map_document env v6 in
      let v7 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = (* "," *) token env v1 in
          let v2 = map_objlocal env v2 in
          R.Tuple [v1; v2]
        ) v7)
      in
      let v8 =
        (match v8 with
        | Some tok -> R.Option (Some (
            (* "," *) token env tok
          ))
        | None -> R.Option None)
      in
      let v9 = map_forspec env v9 in
      let v10 =
        (match v10 with
        | Some x -> R.Option (Some (
            map_compspec env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3; v4; v5; v6; v7; v8; v9; v10]
    )
  )

and map_objlocal (env : env) ((v1, v2) : CST.objlocal) =
  let v1 = (* "local" *) token env v1 in
  let v2 = map_bind env v2 in
  R.Tuple [v1; v2]

and map_param (env : env) (x : CST.param) =
  (match x with
  | `Semg_ellips tok -> R.Case ("Semg_ellips",
      (* "..." *) token env tok
    )
  | `Id_opt_EQ_expr (v1, v2) -> R.Case ("Id_opt_EQ_expr",
      let v1 = map_id env v1 in
      let v2 =
        (match v2 with
        | Some (v1, v2) -> R.Option (Some (
            let v1 = (* "=" *) token env v1 in
            let v2 = map_document env v2 in
            R.Tuple [v1; v2]
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2]
    )
  )

and map_params (env : env) ((v1, v2, v3) : CST.params) =
  let v1 = map_param env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_param env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  let v3 =
    (match v3 with
    | Some tok -> R.Option (Some (
        (* "," *) token env tok
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3]

let map_comment (env : env) (tok : CST.comment) =
  (* comment *) token env tok

let dump_tree root =
  map_document () root
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
