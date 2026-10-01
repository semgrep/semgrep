(**
   Boilerplate to be used as a template when mapping the r CST
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

let map_external_close_bracket (env : env) (tok : CST.external_close_bracket) =
  (* external_close_bracket *) token env tok

let map_external_open_parenthesis (env : env) (tok : CST.external_open_parenthesis) =
  (* external_open_parenthesis *) token env tok

let map_pat_dc28280 (env : env) (tok : CST.pat_dc28280) =
  (* pattern "[^'\\\\]+" *) token env tok

let map_raw_string_close (env : env) (tok : CST.raw_string_close) =
  (* raw_string_close *) token env tok

let map_identifier (env : env) (tok : CST.identifier) =
  (* identifier *) token env tok

let map_raw_string_open (env : env) (tok : CST.raw_string_open) =
  (* raw_string_open *) token env tok

let map_external_open_bracket (env : env) (tok : CST.external_open_bracket) =
  (* external_open_bracket *) token env tok

let map_escape_sequence (env : env) (tok : CST.escape_sequence) =
  (* escape_sequence *) token env tok

let map_external_open_bracket2 (env : env) (tok : CST.external_open_bracket2) =
  (* external_open_bracket2 *) token env tok

let map_external_else (env : env) (tok : CST.external_else) =
  (* external_else *) token env tok

let map_external_close_bracket2 (env : env) (tok : CST.external_close_bracket2) =
  (* external_close_bracket2 *) token env tok

let map_external_close_brace (env : env) (tok : CST.external_close_brace) =
  (* external_close_brace *) token env tok

let map_number_literal (env : env) (tok : CST.number_literal) =
  (* pattern (?:(?:\d+(?:\.\d*\
  )?)|(?:\.\d+))(?:[eE][+-]?\d*\
  )? *) token env tok

let map_semicolon (env : env) (tok : CST.semicolon) =
  (* semicolon *) token env tok

let map_start (env : env) (tok : CST.start) =
  (* start *) token env tok

let map_na (env : env) (x : CST.na) =
  (match x with
  | `NA tok -> R.Case ("NA",
      (* "NA" *) token env tok
    )
  | `NA_int_ tok -> R.Case ("NA_int_",
      (* "NA_integer_" *) token env tok
    )
  | `NA_real_ tok -> R.Case ("NA_real_",
      (* "NA_real_" *) token env tok
    )
  | `NA_comp_ tok -> R.Case ("NA_comp_",
      (* "NA_complex_" *) token env tok
    )
  | `NA_char_ tok -> R.Case ("NA_char_",
      (* "NA_character_" *) token env tok
    )
  )

let map_pat_43ed24e (env : env) (tok : CST.pat_43ed24e) =
  (* pattern %[^%\\\n]*% *) token env tok

let map_external_open_brace (env : env) (tok : CST.external_open_brace) =
  (* external_open_brace *) token env tok

let map_raw_string_content (env : env) (tok : CST.raw_string_content) =
  (* raw_string_content *) token env tok

let map_pat_3a2a380 (env : env) (tok : CST.pat_3a2a380) =
  (* pattern "[^\"\\\\]+" *) token env tok

let map_newline (env : env) (tok : CST.newline) =
  (* newline *) token env tok

let map_dot_dot_i (env : env) (tok : CST.dot_dot_i) =
  (* pattern [.][.]\d+ *) token env tok

let map_hex_literal (env : env) (tok : CST.hex_literal) =
  (* pattern 0[xX](([0-9a-fA-F]+(\.[0-9a-fA-F]*\
  )?)|(\.[0-9a-fA-F]*\
  ))([pP][+-]?[0-9]+)? *) token env tok

let map_external_close_parenthesis (env : env) (tok : CST.external_close_parenthesis) =
  (* external_close_parenthesis *) token env tok

let map_single_quoted_string_content (env : env) (xs : CST.single_quoted_string_content) =
  R.List (List.map (fun x ->
    (match x with
    | `Pat_dc28280 x -> R.Case ("Pat_dc28280",
        map_pat_dc28280 env x
      )
    | `Esc_seq tok -> R.Case ("Esc_seq",
        (* escape_sequence *) token env tok
      )
    )
  ) xs)

let map_double_quoted_string_content (env : env) (xs : CST.double_quoted_string_content) =
  R.List (List.map (fun x ->
    (match x with
    | `Pat_3a2a380 x -> R.Case ("Pat_3a2a380",
        map_pat_3a2a380 env x
      )
    | `Esc_seq tok -> R.Case ("Esc_seq",
        (* escape_sequence *) token env tok
      )
    )
  ) xs)

let map_parameter_name (env : env) (x : CST.parameter_name) =
  (match x with
  | `Dots tok -> R.Case ("Dots",
      (* "..." *) token env tok
    )
  | `Dot_dot_i tok -> R.Case ("Dot_dot_i",
      (* pattern [.][.]\d+ *) token env tok
    )
  | `Id tok -> R.Case ("Id",
      (* identifier *) token env tok
    )
  )

let map_float_literal (env : env) (x : CST.float_literal) =
  (match x with
  | `Hex_lit tok -> R.Case ("Hex_lit",
      (* pattern 0[xX](([0-9a-fA-F]+(\.[0-9a-fA-F]*\
  )?)|(\.[0-9a-fA-F]*\
  ))([pP][+-]?[0-9]+)? *) token env tok
    )
  | `Num_lit tok -> R.Case ("Num_lit",
      (* pattern (?:(?:\d+(?:\.\d*\
  )?)|(?:\.\d+))(?:[eE][+-]?\d*\
  )? *) token env tok
    )
  )

let map_parameter_without_default (env : env) (x : CST.parameter_without_default) =
  map_parameter_name env x

let map_float_ (env : env) (x : CST.float_) =
  map_float_literal env x

let map_string_ (env : env) (x : CST.string_) =
  (match x with
  | `Raw_str (v1, v2, v3) -> R.Case ("Raw_str",
      let v1 = (* raw_string_open *) token env v1 in
      let v2 =
        (match v2 with
        | Some tok -> R.Option (Some (
            (* raw_string_content *) token env tok
          ))
        | None -> R.Option None)
      in
      let v3 = (* raw_string_close *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Single_quoted_str (v1, v2, v3) -> R.Case ("Single_quoted_str",
      let v1 = (* "'" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_single_quoted_string_content env x
          ))
        | None -> R.Option None)
      in
      let v3 = (* "'" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Double_quoted_str (v1, v2, v3) -> R.Case ("Double_quoted_str",
      let v1 = (* "\"" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_double_quoted_string_content env x
          ))
        | None -> R.Option None)
      in
      let v3 = (* "\"" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

let map_string_or_identifier (env : env) (x : CST.string_or_identifier) =
  (match x with
  | `Str x -> R.Case ("Str",
      map_string_ env x
    )
  | `Choice_dots x -> R.Case ("Choice_dots",
      map_parameter_without_default env x
    )
  )

let map_argument_name_string_or_identifier_or_null (env : env) (x : CST.argument_name_string_or_identifier_or_null) =
  (match x with
  | `Choice_str x -> R.Case ("Choice_str",
      map_string_or_identifier env x
    )
  | `Null tok -> R.Case ("Null",
      (* "NULL" *) token env tok
    )
  )

let map_namespace_operator (env : env) (x : CST.namespace_operator) =
  (match x with
  | `Choice_str_COLONCOLON_opt_choice_str (v1, v2, v3) -> R.Case ("Choice_str_COLONCOLON_opt_choice_str",
      let v1 = map_string_or_identifier env v1 in
      let v2 = (* "::" *) token env v2 in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_string_or_identifier env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3]
    )
  | `Choice_str_COLONCOLONCOLON_opt_choice_str (v1, v2, v3) -> R.Case ("Choice_str_COLONCOLONCOLON_opt_choice_str",
      let v1 = map_string_or_identifier env v1 in
      let v2 = (* ":::" *) token env v2 in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_string_or_identifier env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3]
    )
  )

let rec map_anon_choice_arg_value_c460aa0 (env : env) (x : CST.anon_choice_arg_value_c460aa0) =
  (match x with
  | `Exp x -> R.Case ("Exp",
      map_argument_unnamed env x
    )
  | `Semi tok -> R.Case ("Semi",
      (* semicolon *) token env tok
    )
  | `Nl tok -> R.Case ("Nl",
      (* newline *) token env tok
    )
  )

and map_anon_rep_comma_opt_arg_59635e2 (env : env) (xs : CST.anon_rep_comma_opt_arg_59635e2) =
  R.List (List.map (fun (v1, v2) ->
    let v1 = (* "," *) token env v1 in
    let v2 =
      (match v2 with
      | Some x -> R.Option (Some (
          map_argument env x
        ))
      | None -> R.Option None)
    in
    R.Tuple [v1; v2]
  ) xs)

and map_argument (env : env) (x : CST.argument) =
  (match x with
  | `Arg_named (v1, v2, v3) -> R.Case ("Arg_named",
      let v1 =
        map_argument_name_string_or_identifier_or_null env v1
      in
      let v2 = (* "=" *) token env v2 in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_argument_unnamed env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3]
    )
  | `Arg_unna x -> R.Case ("Arg_unna",
      map_argument_unnamed env x
    )
  )

and map_argument_unnamed (env : env) (x : CST.argument_unnamed) =
  map_argument_value env x

and map_argument_value (env : env) (x : CST.argument_value) =
  map_expression env x

and map_binary_operator (env : env) (x : CST.binary_operator) =
  (match x with
  | `Exp_QMARK_rep_nl_exp (v1, v2, v3, v4) -> R.Case ("Exp_QMARK_rep_nl_exp",
      let v1 = map_argument_unnamed env v1 in
      let v2 = (* "?" *) token env v2 in
      let v3 = R.List (List.map (token env (* newline *)) v3) in
      let v4 = map_argument_unnamed env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Exp_TILDE_rep_nl_exp (v1, v2, v3, v4) -> R.Case ("Exp_TILDE_rep_nl_exp",
      let v1 = map_argument_unnamed env v1 in
      let v2 = (* "~" *) token env v2 in
      let v3 = R.List (List.map (token env (* newline *)) v3) in
      let v4 = map_argument_unnamed env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Exp_LTDASH_rep_nl_exp (v1, v2, v3, v4) -> R.Case ("Exp_LTDASH_rep_nl_exp",
      let v1 = map_argument_unnamed env v1 in
      let v2 = (* "<-" *) token env v2 in
      let v3 = R.List (List.map (token env (* newline *)) v3) in
      let v4 = map_argument_unnamed env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Exp_LTLTDASH_rep_nl_exp (v1, v2, v3, v4) -> R.Case ("Exp_LTLTDASH_rep_nl_exp",
      let v1 = map_argument_unnamed env v1 in
      let v2 = (* "<<-" *) token env v2 in
      let v3 = R.List (List.map (token env (* newline *)) v3) in
      let v4 = map_argument_unnamed env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Exp_COLONEQ_rep_nl_exp (v1, v2, v3, v4) -> R.Case ("Exp_COLONEQ_rep_nl_exp",
      let v1 = map_argument_unnamed env v1 in
      let v2 = (* ":=" *) token env v2 in
      let v3 = R.List (List.map (token env (* newline *)) v3) in
      let v4 = map_argument_unnamed env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Exp_DASHGT_rep_nl_exp (v1, v2, v3, v4) -> R.Case ("Exp_DASHGT_rep_nl_exp",
      let v1 = map_argument_unnamed env v1 in
      let v2 = (* "->" *) token env v2 in
      let v3 = R.List (List.map (token env (* newline *)) v3) in
      let v4 = map_argument_unnamed env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Exp_DASHGTGT_rep_nl_exp (v1, v2, v3, v4) -> R.Case ("Exp_DASHGTGT_rep_nl_exp",
      let v1 = map_argument_unnamed env v1 in
      let v2 = (* "->>" *) token env v2 in
      let v3 = R.List (List.map (token env (* newline *)) v3) in
      let v4 = map_argument_unnamed env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Exp_EQ_rep_nl_exp (v1, v2, v3, v4) -> R.Case ("Exp_EQ_rep_nl_exp",
      let v1 = map_argument_unnamed env v1 in
      let v2 = (* "=" *) token env v2 in
      let v3 = R.List (List.map (token env (* newline *)) v3) in
      let v4 = map_argument_unnamed env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Exp_BAR_rep_nl_exp (v1, v2, v3, v4) -> R.Case ("Exp_BAR_rep_nl_exp",
      let v1 = map_argument_unnamed env v1 in
      let v2 = (* "|" *) token env v2 in
      let v3 = R.List (List.map (token env (* newline *)) v3) in
      let v4 = map_argument_unnamed env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Exp_AMP_rep_nl_exp (v1, v2, v3, v4) -> R.Case ("Exp_AMP_rep_nl_exp",
      let v1 = map_argument_unnamed env v1 in
      let v2 = (* "&" *) token env v2 in
      let v3 = R.List (List.map (token env (* newline *)) v3) in
      let v4 = map_argument_unnamed env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Exp_BARBAR_rep_nl_exp (v1, v2, v3, v4) -> R.Case ("Exp_BARBAR_rep_nl_exp",
      let v1 = map_argument_unnamed env v1 in
      let v2 = (* "||" *) token env v2 in
      let v3 = R.List (List.map (token env (* newline *)) v3) in
      let v4 = map_argument_unnamed env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Exp_AMPAMP_rep_nl_exp (v1, v2, v3, v4) -> R.Case ("Exp_AMPAMP_rep_nl_exp",
      let v1 = map_argument_unnamed env v1 in
      let v2 = (* "&&" *) token env v2 in
      let v3 = R.List (List.map (token env (* newline *)) v3) in
      let v4 = map_argument_unnamed env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Exp_LT_rep_nl_exp (v1, v2, v3, v4) -> R.Case ("Exp_LT_rep_nl_exp",
      let v1 = map_argument_unnamed env v1 in
      let v2 = (* "<" *) token env v2 in
      let v3 = R.List (List.map (token env (* newline *)) v3) in
      let v4 = map_argument_unnamed env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Exp_LTEQ_rep_nl_exp (v1, v2, v3, v4) -> R.Case ("Exp_LTEQ_rep_nl_exp",
      let v1 = map_argument_unnamed env v1 in
      let v2 = (* "<=" *) token env v2 in
      let v3 = R.List (List.map (token env (* newline *)) v3) in
      let v4 = map_argument_unnamed env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Exp_GT_rep_nl_exp (v1, v2, v3, v4) -> R.Case ("Exp_GT_rep_nl_exp",
      let v1 = map_argument_unnamed env v1 in
      let v2 = (* ">" *) token env v2 in
      let v3 = R.List (List.map (token env (* newline *)) v3) in
      let v4 = map_argument_unnamed env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Exp_GTEQ_rep_nl_exp (v1, v2, v3, v4) -> R.Case ("Exp_GTEQ_rep_nl_exp",
      let v1 = map_argument_unnamed env v1 in
      let v2 = (* ">=" *) token env v2 in
      let v3 = R.List (List.map (token env (* newline *)) v3) in
      let v4 = map_argument_unnamed env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Exp_EQEQ_rep_nl_exp (v1, v2, v3, v4) -> R.Case ("Exp_EQEQ_rep_nl_exp",
      let v1 = map_argument_unnamed env v1 in
      let v2 = (* "==" *) token env v2 in
      let v3 = R.List (List.map (token env (* newline *)) v3) in
      let v4 = map_argument_unnamed env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Exp_BANGEQ_rep_nl_exp (v1, v2, v3, v4) -> R.Case ("Exp_BANGEQ_rep_nl_exp",
      let v1 = map_argument_unnamed env v1 in
      let v2 = (* "!=" *) token env v2 in
      let v3 = R.List (List.map (token env (* newline *)) v3) in
      let v4 = map_argument_unnamed env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Exp_PLUS_rep_nl_exp (v1, v2, v3, v4) -> R.Case ("Exp_PLUS_rep_nl_exp",
      let v1 = map_argument_unnamed env v1 in
      let v2 = (* "+" *) token env v2 in
      let v3 = R.List (List.map (token env (* newline *)) v3) in
      let v4 = map_argument_unnamed env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Exp_DASH_rep_nl_exp (v1, v2, v3, v4) -> R.Case ("Exp_DASH_rep_nl_exp",
      let v1 = map_argument_unnamed env v1 in
      let v2 = (* "-" *) token env v2 in
      let v3 = R.List (List.map (token env (* newline *)) v3) in
      let v4 = map_argument_unnamed env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Exp_STAR_rep_nl_exp (v1, v2, v3, v4) -> R.Case ("Exp_STAR_rep_nl_exp",
      let v1 = map_argument_unnamed env v1 in
      let v2 = (* "*" *) token env v2 in
      let v3 = R.List (List.map (token env (* newline *)) v3) in
      let v4 = map_argument_unnamed env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Exp_SLASH_rep_nl_exp (v1, v2, v3, v4) -> R.Case ("Exp_SLASH_rep_nl_exp",
      let v1 = map_argument_unnamed env v1 in
      let v2 = (* "/" *) token env v2 in
      let v3 = R.List (List.map (token env (* newline *)) v3) in
      let v4 = map_argument_unnamed env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Exp_STARSTAR_rep_nl_exp (v1, v2, v3, v4) -> R.Case ("Exp_STARSTAR_rep_nl_exp",
      let v1 = map_argument_unnamed env v1 in
      let v2 = (* "**" *) token env v2 in
      let v3 = R.List (List.map (token env (* newline *)) v3) in
      let v4 = map_argument_unnamed env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Exp_HAT_rep_nl_exp (v1, v2, v3, v4) -> R.Case ("Exp_HAT_rep_nl_exp",
      let v1 = map_argument_unnamed env v1 in
      let v2 = (* "^" *) token env v2 in
      let v3 = R.List (List.map (token env (* newline *)) v3) in
      let v4 = map_argument_unnamed env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Exp_pat_43ed24e_rep_nl_exp (v1, v2, v3, v4) -> R.Case ("Exp_pat_43ed24e_rep_nl_exp",
      let v1 = map_argument_unnamed env v1 in
      let v2 = map_pat_43ed24e env v2 in
      let v3 = R.List (List.map (token env (* newline *)) v3) in
      let v4 = map_argument_unnamed env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Exp_BARGT_rep_nl_exp (v1, v2, v3, v4) -> R.Case ("Exp_BARGT_rep_nl_exp",
      let v1 = map_argument_unnamed env v1 in
      let v2 = (* "|>" *) token env v2 in
      let v3 = R.List (List.map (token env (* newline *)) v3) in
      let v4 = map_argument_unnamed env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Exp_COLON_rep_nl_exp (v1, v2, v3, v4) -> R.Case ("Exp_COLON_rep_nl_exp",
      let v1 = map_argument_unnamed env v1 in
      let v2 = (* ":" *) token env v2 in
      let v3 = R.List (List.map (token env (* newline *)) v3) in
      let v4 = map_argument_unnamed env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  )

and map_call_arguments (env : env) ((v1, v2, v3, v4) : CST.call_arguments) =
  let v1 = (* external_open_parenthesis *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_argument env x
      ))
    | None -> R.Option None)
  in
  let v3 = map_anon_rep_comma_opt_arg_59635e2 env v3 in
  let v4 = (* external_close_parenthesis *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_expression (env : env) (x : CST.expression) =
  (match x with
  | `Func_defi (v1, v2, v3, v4, v5) -> R.Case ("Func_defi",
      let v1 =
        (match v1 with
        | `BSLASH tok -> R.Case ("BSLASH",
            (* "\\" *) token env tok
          )
        | `Func tok -> R.Case ("Func",
            (* "function" *) token env tok
          )
        )
      in
      let v2 = R.List (List.map (token env (* newline *)) v2) in
      let v3 = map_parameters env v3 in
      let v4 = R.List (List.map (token env (* newline *)) v4) in
      let v5 = map_argument_unnamed env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `If_stmt (v1, v2, v3, v4, v5, v6, v7, v8) -> R.Case ("If_stmt",
      let v1 = (* "if" *) token env v1 in
      let v2 = R.List (List.map (token env (* newline *)) v2) in
      let v3 = (* external_open_parenthesis *) token env v3 in
      let v4 = map_argument_unnamed env v4 in
      let v5 = (* external_close_parenthesis *) token env v5 in
      let v6 = R.List (List.map (token env (* newline *)) v6) in
      let v7 = map_argument_unnamed env v7 in
      let v8 =
        (match v8 with
        | Some (v1, v2, v3) -> R.Option (Some (
            let v1 = (* external_else *) token env v1 in
            let v2 = R.List (List.map (token env (* newline *)) v2) in
            let v3 = map_argument_unnamed env v3 in
            R.Tuple [v1; v2; v3]
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3; v4; v5; v6; v7; v8]
    )
  | `For_stmt (v1, v2, v3, v4, v5, v6, v7, v8, v9) -> R.Case ("For_stmt",
      let v1 = (* "for" *) token env v1 in
      let v2 = R.List (List.map (token env (* newline *)) v2) in
      let v3 = (* external_open_parenthesis *) token env v3 in
      let v4 = map_parameter_without_default env v4 in
      let v5 = (* "in" *) token env v5 in
      let v6 = map_argument_unnamed env v6 in
      let v7 = (* external_close_parenthesis *) token env v7 in
      let v8 = R.List (List.map (token env (* newline *)) v8) in
      let v9 = map_argument_unnamed env v9 in
      R.Tuple [v1; v2; v3; v4; v5; v6; v7; v8; v9]
    )
  | `While_stmt (v1, v2, v3, v4, v5, v6, v7) -> R.Case ("While_stmt",
      let v1 = (* "while" *) token env v1 in
      let v2 = R.List (List.map (token env (* newline *)) v2) in
      let v3 = (* external_open_parenthesis *) token env v3 in
      let v4 = map_argument_unnamed env v4 in
      let v5 = (* external_close_parenthesis *) token env v5 in
      let v6 = R.List (List.map (token env (* newline *)) v6) in
      let v7 = map_argument_unnamed env v7 in
      R.Tuple [v1; v2; v3; v4; v5; v6; v7]
    )
  | `Repeat_stmt (v1, v2, v3) -> R.Case ("Repeat_stmt",
      let v1 = (* "repeat" *) token env v1 in
      let v2 = R.List (List.map (token env (* newline *)) v2) in
      let v3 = map_argument_unnamed env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Braced_exp (v1, v2, v3) -> R.Case ("Braced_exp",
      let v1 = (* external_open_brace *) token env v1 in
      let v2 =
        R.List (List.map (map_anon_choice_arg_value_c460aa0 env) v2)
      in
      let v3 = (* external_close_brace *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Paren_exp (v1, v2, v3) -> R.Case ("Paren_exp",
      let v1 = (* external_open_parenthesis *) token env v1 in
      let v2 = map_argument_unnamed env v2 in
      let v3 = (* external_close_parenthesis *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Call (v1, v2) -> R.Case ("Call",
      let v1 = map_argument_unnamed env v1 in
      let v2 = map_call_arguments env v2 in
      R.Tuple [v1; v2]
    )
  | `Subset (v1, v2) -> R.Case ("Subset",
      let v1 = map_argument_unnamed env v1 in
      let v2 = map_subset_arguments env v2 in
      R.Tuple [v1; v2]
    )
  | `Subset2 (v1, v2) -> R.Case ("Subset2",
      let v1 = map_argument_unnamed env v1 in
      let v2 = map_subset2_arguments env v2 in
      R.Tuple [v1; v2]
    )
  | `Un_op x -> R.Case ("Un_op",
      map_unary_operator env x
    )
  | `Bin_op x -> R.Case ("Bin_op",
      map_binary_operator env x
    )
  | `Extr_op x -> R.Case ("Extr_op",
      map_extract_operator env x
    )
  | `Name_op x -> R.Case ("Name_op",
      map_namespace_operator env x
    )
  | `Int (v1, v2) -> R.Case ("Int",
      let v1 = map_float_ env v1 in
      let v2 = (* "L" *) token env v2 in
      R.Tuple [v1; v2]
    )
  | `Comp (v1, v2) -> R.Case ("Comp",
      let v1 = map_float_ env v1 in
      let v2 = (* "i" *) token env v2 in
      R.Tuple [v1; v2]
    )
  | `Float x -> R.Case ("Float",
      map_float_ env x
    )
  | `Str x -> R.Case ("Str",
      map_string_ env x
    )
  | `Id tok -> R.Case ("Id",
      (* identifier *) token env tok
    )
  | `Dots tok -> R.Case ("Dots",
      (* "..." *) token env tok
    )
  | `Dot_dot_i tok -> R.Case ("Dot_dot_i",
      (* pattern [.][.]\d+ *) token env tok
    )
  | `Next tok -> R.Case ("Next",
      (* "next" *) token env tok
    )
  | `Brk tok -> R.Case ("Brk",
      (* "break" *) token env tok
    )
  | `True tok -> R.Case ("True",
      (* "TRUE" *) token env tok
    )
  | `False tok -> R.Case ("False",
      (* "FALSE" *) token env tok
    )
  | `Null tok -> R.Case ("Null",
      (* "NULL" *) token env tok
    )
  | `Inf tok -> R.Case ("Inf",
      (* "Inf" *) token env tok
    )
  | `Nan tok -> R.Case ("Nan",
      (* "NaN" *) token env tok
    )
  | `Na x -> R.Case ("Na",
      map_na env x
    )
  )

and map_extract_operator (env : env) (x : CST.extract_operator) =
  (match x with
  | `Exp_DOLLAR_rep_nl_opt_choice_str (v1, v2, v3, v4) -> R.Case ("Exp_DOLLAR_rep_nl_opt_choice_str",
      let v1 = map_argument_unnamed env v1 in
      let v2 = (* "$" *) token env v2 in
      let v3 = R.List (List.map (token env (* newline *)) v3) in
      let v4 =
        (match v4 with
        | Some x -> R.Option (Some (
            map_string_or_identifier env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Exp_AT_rep_nl_opt_choice_str (v1, v2, v3, v4) -> R.Case ("Exp_AT_rep_nl_opt_choice_str",
      let v1 = map_argument_unnamed env v1 in
      let v2 = (* "@" *) token env v2 in
      let v3 = R.List (List.map (token env (* newline *)) v3) in
      let v4 =
        (match v4 with
        | Some x -> R.Option (Some (
            map_string_or_identifier env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3; v4]
    )
  )

and map_parameter (env : env) (x : CST.parameter) =
  (match x with
  | `Param_with_defa_d9d11f1 (v1, v2, v3) -> R.Case ("Param_with_defa_d9d11f1",
      let v1 = map_parameter_without_default env v1 in
      let v2 = (* "=" *) token env v2 in
      let v3 = map_argument_unnamed env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Param_with_defa_6e24c8f x -> R.Case ("Param_with_defa_6e24c8f",
      map_parameter_without_default env x
    )
  )

and map_parameters (env : env) ((v1, v2, v3) : CST.parameters) =
  let v1 = (* external_open_parenthesis *) token env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = map_parameter env v1 in
        let v2 =
          R.List (List.map (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_parameter env v2 in
            R.Tuple [v1; v2]
          ) v2)
        in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  let v3 = (* external_close_parenthesis *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_subset2_arguments (env : env) ((v1, v2, v3, v4) : CST.subset2_arguments) =
  let v1 = (* external_open_bracket2 *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_argument env x
      ))
    | None -> R.Option None)
  in
  let v3 = map_anon_rep_comma_opt_arg_59635e2 env v3 in
  let v4 = (* external_close_bracket2 *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_subset_arguments (env : env) ((v1, v2, v3, v4) : CST.subset_arguments) =
  let v1 = (* external_open_bracket *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_argument env x
      ))
    | None -> R.Option None)
  in
  let v3 = map_anon_rep_comma_opt_arg_59635e2 env v3 in
  let v4 = (* external_close_bracket *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_unary_operator (env : env) (x : CST.unary_operator) =
  (match x with
  | `QMARK_rep_nl_exp (v1, v2, v3) -> R.Case ("QMARK_rep_nl_exp",
      let v1 = (* "?" *) token env v1 in
      let v2 = R.List (List.map (token env (* newline *)) v2) in
      let v3 = map_argument_unnamed env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `TILDE_rep_nl_exp (v1, v2, v3) -> R.Case ("TILDE_rep_nl_exp",
      let v1 = (* "~" *) token env v1 in
      let v2 = R.List (List.map (token env (* newline *)) v2) in
      let v3 = map_argument_unnamed env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `BANG_rep_nl_exp (v1, v2, v3) -> R.Case ("BANG_rep_nl_exp",
      let v1 = (* "!" *) token env v1 in
      let v2 = R.List (List.map (token env (* newline *)) v2) in
      let v3 = map_argument_unnamed env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `PLUS_rep_nl_exp (v1, v2, v3) -> R.Case ("PLUS_rep_nl_exp",
      let v1 = (* "+" *) token env v1 in
      let v2 = R.List (List.map (token env (* newline *)) v2) in
      let v3 = map_argument_unnamed env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `DASH_rep_nl_exp (v1, v2, v3) -> R.Case ("DASH_rep_nl_exp",
      let v1 = (* "-" *) token env v1 in
      let v2 = R.List (List.map (token env (* newline *)) v2) in
      let v3 = map_argument_unnamed env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

let map_program (env : env) ((v1, v2) : CST.program) =
  let v1 = (* start *) token env v1 in
  let v2 =
    R.List (List.map (map_anon_choice_arg_value_c460aa0 env) v2)
  in
  R.Tuple [v1; v2]

let map_comment (env : env) (tok : CST.comment) =
  (* comment *) token env tok

let dump_tree root =
  map_program () root
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
