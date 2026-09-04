(**
   Boilerplate to be used as a template when mapping the bash CST
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
  (* pattern \$[A-Z_][A-Z_0-9]* *) token env tok

let map_string_content (env : env) (tok : CST.string_content) =
  (* string_content *) token env tok

let map_empty_value (env : env) (tok : CST.empty_value) =
  (* empty_value *) token env tok

let map_word (env : env) (tok : CST.word) =
  (* word *) token env tok

let map_raw_string (env : env) (tok : CST.raw_string) =
  (* pattern "'[^']*'" *) token env tok

let map_regex (env : env) (tok : CST.regex) =
  (* regex *) token env tok

let map_heredoc_body_end (env : env) (tok : CST.heredoc_body_end) =
  (* heredoc_body_end *) token env tok

let map_ansii_c_string (env : env) (tok : CST.ansii_c_string) =
  (* pattern "\\$'([^']|\\\\')*'" *) token env tok

let map_special_character (env : env) (tok : CST.special_character) =
  (* special_character *) token env tok

let map_concat (env : env) (tok : CST.concat) =
  (* concat *) token env tok

let map_semgrep_metavar_eq (env : env) (tok : CST.semgrep_metavar_eq) =
  (* pattern \$[A-Z_][A-Z_0-9]*= *) token env tok

let map_special_variable_name (env : env) (x : CST.special_variable_name) =
  (match x with
  | `STAR tok -> R.Case ("STAR",
      (* "*" *) token env tok
    )
  | `AT tok -> R.Case ("AT",
      (* "@" *) token env tok
    )
  | `QMARK tok -> R.Case ("QMARK",
      (* "?" *) token env tok
    )
  | `DASH tok -> R.Case ("DASH",
      (* "-" *) token env tok
    )
  | `DOLLAR tok -> R.Case ("DOLLAR",
      (* "$" *) token env tok
    )
  | `X_0 tok -> R.Case ("X_0",
      (* "0" *) token env tok
    )
  | `X__ tok -> R.Case ("X__",
      (* "_" *) token env tok
    )
  )

let map_simple_heredoc_body (env : env) (tok : CST.simple_heredoc_body) =
  (* simple_heredoc_body *) token env tok

let map_terminator (env : env) (x : CST.terminator) =
  (match x with
  | `SEMI tok -> R.Case ("SEMI",
      (* ";" *) token env tok
    )
  | `SEMISEMI tok -> R.Case ("SEMISEMI",
      (* ";;" *) token env tok
    )
  | `LF tok -> R.Case ("LF",
      (* "\n" *) token env tok
    )
  | `AMP tok -> R.Case ("AMP",
      (* "&" *) token env tok
    )
  )

let map_heredoc_start (env : env) (tok : CST.heredoc_start) =
  (* heredoc_start *) token env tok

let map_heredoc_body_middle (env : env) (tok : CST.heredoc_body_middle) =
  (* heredoc_body_middle *) token env tok

let map_pat_42e353e (env : env) (tok : CST.pat_42e353e) =
  (* pattern \w+ *) token env tok

let map_semgrep_named_ellipsis (env : env) (tok : CST.semgrep_named_ellipsis) =
  (* pattern \$\.\.\.[A-Z_][A-Z_0-9]* *) token env tok

let map_semgrep_metavar_pluseq (env : env) (tok : CST.semgrep_metavar_pluseq) =
  (* pattern \$[A-Z_][A-Z_0-9]*\+= *) token env tok

let map_file_descriptor (env : env) (tok : CST.file_descriptor) =
  (* file_descriptor *) token env tok

let map_test_operator (env : env) (tok : CST.test_operator) =
  (* test_operator *) token env tok

let map_tok_prec_p1_slash (env : env) (tok : CST.tok_prec_p1_slash) =
  (* tok_prec_p1_slash *) token env tok

let map_heredoc_body_beginning (env : env) (tok : CST.heredoc_body_beginning) =
  (* heredoc_body_beginning *) token env tok

let map_variable_name (env : env) (tok : CST.variable_name) =
  (* variable_name *) token env tok

let map_extended_word (env : env) (x : CST.extended_word) =
  (match x with
  | `Semg_meta tok -> R.Case ("Semg_meta",
      (* pattern \$[A-Z_][A-Z_0-9]* *) token env tok
    )
  | `Word tok -> R.Case ("Word",
      (* word *) token env tok
    )
  )

let map_heredoc_redirect (env : env) ((v1, v2) : CST.heredoc_redirect) =
  let v1 =
    (match v1 with
    | `LTLT tok -> R.Case ("LTLT",
        (* "<<" *) token env tok
      )
    | `LTLTDASH tok -> R.Case ("LTLTDASH",
        (* "<<-" *) token env tok
      )
    )
  in
  let v2 = (* heredoc_start *) token env v2 in
  R.Tuple [v1; v2]

let map_orig_simple_variable_name (env : env) (x : CST.orig_simple_variable_name) =
  map_pat_42e353e env x

let map_simple_variable_name (env : env) (x : CST.simple_variable_name) =
  (match x with
  | `Semg_meta tok -> R.Case ("Semg_meta",
      (* pattern \$[A-Z_][A-Z_0-9]* *) token env tok
    )
  | `Pat_42e353e x -> R.Case ("Pat_42e353e",
      map_orig_simple_variable_name env x
    )
  )

let map_simple_expansion (env : env) (x : CST.simple_expansion) =
  (match x with
  | `DOLLAR_choice_orig_simple_var_name (v1, v2) -> R.Case ("DOLLAR_choice_orig_simple_var_name",
      let v1 = (* "$" *) token env v1 in
      let v2 =
        (match v2 with
        | `Orig_simple_var_name x -> R.Case ("Orig_simple_var_name",
            map_orig_simple_variable_name env x
          )
        | `Choice_STAR x -> R.Case ("Choice_STAR",
            map_special_variable_name env x
          )
        | `BANG tok -> R.Case ("BANG",
            (* "!" *) token env tok
          )
        | `HASH tok -> R.Case ("HASH",
            (* "#" *) token env tok
          )
        )
      in
      R.Tuple [v1; v2]
    )
  | `Semg_named_ellips tok -> R.Case ("Semg_named_ellips",
      (* pattern \$\.\.\.[A-Z_][A-Z_0-9]* *) token env tok
    )
  )

let rec map_anon_choice_lit_bbf16c7 (env : env) (x : CST.anon_choice_lit_bbf16c7) =
  (match x with
  | `Choice_conc x -> R.Case ("Choice_conc",
      map_literal env x
    )
  | `Array (v1, v2, v3) -> R.Case ("Array",
      let v1 = (* "(" *) token env v1 in
      let v2 = R.List (List.map (map_literal env) v2) in
      let v3 = (* ")" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Empty_value tok -> R.Case ("Empty_value",
      (* empty_value *) token env tok
    )
  )

and map_anon_choice_prim_exp_65e2c2e (env : env) (x : CST.anon_choice_prim_exp_65e2c2e) =
  (match x with
  | `Choice_semg_deep_exp x -> R.Case ("Choice_semg_deep_exp",
      map_primary_expression env x
    )
  | `Spec_char tok -> R.Case ("Spec_char",
      (* special_character *) token env tok
    )
  )

and map_anon_stmt_opt_LF_here_body_term_3efa649 (env : env) ((v1, v2, v3) : CST.anon_stmt_opt_LF_here_body_term_3efa649) =
  let v1 = map_statement env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = (* "\n" *) token env v1 in
        let v2 = map_heredoc_body env v2 in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  let v3 = map_terminator env v3 in
  R.Tuple [v1; v2; v3]

and map_binary_expression (env : env) (x : CST.binary_expression) =
  (match x with
  | `Exp_choice_EQ_exp (v1, v2, v3) -> R.Case ("Exp_choice_EQ_exp",
      let v1 = map_expression env v1 in
      let v2 =
        (match v2 with
        | `EQ tok -> R.Case ("EQ",
            (* "=" *) token env tok
          )
        | `EQEQ tok -> R.Case ("EQEQ",
            (* "==" *) token env tok
          )
        | `EQTILDE tok -> R.Case ("EQTILDE",
            (* "=~" *) token env tok
          )
        | `BANGEQ tok -> R.Case ("BANGEQ",
            (* "!=" *) token env tok
          )
        | `PLUS tok -> R.Case ("PLUS",
            (* "+" *) token env tok
          )
        | `DASH tok -> R.Case ("DASH",
            (* "-" *) token env tok
          )
        | `PLUSEQ tok -> R.Case ("PLUSEQ",
            (* "+=" *) token env tok
          )
        | `DASHEQ tok -> R.Case ("DASHEQ",
            (* "-=" *) token env tok
          )
        | `LT tok -> R.Case ("LT",
            (* "<" *) token env tok
          )
        | `GT tok -> R.Case ("GT",
            (* ">" *) token env tok
          )
        | `LTEQ tok -> R.Case ("LTEQ",
            (* "<=" *) token env tok
          )
        | `GTEQ tok -> R.Case ("GTEQ",
            (* ">=" *) token env tok
          )
        | `BARBAR tok -> R.Case ("BARBAR",
            (* "||" *) token env tok
          )
        | `AMPAMP tok -> R.Case ("AMPAMP",
            (* "&&" *) token env tok
          )
        | `Test_op tok -> R.Case ("Test_op",
            (* test_operator *) token env tok
          )
        )
      in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_choice_EQEQ_regex (v1, v2, v3) -> R.Case ("Exp_choice_EQEQ_regex",
      let v1 = map_expression env v1 in
      let v2 =
        (match v2 with
        | `EQEQ tok -> R.Case ("EQEQ",
            (* "==" *) token env tok
          )
        | `EQTILDE tok -> R.Case ("EQTILDE",
            (* "=~" *) token env tok
          )
        )
      in
      let v3 = (* regex *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_case_item (env : env) ((v1, v2, v3, v4, v5) : CST.case_item) =
  let v1 = map_literal env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "|" *) token env v1 in
      let v2 = map_literal env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  let v3 = (* ")" *) token env v3 in
  let v4 = map_program env v4 in
  let v5 =
    (match v5 with
    | `SEMISEMI tok -> R.Case ("SEMISEMI",
        (* ";;" *) token env tok
      )
    | `Choice_SEMIAMP x -> R.Case ("Choice_SEMIAMP",
        (match x with
        | `SEMIAMP tok -> R.Case ("SEMIAMP",
            (* ";&" *) token env tok
          )
        | `SEMISEMIAMP tok -> R.Case ("SEMISEMIAMP",
            (* ";;&" *) token env tok
          )
        )
      )
    )
  in
  R.Tuple [v1; v2; v3; v4; v5]

and map_command (env : env) ((v1, v2, v3) : CST.command) =
  let v1 =
    R.List (List.map (fun x ->
      (match x with
      | `Var_assign x -> R.Case ("Var_assign",
          map_variable_assignment env x
        )
      | `File_redi x -> R.Case ("File_redi",
          map_file_redirect env x
        )
      )
    ) v1)
  in
  let v2 = map_command_name env v2 in
  let v3 =
    R.List (List.map (fun x ->
      (match x with
      | `Choice_conc x -> R.Case ("Choice_conc",
          map_literal env x
        )
      | `Choice_EQTILDE_choice_choice_conc (v1, v2) -> R.Case ("Choice_EQTILDE_choice_choice_conc",
          let v1 =
            (match v1 with
            | `EQTILDE tok -> R.Case ("EQTILDE",
                (* "=~" *) token env tok
              )
            | `EQEQ tok -> R.Case ("EQEQ",
                (* "==" *) token env tok
              )
            )
          in
          let v2 =
            (match v2 with
            | `Choice_conc x -> R.Case ("Choice_conc",
                map_literal env x
              )
            | `Regex tok -> R.Case ("Regex",
                (* regex *) token env tok
              )
            )
          in
          R.Tuple [v1; v2]
        )
      )
    ) v3)
  in
  R.Tuple [v1; v2; v3]

and map_command_name (env : env) (x : CST.command_name) =
  (match x with
  | `Conc x -> R.Case ("Conc",
      map_concatenation env x
    )
  | `Choice_semg_deep_exp x -> R.Case ("Choice_semg_deep_exp",
      map_primary_expression env x
    )
  | `Rep1_spec_char xs -> R.Case ("Rep1_spec_char",
      R.List (List.map (token env (* special_character *)) xs)
    )
  )

and map_command_substitution (env : env) (x : CST.command_substitution) =
  (match x with
  | `DOLLARLPAR_stmts_RPAR (v1, v2, v3) -> R.Case ("DOLLARLPAR_stmts_RPAR",
      let v1 = (* "$(" *) token env v1 in
      let v2 = map_statements env v2 in
      let v3 = (* ")" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `DOLLARLPAR_file_redi_RPAR (v1, v2, v3) -> R.Case ("DOLLARLPAR_file_redi_RPAR",
      let v1 = (* "$(" *) token env v1 in
      let v2 = map_file_redirect env v2 in
      let v3 = (* ")" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `BQUOT_stmts_BQUOT (v1, v2, v3) -> R.Case ("BQUOT_stmts_BQUOT",
      let v1 = (* "`" *) token env v1 in
      let v2 = map_statements env v2 in
      let v3 = (* "`" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_compound_statement (env : env) ((v1, v2, v3) : CST.compound_statement) =
  let v1 = (* "{" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_statements2 env x
      ))
    | None -> R.Option None)
  in
  let v3 = (* "}" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_concatenation (env : env) ((v1, v2, v3) : CST.concatenation) =
  let v1 = map_anon_choice_prim_exp_65e2c2e env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* concat *) token env v1 in
      let v2 = map_anon_choice_prim_exp_65e2c2e env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  let v3 =
    (match v3 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = (* concat *) token env v1 in
        let v2 = (* "$" *) token env v2 in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3]

and map_do_group (env : env) ((v1, v2, v3) : CST.do_group) =
  let v1 = (* "do" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_statements2 env x
      ))
    | None -> R.Option None)
  in
  let v3 = (* "done" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_elif_clause (env : env) ((v1, v2, v3, v4) : CST.elif_clause) =
  let v1 = (* "elif" *) token env v1 in
  let v2 = map_terminated_statement env v2 in
  let v3 = (* "then" *) token env v3 in
  let v4 =
    (match v4 with
    | Some x -> R.Option (Some (
        map_statements2 env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3; v4]

and map_else_clause (env : env) ((v1, v2) : CST.else_clause) =
  let v1 = (* "else" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_statements2 env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2]

and map_expansion (env : env) ((v1, v2, v3, v4) : CST.expansion) =
  let v1 = (* "${" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        (match x with
        | `HASH tok -> R.Case ("HASH",
            (* "#" *) token env tok
          )
        | `BANG tok -> R.Case ("BANG",
            (* "!" *) token env tok
          )
        )
      ))
    | None -> R.Option None)
  in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        (match x with
        | `Var_name_EQ_opt_choice_conc (v1, v2, v3) -> R.Case ("Var_name_EQ_opt_choice_conc",
            let v1 = (* variable_name *) token env v1 in
            let v2 = (* "=" *) token env v2 in
            let v3 =
              (match v3 with
              | Some x -> R.Option (Some (
                  map_literal env x
                ))
              | None -> R.Option None)
            in
            R.Tuple [v1; v2; v3]
          )
        | `Choice_subs_opt_tok_prec_p1_slash_opt_regex_rep_choice_choice_conc (v1, v2, v3) -> R.Case ("Choice_subs_opt_tok_prec_p1_slash_opt_regex_rep_choice_choice_conc",
            let v1 =
              (match v1 with
              | `Subs x -> R.Case ("Subs",
                  map_subscript env x
                )
              | `Choice_semg_meta x -> R.Case ("Choice_semg_meta",
                  map_simple_variable_name env x
                )
              | `Choice_STAR x -> R.Case ("Choice_STAR",
                  map_special_variable_name env x
                )
              )
            in
            let v2 =
              (match v2 with
              | Some (v1, v2) -> R.Option (Some (
                  let v1 = map_tok_prec_p1_slash env v1 in
                  let v2 =
                    (match v2 with
                    | Some tok -> R.Option (Some (
                        (* regex *) token env tok
                      ))
                    | None -> R.Option None)
                  in
                  R.Tuple [v1; v2]
                ))
              | None -> R.Option None)
            in
            let v3 =
              R.List (List.map (fun x ->
                (match x with
                | `Choice_conc x -> R.Case ("Choice_conc",
                    map_literal env x
                  )
                | `COLON tok -> R.Case ("COLON",
                    (* ":" *) token env tok
                  )
                | `COLONQMARK tok -> R.Case ("COLONQMARK",
                    (* ":?" *) token env tok
                  )
                | `EQ tok -> R.Case ("EQ",
                    (* "=" *) token env tok
                  )
                | `COLONDASH tok -> R.Case ("COLONDASH",
                    (* ":-" *) token env tok
                  )
                | `PERC tok -> R.Case ("PERC",
                    (* "%" *) token env tok
                  )
                | `DASH tok -> R.Case ("DASH",
                    (* "-" *) token env tok
                  )
                | `HASH tok -> R.Case ("HASH",
                    (* "#" *) token env tok
                  )
                )
              ) v3)
            in
            R.Tuple [v1; v2; v3]
          )
        )
      ))
    | None -> R.Option None)
  in
  let v4 = (* "}" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_expression (env : env) (x : CST.expression) =
  (match x with
  | `Choice_conc x -> R.Case ("Choice_conc",
      map_literal env x
    )
  | `Un_exp (v1, v2) -> R.Case ("Un_exp",
      let v1 =
        (match v1 with
        | `BANG tok -> R.Case ("BANG",
            (* "!" *) token env tok
          )
        | `Test_op tok -> R.Case ("Test_op",
            (* test_operator *) token env tok
          )
        )
      in
      let v2 = map_expression env v2 in
      R.Tuple [v1; v2]
    )
  | `Tern_exp (v1, v2, v3, v4, v5) -> R.Case ("Tern_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "?" *) token env v2 in
      let v3 = map_expression env v3 in
      let v4 = (* ":" *) token env v4 in
      let v5 = map_expression env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `Bin_exp x -> R.Case ("Bin_exp",
      map_binary_expression env x
    )
  | `Post_exp (v1, v2) -> R.Case ("Post_exp",
      let v1 = map_expression env v1 in
      let v2 =
        (match v2 with
        | `PLUSPLUS tok -> R.Case ("PLUSPLUS",
            (* "++" *) token env tok
          )
        | `DASHDASH tok -> R.Case ("DASHDASH",
            (* "--" *) token env tok
          )
        )
      in
      R.Tuple [v1; v2]
    )
  | `Paren_exp (v1, v2, v3) -> R.Case ("Paren_exp",
      let v1 = (* "(" *) token env v1 in
      let v2 = map_expression env v2 in
      let v3 = (* ")" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_file_redirect (env : env) ((v1, v2, v3) : CST.file_redirect) =
  let v1 =
    (match v1 with
    | Some tok -> R.Option (Some (
        (* file_descriptor *) token env tok
      ))
    | None -> R.Option None)
  in
  let v2 =
    (match v2 with
    | `LT tok -> R.Case ("LT",
        (* "<" *) token env tok
      )
    | `GT tok -> R.Case ("GT",
        (* ">" *) token env tok
      )
    | `GTGT tok -> R.Case ("GTGT",
        (* ">>" *) token env tok
      )
    | `AMPGT tok -> R.Case ("AMPGT",
        (* "&>" *) token env tok
      )
    | `AMPGTGT tok -> R.Case ("AMPGTGT",
        (* "&>>" *) token env tok
      )
    | `LTAMP tok -> R.Case ("LTAMP",
        (* "<&" *) token env tok
      )
    | `GTAMP tok -> R.Case ("GTAMP",
        (* ">&" *) token env tok
      )
    | `GTBAR tok -> R.Case ("GTBAR",
        (* ">|" *) token env tok
      )
    )
  in
  let v3 = map_literal env v3 in
  R.Tuple [v1; v2; v3]

and map_heredoc_body (env : env) (x : CST.heredoc_body) =
  (match x with
  | `Simple_here_body tok -> R.Case ("Simple_here_body",
      (* simple_heredoc_body *) token env tok
    )
  | `Here_body_begin_rep_choice_expa_here_body_end (v1, v2, v3) -> R.Case ("Here_body_begin_rep_choice_expa_here_body_end",
      let v1 = (* heredoc_body_beginning *) token env v1 in
      let v2 =
        R.List (List.map (fun x ->
          (match x with
          | `Expa x -> R.Case ("Expa",
              map_expansion env x
            )
          | `Simple_expa x -> R.Case ("Simple_expa",
              map_simple_expansion env x
            )
          | `Cmd_subs x -> R.Case ("Cmd_subs",
              map_command_substitution env x
            )
          | `Here_body_middle tok -> R.Case ("Here_body_middle",
              (* heredoc_body_middle *) token env tok
            )
          )
        ) v2)
      in
      let v3 = (* heredoc_body_end *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_herestring_redirect (env : env) ((v1, v2) : CST.herestring_redirect) =
  let v1 = (* "<<<" *) token env v1 in
  let v2 = map_literal env v2 in
  R.Tuple [v1; v2]

and map_last_case_item (env : env) ((v1, v2, v3, v4, v5) : CST.last_case_item) =
  let v1 = map_literal env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "|" *) token env v1 in
      let v2 = map_literal env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  let v3 = (* ")" *) token env v3 in
  let v4 = map_program env v4 in
  let v5 =
    (match v5 with
    | Some tok -> R.Option (Some (
        (* ";;" *) token env tok
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3; v4; v5]

and map_literal (env : env) (x : CST.literal) =
  (match x with
  | `Conc x -> R.Case ("Conc",
      map_concatenation env x
    )
  | `Choice_semg_deep_exp x -> R.Case ("Choice_semg_deep_exp",
      map_primary_expression env x
    )
  | `Rep1_spec_char xs -> R.Case ("Rep1_spec_char",
      R.List (List.map (token env (* special_character *)) xs)
    )
  )

and map_primary_expression (env : env) (x : CST.primary_expression) =
  (match x with
  | `Semg_deep_exp (v1, v2, v3) -> R.Case ("Semg_deep_exp",
      let v1 = (* "<..." *) token env v1 in
      let v2 = map_literal env v2 in
      let v3 = (* "...>" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Choice_word x -> R.Case ("Choice_word",
      (match x with
      | `Word tok -> R.Case ("Word",
          (* word *) token env tok
        )
      | `Str x -> R.Case ("Str",
          map_string_ env x
        )
      | `Raw_str tok -> R.Case ("Raw_str",
          (* pattern "'[^']*'" *) token env tok
        )
      | `Ansii_c_str tok -> R.Case ("Ansii_c_str",
          (* pattern "\\$'([^']|\\\\')*'" *) token env tok
        )
      | `Expa x -> R.Case ("Expa",
          map_expansion env x
        )
      | `Simple_expa x -> R.Case ("Simple_expa",
          map_simple_expansion env x
        )
      | `Str_expa x -> R.Case ("Str_expa",
          map_string_expansion env x
        )
      | `Cmd_subs x -> R.Case ("Cmd_subs",
          map_command_substitution env x
        )
      | `Proc_subs x -> R.Case ("Proc_subs",
          map_process_substitution env x
        )
      )
    )
  )

and map_process_substitution (env : env) ((v1, v2, v3) : CST.process_substitution) =
  let v1 =
    (match v1 with
    | `LTLPAR tok -> R.Case ("LTLPAR",
        (* "<(" *) token env tok
      )
    | `GTLPAR tok -> R.Case ("GTLPAR",
        (* ">(" *) token env tok
      )
    )
  in
  let v2 = map_statements env v2 in
  let v3 = (* ")" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_program (env : env) (opt : CST.program) =
  (match opt with
  | Some x -> R.Option (Some (
      map_statements env x
    ))
  | None -> R.Option None)

and map_statement (env : env) (x : CST.statement) =
  (match x with
  | `Redi_stmt (v1, v2) -> R.Case ("Redi_stmt",
      let v1 = map_statement env v1 in
      let v2 =
        R.List (List.map (fun x ->
          (match x with
          | `File_redi x -> R.Case ("File_redi",
              map_file_redirect env x
            )
          | `Here_redi_a9657de x -> R.Case ("Here_redi_a9657de",
              map_heredoc_redirect env x
            )
          | `Here_redi_7d3292d x -> R.Case ("Here_redi_7d3292d",
              map_herestring_redirect env x
            )
          )
        ) v2)
      in
      R.Tuple [v1; v2]
    )
  | `Var_assign x -> R.Case ("Var_assign",
      map_variable_assignment env x
    )
  | `Cmd x -> R.Case ("Cmd",
      map_command env x
    )
  | `Decl_cmd (v1, v2) -> R.Case ("Decl_cmd",
      let v1 =
        (match v1 with
        | `Decl tok -> R.Case ("Decl",
            (* "declare" *) token env tok
          )
        | `Type tok -> R.Case ("Type",
            (* "typeset" *) token env tok
          )
        | `Export tok -> R.Case ("Export",
            (* "export" *) token env tok
          )
        | `Read tok -> R.Case ("Read",
            (* "readonly" *) token env tok
          )
        | `Local tok -> R.Case ("Local",
            (* "local" *) token env tok
          )
        )
      in
      let v2 =
        R.List (List.map (fun x ->
          (match x with
          | `Choice_conc x -> R.Case ("Choice_conc",
              map_literal env x
            )
          | `Choice_semg_meta x -> R.Case ("Choice_semg_meta",
              map_simple_variable_name env x
            )
          | `Var_assign x -> R.Case ("Var_assign",
              map_variable_assignment env x
            )
          )
        ) v2)
      in
      R.Tuple [v1; v2]
    )
  | `Unset_cmd (v1, v2) -> R.Case ("Unset_cmd",
      let v1 =
        (match v1 with
        | `Unset tok -> R.Case ("Unset",
            (* "unset" *) token env tok
          )
        | `Unse tok -> R.Case ("Unse",
            (* "unsetenv" *) token env tok
          )
        )
      in
      let v2 =
        R.List (List.map (fun x ->
          (match x with
          | `Choice_conc x -> R.Case ("Choice_conc",
              map_literal env x
            )
          | `Choice_semg_meta x -> R.Case ("Choice_semg_meta",
              map_simple_variable_name env x
            )
          )
        ) v2)
      in
      R.Tuple [v1; v2]
    )
  | `Test_cmd x -> R.Case ("Test_cmd",
      map_test_command env x
    )
  | `Nega_cmd (v1, v2) -> R.Case ("Nega_cmd",
      let v1 = (* "!" *) token env v1 in
      let v2 =
        (match v2 with
        | `Cmd x -> R.Case ("Cmd",
            map_command env x
          )
        | `Test_cmd x -> R.Case ("Test_cmd",
            map_test_command env x
          )
        | `Subs x -> R.Case ("Subs",
            map_subshell env x
          )
        )
      in
      R.Tuple [v1; v2]
    )
  | `For_stmt (v1, v2, v3, v4, v5) -> R.Case ("For_stmt",
      let v1 =
        (match v1 with
        | `For tok -> R.Case ("For",
            (* "for" *) token env tok
          )
        | `Select tok -> R.Case ("Select",
            (* "select" *) token env tok
          )
        )
      in
      let v2 = map_simple_variable_name env v2 in
      let v3 =
        (match v3 with
        | Some (v1, v2) -> R.Option (Some (
            let v1 = (* "in" *) token env v1 in
            let v2 = R.List (List.map (map_literal env) v2) in
            R.Tuple [v1; v2]
          ))
        | None -> R.Option None)
      in
      let v4 = map_terminator env v4 in
      let v5 = map_do_group env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `C_style_for_stmt (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10) -> R.Case ("C_style_for_stmt",
      let v1 = (* "for" *) token env v1 in
      let v2 = (* "((" *) token env v2 in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_expression env x
          ))
        | None -> R.Option None)
      in
      let v4 = map_terminator env v4 in
      let v5 =
        (match v5 with
        | Some x -> R.Option (Some (
            map_expression env x
          ))
        | None -> R.Option None)
      in
      let v6 = map_terminator env v6 in
      let v7 =
        (match v7 with
        | Some x -> R.Option (Some (
            map_expression env x
          ))
        | None -> R.Option None)
      in
      let v8 = (* "))" *) token env v8 in
      let v9 =
        (match v9 with
        | Some tok -> R.Option (Some (
            (* ";" *) token env tok
          ))
        | None -> R.Option None)
      in
      let v10 =
        (match v10 with
        | `Do_group x -> R.Case ("Do_group",
            map_do_group env x
          )
        | `Comp_stmt x -> R.Case ("Comp_stmt",
            map_compound_statement env x
          )
        )
      in
      R.Tuple [v1; v2; v3; v4; v5; v6; v7; v8; v9; v10]
    )
  | `While_stmt (v1, v2, v3) -> R.Case ("While_stmt",
      let v1 = (* "while" *) token env v1 in
      let v2 = map_terminated_statement env v2 in
      let v3 = map_do_group env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `If_stmt (v1, v2, v3, v4, v5, v6, v7) -> R.Case ("If_stmt",
      let v1 = (* "if" *) token env v1 in
      let v2 = map_terminated_statement env v2 in
      let v3 = (* "then" *) token env v3 in
      let v4 =
        (match v4 with
        | Some x -> R.Option (Some (
            map_statements2 env x
          ))
        | None -> R.Option None)
      in
      let v5 = R.List (List.map (map_elif_clause env) v5) in
      let v6 =
        (match v6 with
        | Some x -> R.Option (Some (
            map_else_clause env x
          ))
        | None -> R.Option None)
      in
      let v7 = (* "fi" *) token env v7 in
      R.Tuple [v1; v2; v3; v4; v5; v6; v7]
    )
  | `Case_stmt (v1, v2, v3, v4, v5, v6, v7) -> R.Case ("Case_stmt",
      let v1 = (* "case" *) token env v1 in
      let v2 = map_literal env v2 in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_terminator env x
          ))
        | None -> R.Option None)
      in
      let v4 = (* "in" *) token env v4 in
      let v5 = map_terminator env v5 in
      let v6 =
        (match v6 with
        | Some (v1, v2) -> R.Option (Some (
            let v1 = R.List (List.map (map_case_item env) v1) in
            let v2 = map_last_case_item env v2 in
            R.Tuple [v1; v2]
          ))
        | None -> R.Option None)
      in
      let v7 = (* "esac" *) token env v7 in
      R.Tuple [v1; v2; v3; v4; v5; v6; v7]
    )
  | `Pipe (v1, v2, v3) -> R.Case ("Pipe",
      let v1 = map_statement env v1 in
      let v2 =
        (match v2 with
        | `BAR tok -> R.Case ("BAR",
            (* "|" *) token env tok
          )
        | `BARAMP tok -> R.Case ("BARAMP",
            (* "|&" *) token env tok
          )
        )
      in
      let v3 = map_statement env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `List (v1, v2, v3) -> R.Case ("List",
      let v1 = map_statement env v1 in
      let v2 =
        (match v2 with
        | `AMPAMP tok -> R.Case ("AMPAMP",
            (* "&&" *) token env tok
          )
        | `BARBAR tok -> R.Case ("BARBAR",
            (* "||" *) token env tok
          )
        )
      in
      let v3 = map_statement env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Subs x -> R.Case ("Subs",
      map_subshell env x
    )
  | `Comp_stmt x -> R.Case ("Comp_stmt",
      map_compound_statement env x
    )
  | `Func_defi (v1, v2) -> R.Case ("Func_defi",
      let v1 =
        (match v1 with
        | `Func_exte_word_opt_LPAR_RPAR (v1, v2, v3) -> R.Case ("Func_exte_word_opt_LPAR_RPAR",
            let v1 = (* "function" *) token env v1 in
            let v2 = map_extended_word env v2 in
            let v3 =
              (match v3 with
              | Some (v1, v2) -> R.Option (Some (
                  let v1 = (* "(" *) token env v1 in
                  let v2 = (* ")" *) token env v2 in
                  R.Tuple [v1; v2]
                ))
              | None -> R.Option None)
            in
            R.Tuple [v1; v2; v3]
          )
        | `Word_LPAR_RPAR (v1, v2, v3) -> R.Case ("Word_LPAR_RPAR",
            let v1 = (* word *) token env v1 in
            let v2 = (* "(" *) token env v2 in
            let v3 = (* ")" *) token env v3 in
            R.Tuple [v1; v2; v3]
          )
        )
      in
      let v2 =
        (match v2 with
        | `Comp_stmt x -> R.Case ("Comp_stmt",
            map_compound_statement env x
          )
        | `Subs x -> R.Case ("Subs",
            map_subshell env x
          )
        | `Test_cmd x -> R.Case ("Test_cmd",
            map_test_command env x
          )
        )
      in
      R.Tuple [v1; v2]
    )
  )

and map_statements (env : env) ((v1, v2, v3, v4) : CST.statements) =
  let v1 =
    R.List (List.map (map_anon_stmt_opt_LF_here_body_term_3efa649 env) v1)
  in
  let v2 = map_statement env v2 in
  let v3 =
    (match v3 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = (* "\n" *) token env v1 in
        let v2 = map_heredoc_body env v2 in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  let v4 =
    (match v4 with
    | Some x -> R.Option (Some (
        map_terminator env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3; v4]

and map_statements2 (env : env) (xs : CST.statements2) =
  R.List (List.map (map_anon_stmt_opt_LF_here_body_term_3efa649 env) xs)

and map_string_ (env : env) ((v1, v2, v3, v4) : CST.string_) =
  let v1 = (* "\"" *) token env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 =
        (match v1 with
        | `Opt_DOLLAR_str_content (v1, v2) -> R.Case ("Opt_DOLLAR_str_content",
            let v1 =
              (match v1 with
              | Some tok -> R.Option (Some (
                  (* "$" *) token env tok
                ))
              | None -> R.Option None)
            in
            let v2 = (* string_content *) token env v2 in
            R.Tuple [v1; v2]
          )
        | `Expa x -> R.Case ("Expa",
            map_expansion env x
          )
        | `Simple_expa x -> R.Case ("Simple_expa",
            map_simple_expansion env x
          )
        | `Cmd_subs x -> R.Case ("Cmd_subs",
            map_command_substitution env x
          )
        )
      in
      let v2 =
        (match v2 with
        | Some tok -> R.Option (Some (
            (* concat *) token env tok
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2]
    ) v2)
  in
  let v3 =
    (match v3 with
    | Some tok -> R.Option (Some (
        (* "$" *) token env tok
      ))
    | None -> R.Option None)
  in
  let v4 = (* "\"" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_string_expansion (env : env) ((v1, v2) : CST.string_expansion) =
  let v1 = (* "$" *) token env v1 in
  let v2 =
    (match v2 with
    | `Str x -> R.Case ("Str",
        map_string_ env x
      )
    | `Raw_str tok -> R.Case ("Raw_str",
        (* pattern "'[^']*'" *) token env tok
      )
    )
  in
  R.Tuple [v1; v2]

and map_subscript (env : env) ((v1, v2, v3, v4, v5, v6) : CST.subscript) =
  let v1 = (* variable_name *) token env v1 in
  let v2 = (* "[" *) token env v2 in
  let v3 = map_literal env v3 in
  let v4 =
    (match v4 with
    | Some tok -> R.Option (Some (
        (* concat *) token env tok
      ))
    | None -> R.Option None)
  in
  let v5 = (* "]" *) token env v5 in
  let v6 =
    (match v6 with
    | Some tok -> R.Option (Some (
        (* concat *) token env tok
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3; v4; v5; v6]

and map_subshell (env : env) ((v1, v2, v3) : CST.subshell) =
  let v1 = (* "(" *) token env v1 in
  let v2 = map_statements env v2 in
  let v3 = (* ")" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_terminated_statement (env : env) ((v1, v2) : CST.terminated_statement) =
  let v1 = map_statement env v1 in
  let v2 = map_terminator env v2 in
  R.Tuple [v1; v2]

and map_test_command (env : env) (v1 : CST.test_command) =
  (match v1 with
  | `LBRACK_exp_RBRACK (v1, v2, v3) -> R.Case ("LBRACK_exp_RBRACK",
      let v1 = (* "[" *) token env v1 in
      let v2 = map_expression env v2 in
      let v3 = (* "]" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `LBRACKLBRACK_exp_RBRACKRBRACK (v1, v2, v3) -> R.Case ("LBRACKLBRACK_exp_RBRACKRBRACK",
      let v1 = (* "[[" *) token env v1 in
      let v2 = map_expression env v2 in
      let v3 = (* "]]" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `LPARLPAR_exp_RPARRPAR (v1, v2, v3) -> R.Case ("LPARLPAR_exp_RPARRPAR",
      let v1 = (* "((" *) token env v1 in
      let v2 = map_expression env v2 in
      let v3 = (* "))" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_variable_assignment (env : env) (x : CST.variable_assignment) =
  (match x with
  | `Choice_semg_meta_eq_choice_choice_conc (v1, v2) -> R.Case ("Choice_semg_meta_eq_choice_choice_conc",
      let v1 =
        (match v1 with
        | `Semg_meta_eq tok -> R.Case ("Semg_meta_eq",
            (* pattern \$[A-Z_][A-Z_0-9]*= *) token env tok
          )
        | `Semg_meta_pluseq tok -> R.Case ("Semg_meta_pluseq",
            (* pattern \$[A-Z_][A-Z_0-9]*\+= *) token env tok
          )
        )
      in
      let v2 = map_anon_choice_lit_bbf16c7 env v2 in
      R.Tuple [v1; v2]
    )
  | `Choice_var_name_choice_EQ_choice_choice_conc (v1, v2, v3) -> R.Case ("Choice_var_name_choice_EQ_choice_choice_conc",
      let v1 =
        (match v1 with
        | `Var_name tok -> R.Case ("Var_name",
            (* variable_name *) token env tok
          )
        | `Subs x -> R.Case ("Subs",
            map_subscript env x
          )
        )
      in
      let v2 =
        (match v2 with
        | `EQ tok -> R.Case ("EQ",
            (* "=" *) token env tok
          )
        | `PLUSEQ tok -> R.Case ("PLUSEQ",
            (* "+=" *) token env tok
          )
        )
      in
      let v3 = map_anon_choice_lit_bbf16c7 env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

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
