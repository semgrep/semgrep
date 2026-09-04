(**
   Boilerplate to be used as a template when mapping the scala CST
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

let map_tok_using (env : env) (tok : CST.tok_using) =
  (* tok_using *) token env tok

let map_outdent (env : env) (tok : CST.outdent) =
  (* outdent *) token env tok

let map_raw_string_multiline_middle (env : env) (tok : CST.raw_string_multiline_middle) =
  (* raw_string_multiline_middle *) token env tok

let map_imm_tok_prec_p2_gt (env : env) (tok : CST.imm_tok_prec_p2_gt) =
  (* ">" *) token env tok

let map_simple_multiline_string_start (env : env) (tok : CST.simple_multiline_string_start) =
  (* simple_multiline_string_start *) token env tok

let map_boolean_literal (env : env) (x : CST.boolean_literal) =
  (match x with
  | `True tok -> R.Case ("True",
      (* "true" *) token env tok
    )
  | `False tok -> R.Case ("False",
      (* "false" *) token env tok
    )
  )

let map_character_literal (env : env) (tok : CST.character_literal) =
  (* character_literal *) token env tok

let map_tok_starslash (env : env) (tok : CST.tok_starslash) =
  (* tok_starslash *) token env tok

let map_indent (env : env) (tok : CST.indent) =
  (* indent *) token env tok

let map_backquoted_id (env : env) (tok : CST.backquoted_id) =
  (* pattern `[^\n`]+` *) token env tok

let map_semgrep_ellipsis (env : env) (tok : CST.semgrep_ellipsis) =
  (* semgrep_ellipsis *) token env tok

let map_tok_slashstar (env : env) (tok : CST.tok_slashstar) =
  (* tok_slashstar *) token env tok

let map_escape_sequence (env : env) (tok : CST.escape_sequence) =
  (* escape_sequence *) token env tok

let map_tok_slashslash (env : env) (tok : CST.tok_slashslash) =
  (* tok_slashslash *) token env tok

let map_alpha_identifier (env : env) (tok : CST.alpha_identifier) =
  (* pattern [\p{Lu}\p{Lt}\p{Nl}\p{Lo}\p{Lm}\$\p{Ll}_\u00AA\u00BB\u02B0-\u02B8\u02C0-\u02C1\u02E0-\u02E4\u037A\u1D78\u1D9B-\u1DBF\u2071\u207F\u2090-\u209C\u2C7C-\u2C7D\uA69C-\uA69D\uA770\uA7F8-\uA7F9\uAB5C-\uAB5F\$][\p{Lu}\p{Lt}\p{Nl}\p{Lo}\p{Lm}\$\p{Ll}_\u00AA\u00BB\u02B0-\u02B8\u02C0-\u02C1\u02E0-\u02E4\u037A\u1D78\u1D9B-\u1DBF\u2071\u207F\u2090-\u209C\u2C7C-\u2C7D\uA69C-\uA69D\uA770\uA7F8-\uA7F9\uAB5C-\uAB5F0-9\$_\p{Ll}]*(_[\-!#%&*+\/\\:<=>?@\u005e\u007c~\p{Sm}\p{So}]+)? *) token env tok

let map_floating_point_literal (env : env) (tok : CST.floating_point_literal) =
  (* floating_point_literal *) token env tok

let map_interpolated_string_middle (env : env) (tok : CST.interpolated_string_middle) =
  (* interpolated_string_middle *) token env tok

let map_tok_prec_p100___semgrep_statement (env : env) (tok : CST.tok_prec_p100___semgrep_statement) =
  (* tok_prec_p100___semgrep_statement *) token env tok

let map_unit_ (env : env) ((v1, v2) : CST.unit_) =
  let v1 = (* "(" *) token env v1 in
  let v2 = (* ")" *) token env v2 in
  R.Tuple [v1; v2]

let map_automatic_semicolon (env : env) (tok : CST.automatic_semicolon) =
  (* automatic_semicolon *) token env tok

let map_simple_string_middle (env : env) (tok : CST.simple_string_middle) =
  (* simple_string_middle *) token env tok

let map_raw_string_middle (env : env) (tok : CST.raw_string_middle) =
  (* raw_string_middle *) token env tok

let map_comment_text (env : env) (tok : CST.comment_text) =
  (* comment_text *) token env tok

let map_soft_identifier (env : env) (x : CST.soft_identifier) =
  (match x with
  | `Infix tok -> R.Case ("Infix",
      (* "infix" *) token env tok
    )
  | `Inline tok -> R.Case ("Inline",
      (* "inline" *) token env tok
    )
  | `Opaque tok -> R.Case ("Opaque",
      (* "opaque" *) token env tok
    )
  | `Open tok -> R.Case ("Open",
      (* "open" *) token env tok
    )
  | `Trac tok -> R.Case ("Trac",
      (* "tracked" *) token env tok
    )
  | `Tran tok -> R.Case ("Tran",
      (* "transparent" *) token env tok
    )
  | `End tok -> R.Case ("End",
      (* "end" *) token env tok
    )
  )

let map_semgrep_ellipsis_metavariable (env : env) (tok : CST.semgrep_ellipsis_metavariable) =
  (* semgrep_ellipsis_metavariable *) token env tok

let map_using_directive_key (env : env) (tok : CST.using_directive_key) =
  (* using_directive_key *) token env tok

let map_semgrep_metavariable (env : env) (tok : CST.semgrep_metavariable) =
  (* semgrep_metavariable *) token env tok

let map_integer_literal (env : env) (tok : CST.integer_literal) =
  (* integer_literal *) token env tok

let map_tok_prec_p100___semgrep_expression (env : env) (tok : CST.tok_prec_p100___semgrep_expression) =
  (* tok_prec_p100___semgrep_expression *) token env tok

let map_tok_prec_p100___semgrep_member_decl (env : env) (tok : CST.tok_prec_p100___semgrep_member_decl) =
  (* tok_prec_p100___semgrep_member_decl *) token env tok

let map_operator_identifier (env : env) (tok : CST.operator_identifier) =
  (* operator_identifier *) token env tok

let map_tok_dollar_choice_dollar (env : env) (tok : CST.tok_dollar_choice_dollar) =
  (* tok_dollar_choice_dollar *) token env tok

let map_imm_tok_dquot (env : env) (tok : CST.imm_tok_dquot) =
  (* "\"" *) token env tok

let map_raw_string_start (env : env) (tok : CST.raw_string_start) =
  (* raw_string_start *) token env tok

let map_single_line_string_end (env : env) (tok : CST.single_line_string_end) =
  (* single_line_string_end *) token env tok

let map_tok_pat_5058f1a (env : env) (tok : CST.tok_pat_5058f1a) =
  (* tok_pat_5058f1a *) token env tok

let map_anon_choice_EQGT_ce418c1 (env : env) (x : CST.anon_choice_EQGT_ce418c1) =
  (match x with
  | `EQGT tok -> R.Case ("EQGT",
      (* "=>" *) token env tok
    )
  | `QMARKEQGT tok -> R.Case ("QMARKEQGT",
      (* "?=>" *) token env tok
    )
  )

let map_tok_hashbang_pat_4fd4a56 (env : env) (tok : CST.tok_hashbang_pat_4fd4a56) =
  (* tok_hashbang_pat_4fd4a56 *) token env tok

let map_interpolation_identifier (env : env) (tok : CST.interpolation_identifier) =
  (* pattern [\p{Lu}\p{Lt}\p{Nl}\p{Lo}\p{Lm}\p{Ll}_\u00AA\u00BB\u02B0-\u02B8\u02C0-\u02C1\u02E0-\u02E4\u037A\u1D78\u1D9B-\u1DBF\u2071\u207F\u2090-\u209C\u2C7C-\u2C7D\uA69C-\uA69D\uA770\uA7F8-\uA7F9\uAB5C-\uAB5F][\p{Lu}\p{Lt}\p{Nl}\p{Lo}\p{Lm}\p{Ll}_\u00AA\u00BB\u02B0-\u02B8\u02C0-\u02C1\u02E0-\u02E4\u037A\u1D78\u1D9B-\u1DBF\u2071\u207F\u2090-\u209C\u2C7C-\u2C7D\uA69C-\uA69D\uA770\uA7F8-\uA7F9\uAB5C-\uAB5F0-9_\p{Ll}]* *) token env tok

let map_simple_string_start (env : env) (tok : CST.simple_string_start) =
  (* simple_string_start *) token env tok

let map_multiline_string_end (env : env) (tok : CST.multiline_string_end) =
  (* multiline_string_end *) token env tok

let map_using_directive_value (env : env) (tok : CST.using_directive_value) =
  (* using_directive_value *) token env tok

let map_interpolated_multiline_string_middle (env : env) (tok : CST.interpolated_multiline_string_middle) =
  (* interpolated_multiline_string_middle *) token env tok

let map_imm_tok_dquotdquotdquot (env : env) (tok : CST.imm_tok_dquotdquotdquot) =
  (* "\"\"\"" *) token env tok

let map_namespace_wildcard (env : env) (x : CST.namespace_wildcard) =
  (match x with
  | `STAR tok -> R.Case ("STAR",
      (* "*" *) token env tok
    )
  | `X__ tok -> R.Case ("X__",
      (* "_" *) token env tok
    )
  | `Given tok -> R.Case ("Given",
      (* "given" *) token env tok
    )
  )

let map_semicolon (env : env) (x : CST.semicolon) =
  (match x with
  | `SEMI tok -> R.Case ("SEMI",
      (* ";" *) token env tok
    )
  | `Auto_semi tok -> R.Case ("Auto_semi",
      (* automatic_semicolon *) token env tok
    )
  )

let map_identifier (env : env) (x : CST.identifier) =
  (match x with
  | `Alpha_id tok -> R.Case ("Alpha_id",
      (* pattern [\p{Lu}\p{Lt}\p{Nl}\p{Lo}\p{Lm}\$\p{Ll}_\u00AA\u00BB\u02B0-\u02B8\u02C0-\u02C1\u02E0-\u02E4\u037A\u1D78\u1D9B-\u1DBF\u2071\u207F\u2090-\u209C\u2C7C-\u2C7D\uA69C-\uA69D\uA770\uA7F8-\uA7F9\uAB5C-\uAB5F\$][\p{Lu}\p{Lt}\p{Nl}\p{Lo}\p{Lm}\$\p{Ll}_\u00AA\u00BB\u02B0-\u02B8\u02C0-\u02C1\u02E0-\u02E4\u037A\u1D78\u1D9B-\u1DBF\u2071\u207F\u2090-\u209C\u2C7C-\u2C7D\uA69C-\uA69D\uA770\uA7F8-\uA7F9\uAB5C-\uAB5F0-9\$_\p{Ll}]*(_[\-!#%&*+\/\\:<=>?@\u005e\u007c~\p{Sm}\p{So}]+)? *) token env tok
    )
  | `Back_id tok -> R.Case ("Back_id",
      (* pattern `[^\n`]+` *) token env tok
    )
  | `Soft_id x -> R.Case ("Soft_id",
      map_soft_identifier env x
    )
  )

let map_dollar_escape (env : env) (x : CST.dollar_escape) =
  map_tok_dollar_choice_dollar env x

let map_shebang (env : env) (x : CST.shebang) =
  map_tok_hashbang_pat_4fd4a56 env x

let map_string_ (env : env) (x : CST.string_) =
  (match x with
  | `Simple_str_start_rep_simple_str_middle_esc_seq_single_line_str_end (v1, v2, v3) -> R.Case ("Simple_str_start_rep_simple_str_middle_esc_seq_single_line_str_end",
      let v1 = (* simple_string_start *) token env v1 in
      let v2 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = (* simple_string_middle *) token env v1 in
          let v2 = (* escape_sequence *) token env v2 in
          R.Tuple [v1; v2]
        ) v2)
      in
      let v3 = (* single_line_string_end *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Simple_mult_str_start_mult_str_end (v1, v2) -> R.Case ("Simple_mult_str_start_mult_str_end",
      let v1 = (* simple_multiline_string_start *) token env v1 in
      let v2 = (* multiline_string_end *) token env v2 in
      R.Tuple [v1; v2]
    )
  )

let map_using_directive (env : env) ((v1, v2, v3, v4) : CST.using_directive) =
  let v1 = map_imm_tok_prec_p2_gt env v1 in
  let v2 = map_tok_using env v2 in
  let v3 = (* using_directive_key *) token env v3 in
  let v4 = (* using_directive_value *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

let map_type_identifier (env : env) (x : CST.type_identifier) =
  (match x with
  | `Id x -> R.Case ("Id",
      map_identifier env x
    )
  | `Op_id tok -> R.Case ("Op_id",
      (* operator_identifier *) token env tok
    )
  )

let map_non_null_literal (env : env) (x : CST.non_null_literal) =
  (match x with
  | `Int_lit tok -> R.Case ("Int_lit",
      (* integer_literal *) token env tok
    )
  | `Floa_point_lit tok -> R.Case ("Floa_point_lit",
      (* floating_point_literal *) token env tok
    )
  | `Bool_lit x -> R.Case ("Bool_lit",
      map_boolean_literal env x
    )
  | `Char_lit tok -> R.Case ("Char_lit",
      (* character_literal *) token env tok
    )
  | `Str x -> R.Case ("Str",
      map_string_ env x
    )
  )

let map_package_identifier (env : env) ((v1, v2) : CST.package_identifier) =
  let v1 = map_type_identifier env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "." *) token env v1 in
      let v2 = map_type_identifier env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  R.Tuple [v1; v2]

let map_access_qualifier (env : env) ((v1, v2, v3) : CST.access_qualifier) =
  let v1 = (* "[" *) token env v1 in
  let v2 = map_type_identifier env v2 in
  let v3 = (* "]" *) token env v3 in
  R.Tuple [v1; v2; v3]

let map_end_marker (env : env) ((v1, v2) : CST.end_marker) =
  let v1 = (* "end" *) token env v1 in
  let v2 =
    (match v2 with
    | `If tok -> R.Case ("If",
        (* "if" *) token env tok
      )
    | `While tok -> R.Case ("While",
        (* "while" *) token env tok
      )
    | `For tok -> R.Case ("For",
        (* "for" *) token env tok
      )
    | `Match tok -> R.Case ("Match",
        (* "match" *) token env tok
      )
    | `Try tok -> R.Case ("Try",
        (* "try" *) token env tok
      )
    | `New tok -> R.Case ("New",
        (* "new" *) token env tok
      )
    | `This tok -> R.Case ("This",
        (* "this" *) token env tok
      )
    | `Given tok -> R.Case ("Given",
        (* "given" *) token env tok
      )
    | `Exte tok -> R.Case ("Exte",
        (* "extension" *) token env tok
      )
    | `Val tok -> R.Case ("Val",
        (* "val" *) token env tok
      )
    | `Choice_id x -> R.Case ("Choice_id",
        map_type_identifier env x
      )
    )
  in
  R.Tuple [v1; v2]

let map_anon_choice_type_id_0797463 (env : env) (x : CST.anon_choice_type_id_0797463) =
  (match x with
  | `Choice_id x -> R.Case ("Choice_id",
      map_type_identifier env x
    )
  | `Wild tok -> R.Case ("Wild",
      (* "_" *) token env tok
    )
  )

let rec map_anon_choice_type_id_4bf0d65 (env : env) (x : CST.anon_choice_type_id_4bf0d65) =
  (match x with
  | `Choice_id x -> R.Case ("Choice_id",
      map_type_identifier env x
    )
  | `Stable_id x -> R.Case ("Stable_id",
      map_stable_identifier env x
    )
  )

and map_stable_identifier (env : env) ((v1, v2, v3) : CST.stable_identifier) =
  let v1 = map_anon_choice_type_id_4bf0d65 env v1 in
  let v2 = (* "." *) token env v2 in
  let v3 = map_type_identifier env v3 in
  R.Tuple [v1; v2; v3]

let map_literal_type (env : env) (x : CST.literal_type) =
  map_non_null_literal env x

let map_access_modifier (env : env) ((v1, v2) : CST.access_modifier) =
  let v1 =
    (match v1 with
    | `Priv tok -> R.Case ("Priv",
        (* "private" *) token env tok
      )
    | `Prot tok -> R.Case ("Prot",
        (* "protected" *) token env tok
      )
    )
  in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_access_qualifier env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2]

let map_as_renamed_identifier (env : env) ((v1, v2, v3) : CST.as_renamed_identifier) =
  let v1 = map_type_identifier env v1 in
  let v2 = (* "as" *) token env v2 in
  let v3 = map_anon_choice_type_id_0797463 env v3 in
  R.Tuple [v1; v2; v3]

let map_stable_type_identifier (env : env) ((v1, v2, v3) : CST.stable_type_identifier) =
  let v1 = map_anon_choice_type_id_4bf0d65 env v1 in
  let v2 = (* "." *) token env v2 in
  let v3 = map_type_identifier env v3 in
  R.Tuple [v1; v2; v3]

let map_literal (env : env) (x : CST.literal) =
  (match x with
  | `Non_null_lit x -> R.Case ("Non_null_lit",
      map_literal_type env x
    )
  | `Null_lit tok -> R.Case ("Null_lit",
      (* "null" *) token env tok
    )
  )

let map_modifiers (env : env) (xs : CST.modifiers) =
  R.List (List.map (fun x ->
    (match x with
    | `Abst tok -> R.Case ("Abst",
        (* "abstract" *) token env tok
      )
    | `Final tok -> R.Case ("Final",
        (* "final" *) token env tok
      )
    | `Sealed tok -> R.Case ("Sealed",
        (* "sealed" *) token env tok
      )
    | `Impl tok -> R.Case ("Impl",
        (* "implicit" *) token env tok
      )
    | `Lazy tok -> R.Case ("Lazy",
        (* "lazy" *) token env tok
      )
    | `Over tok -> R.Case ("Over",
        (* "override" *) token env tok
      )
    | `Access_modi x -> R.Case ("Access_modi",
        map_access_modifier env x
      )
    | `Inline_modi tok -> R.Case ("Inline_modi",
        (* "inline" *) token env tok
      )
    | `Infix_modi tok -> R.Case ("Infix_modi",
        (* "infix" *) token env tok
      )
    | `Into_modi tok -> R.Case ("Into_modi",
        (* "into" *) token env tok
      )
    | `Open_modi tok -> R.Case ("Open_modi",
        (* "open" *) token env tok
      )
    | `Trac_modi tok -> R.Case ("Trac_modi",
        (* "tracked" *) token env tok
      )
    | `Tran_modi tok -> R.Case ("Tran_modi",
        (* "transparent" *) token env tok
      )
    )
  ) xs)

let map_anon_choice_type_id_ae98204 (env : env) (x : CST.anon_choice_type_id_ae98204) =
  (match x with
  | `Type_id x -> R.Case ("Type_id",
      map_type_identifier env x
    )
  | `Stable_type_id x -> R.Case ("Stable_type_id",
      map_stable_type_identifier env x
    )
  )

let map_derives_clause (env : env) ((v1, v2, v3) : CST.derives_clause) =
  let v1 = (* "derives" *) token env v1 in
  let v2 = map_anon_choice_type_id_ae98204 env v2 in
  let v3 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_anon_choice_type_id_ae98204 env v2 in
      R.Tuple [v1; v2]
    ) v3)
  in
  R.Tuple [v1; v2; v3]

let rec map_alternative_pattern (env : env) ((v1, v2, v3) : CST.alternative_pattern) =
  let v1 = map_pattern env v1 in
  let v2 = (* "|" *) token env v2 in
  let v3 = map_pattern env v3 in
  R.Tuple [v1; v2; v3]

and map_annotated_type (env : env) (x : CST.annotated_type) =
  (match x with
  | `Anno_type_ (v1, v2) -> R.Case ("Anno_type_",
      let v1 = map_simple_type env v1 in
      let v2 = R.List (List.map (map_annotation env) v2) in
      R.Tuple [v1; v2]
    )
  | `Simple_type x -> R.Case ("Simple_type",
      map_simple_type env x
    )
  )

and map_annotation (env : env) ((v1, v2, v3) : CST.annotation) =
  let v1 = (* "@" *) token env v1 in
  let v2 = map_simple_type env v2 in
  let v3 = R.List (List.map (map_arguments env) v3) in
  R.Tuple [v1; v2; v3]

and map_anon_choice_dollar_esc_fba2882 (env : env) (x : CST.anon_choice_dollar_esc_fba2882) =
  (match x with
  | `Dollar_esc x -> R.Case ("Dollar_esc",
      map_dollar_escape env x
    )
  | `Interp x -> R.Case ("Interp",
      map_interpolation env x
    )
  )

and map_anon_choice_enum_case_defins_b7955e9 (env : env) (x : CST.anon_choice_enum_case_defins_b7955e9) =
  (match x with
  | `Enum_case_defins (v1, v2, v3) -> R.Case ("Enum_case_defins",
      let v1 = R.List (List.map (map_annotation env) v1) in
      let v2 = (* "case" *) token env v2 in
      let v3 =
        (match v3 with
        | `Simple_enum_case_rep_COMMA_simple_enum_case (v1, v2) -> R.Case ("Simple_enum_case_rep_COMMA_simple_enum_case",
            let v1 = map_simple_enum_case env v1 in
            let v2 =
              R.List (List.map (fun (v1, v2) ->
                let v1 = (* "," *) token env v1 in
                let v2 = map_simple_enum_case env v2 in
                R.Tuple [v1; v2]
              ) v2)
            in
            R.Tuple [v1; v2]
          )
        | `Full_enum_case x -> R.Case ("Full_enum_case",
            map_full_enum_case env x
          )
        )
      in
      R.Tuple [v1; v2; v3]
    )
  | `Exp x -> R.Case ("Exp",
      map_expression env x
    )
  | `Choice_choice_given_defi x -> R.Case ("Choice_choice_given_defi",
      map_definition env x
    )
  )

and map_anon_choice_exp_5763a53 (env : env) (x : CST.anon_choice_exp_5763a53) =
  (match x with
  | `Exp x -> R.Case ("Exp",
      map_expression env x
    )
  | `Choice_choice_given_defi x -> R.Case ("Choice_choice_given_defi",
      map_definition env x
    )
  | `End_marker x -> R.Case ("End_marker",
      map_end_marker env x
    )
  | `SEMI tok -> R.Case ("SEMI",
      (* ";" *) token env tok
    )
  )

and map_anon_choice_infix_exp_dc476f6 (env : env) (x : CST.anon_choice_infix_exp_dc476f6) =
  (match x with
  | `Infix_exp x -> R.Case ("Infix_exp",
      map_infix_expression env x
    )
  | `Prefix_exp x -> R.Case ("Prefix_exp",
      map_prefix_expression env x
    )
  | `Simple_exp x -> R.Case ("Simple_exp",
      map_simple_expression env x
    )
  )

and map_anon_choice_name_given_by_type_ca66fd5 (env : env) (x : CST.anon_choice_name_given_by_type_ca66fd5) =
  (match x with
  | `Name_given_by_type (v1, v2) -> R.Case ("Name_given_by_type",
      let v1 = (* "given" *) token env v1 in
      let v2 = map_type_ env v2 in
      R.Tuple [v1; v2]
    )
  | `Name_wild x -> R.Case ("Name_wild",
      map_namespace_wildcard env x
    )
  | `Choice_id x -> R.Case ("Choice_id",
      map_type_identifier env x
    )
  | `Arrow_rena_id (v1, v2, v3) -> R.Case ("Arrow_rena_id",
      let v1 = map_type_identifier env v1 in
      let v2 = (* "=>" *) token env v2 in
      let v3 = map_anon_choice_type_id_0797463 env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `As_rena_id x -> R.Case ("As_rena_id",
      map_as_renamed_identifier env x
    )
  )

and map_anon_choice_pat_a6d147b (env : env) (x : CST.anon_choice_pat_a6d147b) =
  (match x with
  | `Choice_choice_choice_id x -> R.Case ("Choice_choice_choice_id",
      map_pattern env x
    )
  | `Idents (v1, v2, v3, v4) -> R.Case ("Idents",
      let v1 = map_identifier env v1 in
      let v2 = (* "," *) token env v2 in
      let v3 = map_identifier env v3 in
      let v4 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = (* "," *) token env v1 in
          let v2 = map_identifier env v2 in
          R.Tuple [v1; v2]
        ) v4)
      in
      R.Tuple [v1; v2; v3; v4]
    )
  )

and map_anon_param_rep_COMMA_param_opt_COMMA_bde8b1d (env : env) ((v1, v2, v3) : CST.anon_param_rep_COMMA_param_opt_COMMA_bde8b1d) =
  let v1 = map_parameter env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_parameter env v2 in
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

and map_anon_param_type_rep_COMMA_param_type_opt_COMMA_dbcd678 (env : env) ((v1, v2, v3) : CST.anon_param_type_rep_COMMA_param_type_opt_COMMA_dbcd678) =
  let v1 = map_param_type env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_param_type env v2 in
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

and map_arguments (env : env) ((v1, v2, v3) : CST.arguments) =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    (match v2 with
    | `Opt_exprs_in_parens opt -> R.Case ("Opt_exprs_in_parens",
        (match opt with
        | Some x -> R.Option (Some (
            map_exprs_in_parens env x
          ))
        | None -> R.Option None)
      )
    | `Using_exprs_in_parens (v1, v2) -> R.Case ("Using_exprs_in_parens",
        let v1 = (* "using" *) token env v1 in
        let v2 = map_exprs_in_parens env v2 in
        R.Tuple [v1; v2]
      )
    )
  in
  let v3 = (* ")" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_arrow_then_type (env : env) ((v1, v2) : CST.arrow_then_type) =
  let v1 = map_anon_choice_EQGT_ce418c1 env v1 in
  let v2 = map_type_ env v2 in
  R.Tuple [v1; v2]

and map_binding (env : env) ((v1, v2) : CST.binding) =
  let v1 = map_anon_choice_type_id_0797463 env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = (* ":" *) token env v1 in
        let v2 = map_param_type env v2 in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2]

and map_bindings (env : env) ((v1, v2, v3) : CST.bindings) =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2, v3) -> R.Option (Some (
        let v1 = map_binding env v1 in
        let v2 =
          R.List (List.map (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_binding env v2 in
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
  let v3 = (* ")" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_block (env : env) ((v1, v2, v3) : CST.block) =
  let v1 = map_anon_choice_exp_5763a53 env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = map_semicolon env v1 in
      let v2 = map_anon_choice_exp_5763a53 env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_semicolon env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3]

and map_block_ (env : env) ((v1, v2, v3) : CST.block_) =
  let v1 = (* "{" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_block env x
      ))
    | None -> R.Option None)
  in
  let v3 = (* "}" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_braced_template_body1 (env : env) ((v1, v2) : CST.braced_template_body1) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        map_self_type env x
      ))
    | None -> R.Option None)
  in
  let v2 = map_block env v2 in
  R.Tuple [v1; v2]

and map_braced_template_body2 (env : env) ((v1, v2, v3) : CST.braced_template_body2) =
  let v1 =
    (match v1 with
    | `Indent_opt_self_type (v1, v2) -> R.Case ("Indent_opt_self_type",
        let v1 = (* indent *) token env v1 in
        let v2 =
          (match v2 with
          | Some x -> R.Option (Some (
              map_self_type env x
            ))
          | None -> R.Option None)
        in
        R.Tuple [v1; v2]
      )
    | `Opt_self_type_indent (v1, v2) -> R.Case ("Opt_self_type_indent",
        let v1 =
          (match v1 with
          | Some x -> R.Option (Some (
              map_self_type env x
            ))
          | None -> R.Option None)
        in
        let v2 = (* indent *) token env v2 in
        R.Tuple [v1; v2]
      )
    )
  in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_block env x
      ))
    | None -> R.Option None)
  in
  let v3 = (* outdent *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_call_expression (env : env) (x : CST.call_expression) =
  (match x with
  | `Simple_exp_choice_args (v1, v2) -> R.Case ("Simple_exp_choice_args",
      let v1 = map_simple_expression env v1 in
      let v2 =
        (match v2 with
        | `Args x -> R.Case ("Args",
            map_arguments env x
          )
        | `Case_blk x -> R.Case ("Case_blk",
            map_case_block env x
          )
        | `Blk_ x -> R.Case ("Blk_",
            map_block_ env x
          )
        )
      in
      R.Tuple [v1; v2]
    )
  | `Post_exp_choice_COLON_colon_arg (v1, v2, v3) -> R.Case ("Post_exp_choice_COLON_colon_arg",
      let v1 = map_postfix_expression_choice env v1 in
      let v2 = (* ":" *) token env v2 in
      let v3 = map_colon_argument env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_capture_pattern (env : env) ((v1, v2, v3) : CST.capture_pattern) =
  let v1 = map_anon_choice_type_id_0797463 env v1 in
  let v2 = (* "@" *) token env v2 in
  let v3 = map_pattern env v3 in
  R.Tuple [v1; v2; v3]

and map_case_block (env : env) (x : CST.case_block) =
  (match x with
  | `LCURL_RCURL (v1, v2) -> R.Case ("LCURL_RCURL",
      let v1 = (* "{" *) token env v1 in
      let v2 = (* "}" *) token env v2 in
      R.Tuple [v1; v2]
    )
  | `LCURL_rep1_case_clause_RCURL (v1, v2, v3) -> R.Case ("LCURL_rep1_case_clause_RCURL",
      let v1 = (* "{" *) token env v1 in
      let v2 = R.List (List.map (map_case_clause env) v2) in
      let v3 = (* "}" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_case_class_pattern (env : env) ((v1, v2, v3, v4) : CST.case_class_pattern) =
  let v1 = map_anon_choice_type_id_ae98204 env v1 in
  let v2 = (* "(" *) token env v2 in
  let v3 =
    (match v3 with
    | `Opt_choice_choice_choice_id_rep_COMMA_choice_choice_choice_id_opt_COMMA opt -> R.Case ("Opt_choice_choice_choice_id_rep_COMMA_choice_choice_choice_id_opt_COMMA",
        (match opt with
        | Some (v1, v2, v3) -> R.Option (Some (
            let v1 = map_pattern env v1 in
            let v2 =
              R.List (List.map (fun (v1, v2) ->
                let v1 = (* "," *) token env v1 in
                let v2 = map_pattern env v2 in
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
      )
    | `Opt_named_pat_rep_COMMA_named_pat_opt_COMMA opt -> R.Case ("Opt_named_pat_rep_COMMA_named_pat_opt_COMMA",
        (match opt with
        | Some (v1, v2, v3) -> R.Option (Some (
            let v1 = map_named_pattern env v1 in
            let v2 =
              R.List (List.map (fun (v1, v2) ->
                let v1 = (* "," *) token env v1 in
                let v2 = map_named_pattern env v2 in
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
      )
    )
  in
  let v4 = (* ")" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_case_clause (env : env) ((v1, v2, v3) : CST.case_clause) =
  let v1 = (* "case" *) token env v1 in
  let v2 = map_case_pattern env v2 in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_block env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3]

and map_case_pattern (env : env) ((v1, v2, v3) : CST.case_pattern) =
  let v1 = map_pattern env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_guard env x
      ))
    | None -> R.Option None)
  in
  let v3 = (* "=>" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_catch_clause (env : env) ((v1, v2) : CST.catch_clause) =
  let v1 = (* "catch" *) token env v1 in
  let v2 =
    (match v2 with
    | `Inde_exp x -> R.Case ("Inde_exp",
        map_indentable_expression env x
      )
    | `Expr_case_clause x -> R.Case ("Expr_case_clause",
        map_expr_case_clause env x
      )
    )
  in
  R.Tuple [v1; v2]

and map_class_constructor (env : env) ((v1, v2, v3, v4, v5) : CST.class_constructor) =
  let v1 = map_type_identifier env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_type_parameters env x
      ))
    | None -> R.Option None)
  in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_constructor_annotation env x
      ))
    | None -> R.Option None)
  in
  let v4 =
    (match v4 with
    | Some x -> R.Option (Some (
        map_access_modifier env x
      ))
    | None -> R.Option None)
  in
  let v5 =
    R.List (List.map (fun (v1, v2) ->
      let v1 =
        (match v1 with
        | Some tok -> R.Option (Some (
            (* automatic_semicolon *) token env tok
          ))
        | None -> R.Option None)
      in
      let v2 = map_class_parameters env v2 in
      R.Tuple [v1; v2]
    ) v5)
  in
  R.Tuple [v1; v2; v3; v4; v5]

and map_class_definition (env : env) ((v1, v2, v3, v4, v5) : CST.class_definition) =
  let v1 = R.List (List.map (map_annotation env) v1) in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_modifiers env x
      ))
    | None -> R.Option None)
  in
  let v3 =
    (match v3 with
    | Some tok -> R.Option (Some (
        (* "case" *) token env tok
      ))
    | None -> R.Option None)
  in
  let v4 = (* "class" *) token env v4 in
  let v5 = map_class_definition_ env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

and map_class_definition_ (env : env) ((v1, v2, v3, v4) : CST.class_definition_) =
  let v1 = map_class_constructor env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_extends_clause env x
      ))
    | None -> R.Option None)
  in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_derives_clause env x
      ))
    | None -> R.Option None)
  in
  let v4 =
    (match v4 with
    | Some x -> R.Option (Some (
        map_definition_body env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3; v4]

and map_class_parameter (env : env) (x : CST.class_parameter) =
  (match x with
  | `Rep_anno_opt_modifs_opt_choice_val_choice_id_opt_COLON_choice_type_opt_EQ_exp (v1, v2, v3, v4, v5, v6) -> R.Case ("Rep_anno_opt_modifs_opt_choice_val_choice_id_opt_COLON_choice_type_opt_EQ_exp",
      let v1 = R.List (List.map (map_annotation env) v1) in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_modifiers env x
          ))
        | None -> R.Option None)
      in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            (match x with
            | `Val tok -> R.Case ("Val",
                (* "val" *) token env tok
              )
            | `Var tok -> R.Case ("Var",
                (* "var" *) token env tok
              )
            )
          ))
        | None -> R.Option None)
      in
      let v4 = map_type_identifier env v4 in
      let v5 =
        (match v5 with
        | Some (v1, v2) -> R.Option (Some (
            let v1 = (* ":" *) token env v1 in
            let v2 = map_param_type env v2 in
            R.Tuple [v1; v2]
          ))
        | None -> R.Option None)
      in
      let v6 =
        (match v6 with
        | Some (v1, v2) -> R.Option (Some (
            let v1 = (* "=" *) token env v1 in
            let v2 = map_expression env v2 in
            R.Tuple [v1; v2]
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3; v4; v5; v6]
    )
  | `Semg_ellips tok -> R.Case ("Semg_ellips",
      (* semgrep_ellipsis *) token env tok
    )
  )

and map_class_parameters (env : env) ((v1, v2, v3, v4, v5) : CST.class_parameters) =
  let v1 =
    (match v1 with
    | Some tok -> R.Option (Some (
        (* automatic_semicolon *) token env tok
      ))
    | None -> R.Option None)
  in
  let v2 = (* "(" *) token env v2 in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        (match x with
        | `Impl tok -> R.Case ("Impl",
            (* "implicit" *) token env tok
          )
        | `Using tok -> R.Case ("Using",
            (* "using" *) token env tok
          )
        )
      ))
    | None -> R.Option None)
  in
  let v4 =
    (match v4 with
    | Some (v1, v2, v3) -> R.Option (Some (
        let v1 = map_class_parameter env v1 in
        let v2 =
          R.List (List.map (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_class_parameter env v2 in
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
  let v5 = (* ")" *) token env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

and map_colon_argument (env : env) ((v1, v2) : CST.colon_argument) =
  let v1 =
    (match v1 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 =
          (match v1 with
          | `Bindis x -> R.Case ("Bindis",
              map_bindings env x
            )
          | `Choice_id x -> R.Case ("Choice_id",
              map_type_identifier env x
            )
          | `Wild tok -> R.Case ("Wild",
              (* "_" *) token env tok
            )
          )
        in
        let v2 = (* "=>" *) token env v2 in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  let v2 =
    (match v2 with
    | `Inde_blk x -> R.Case ("Inde_blk",
        map_indented_block env x
      )
    | `Inde_cases x -> R.Case ("Inde_cases",
        map_indented_cases env x
      )
    )
  in
  R.Tuple [v1; v2]

and map_compound_type (env : env) (x : CST.compound_type) =
  (match x with
  | `Anno_type_rep1_with_anno_type (v1, v2) -> R.Case ("Anno_type_rep1_with_anno_type",
      let v1 = map_annotated_type env v1 in
      let v2 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = (* "with" *) token env v1 in
          let v2 = map_annotated_type env v2 in
          R.Tuple [v1; v2]
        ) v2)
      in
      R.Tuple [v1; v2]
    )
  | `Anno_type_refi (v1, v2) -> R.Case ("Anno_type_refi",
      let v1 = map_annotated_type env v1 in
      let v2 = map_refinement env v2 in
      R.Tuple [v1; v2]
    )
  | `Anno_type_rep1_with_anno_type_refi (v1, v2, v3) -> R.Case ("Anno_type_rep1_with_anno_type_refi",
      let v1 = map_annotated_type env v1 in
      let v2 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = (* "with" *) token env v1 in
          let v2 = map_annotated_type env v2 in
          R.Tuple [v1; v2]
        ) v2)
      in
      let v3 = map_refinement env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_constructor_annotation (env : env) ((v1, v2, v3) : CST.constructor_annotation) =
  let v1 = (* "@" *) token env v1 in
  let v2 = map_simple_type env v2 in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_arguments env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3]

and map_constructor_application (env : env) (x : CST.constructor_application) =
  (match x with
  | `Anno_type x -> R.Case ("Anno_type",
      map_annotated_type env x
    )
  | `Comp_type x -> R.Case ("Comp_type",
      map_compound_type env x
    )
  | `Stru_type x -> R.Case ("Stru_type",
      map_structural_type env x
    )
  | `Simple_type_args (v1, v2) -> R.Case ("Simple_type_args",
      let v1 = map_simple_type env v1 in
      let v2 = map_arguments env v2 in
      R.Tuple [v1; v2]
    )
  | `Anno_type_args (v1, v2) -> R.Case ("Anno_type_args",
      let v1 = map_annotated_type env v1 in
      let v2 = map_arguments env v2 in
      R.Tuple [v1; v2]
    )
  | `Comp_type_args (v1, v2) -> R.Case ("Comp_type_args",
      let v1 = map_compound_type env v1 in
      let v2 = map_arguments env v2 in
      R.Tuple [v1; v2]
    )
  )

and map_constructor_applications (env : env) (x : CST.constructor_applications) =
  (match x with
  | `Cons_app_rep_COMMA_cons_app (v1, v2) -> R.Case ("Cons_app_rep_COMMA_cons_app",
      let v1 = map_constructor_application env v1 in
      let v2 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = (* "," *) token env v1 in
          let v2 = map_constructor_application env v2 in
          R.Tuple [v1; v2]
        ) v2)
      in
      R.Tuple [v1; v2]
    )
  | `Cons_app_rep_with_cons_app (v1, v2) -> R.Case ("Cons_app_rep_with_cons_app",
      let v1 = map_constructor_application env v1 in
      let v2 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = (* "with" *) token env v1 in
          let v2 = map_constructor_application env v2 in
          R.Tuple [v1; v2]
        ) v2)
      in
      R.Tuple [v1; v2]
    )
  )

and map_context_bound (env : env) ((v1, v2) : CST.context_bound) =
  let v1 = map_type_ env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = (* "as" *) token env v1 in
        let v2 = map_type_identifier env v2 in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2]

and map_context_bounds (env : env) (x : CST.context_bounds) =
  (match x with
  | `Rep1_COLON_cont_bound xs -> R.Case ("Rep1_COLON_cont_bound",
      R.List (List.map (fun (v1, v2) ->
        let v1 = (* ":" *) token env v1 in
        let v2 = map_context_bound env v2 in
        R.Tuple [v1; v2]
      ) xs)
    )
  | `COLON_LCURL_cont_bound_rep_COMMA_cont_bound_opt_COMMA_RCURL (v1, v2, v3, v4, v5, v6) -> R.Case ("COLON_LCURL_cont_bound_rep_COMMA_cont_bound_opt_COMMA_RCURL",
      let v1 = (* ":" *) token env v1 in
      let v2 = (* "{" *) token env v2 in
      let v3 = map_context_bound env v3 in
      let v4 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = (* "," *) token env v1 in
          let v2 = map_context_bound env v2 in
          R.Tuple [v1; v2]
        ) v4)
      in
      let v5 =
        (match v5 with
        | Some tok -> R.Option (Some (
            (* "," *) token env tok
          ))
        | None -> R.Option None)
      in
      let v6 = (* "}" *) token env v6 in
      R.Tuple [v1; v2; v3; v4; v5; v6]
    )
  )

and map_contravariant_type_parameter (env : env) ((v1, v2) : CST.contravariant_type_parameter) =
  let v1 = (* "-" *) token env v1 in
  let v2 = map_type_parameter env v2 in
  R.Tuple [v1; v2]

and map_covariant_type_parameter (env : env) ((v1, v2) : CST.covariant_type_parameter) =
  let v1 = (* "+" *) token env v1 in
  let v2 = map_type_parameter env v2 in
  R.Tuple [v1; v2]

and map_definition (env : env) (x : CST.definition) =
  (match x with
  | `Choice_given_defi x -> R.Case ("Choice_given_defi",
      (match x with
      | `Given_defi x -> R.Case ("Given_defi",
          map_given_definition env x
        )
      | `Exte_defi x -> R.Case ("Exte_defi",
          map_extension_definition env x
        )
      | `Class_defi x -> R.Case ("Class_defi",
          map_class_definition env x
        )
      | `Import_decl x -> R.Case ("Import_decl",
          map_import_declaration env x
        )
      | `Export_decl x -> R.Case ("Export_decl",
          map_export_declaration env x
        )
      | `Obj_defi x -> R.Case ("Obj_defi",
          map_object_definition env x
        )
      | `Enum_defi x -> R.Case ("Enum_defi",
          map_enum_definition env x
        )
      | `Trait_defi x -> R.Case ("Trait_defi",
          map_trait_definition env x
        )
      | `Val_defi x -> R.Case ("Val_defi",
          map_val_definition env x
        )
      | `Val_decl x -> R.Case ("Val_decl",
          map_val_declaration env x
        )
      | `Var_defi x -> R.Case ("Var_defi",
          map_var_definition env x
        )
      | `Var_decl x -> R.Case ("Var_decl",
          map_var_declaration env x
        )
      | `Type_defi x -> R.Case ("Type_defi",
          map_type_definition env x
        )
      | `Func_defi x -> R.Case ("Func_defi",
          map_function_definition env x
        )
      | `Func_decl x -> R.Case ("Func_decl",
          map_function_declaration env x
        )
      | `Pack_clause x -> R.Case ("Pack_clause",
          map_package_clause env x
        )
      | `Pack_obj x -> R.Case ("Pack_obj",
          map_package_object env x
        )
      )
    )
  | `Semg_val_or_var_defi (v1, v2, v3, v4, v5) -> R.Case ("Semg_val_or_var_defi",
      let v1 = (* semgrep_metavariable *) token env v1 in
      let v2 = map_anon_choice_pat_a6d147b env v2 in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_self_type_ascription env x
          ))
        | None -> R.Option None)
      in
      let v4 = (* "=" *) token env v4 in
      let v5 = map_indentable_expression env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  )

and map_definition_body (env : env) ((v1, v2) : CST.definition_body) =
  let v1 =
    (match v1 with
    | Some tok -> R.Option (Some (
        (* automatic_semicolon *) token env tok
      ))
    | None -> R.Option None)
  in
  let v2 = map_structural_type env v2 in
  R.Tuple [v1; v2]

and map_enum_block (env : env) ((v1, v2, v3) : CST.enum_block) =
  let v1 = map_anon_choice_enum_case_defins_b7955e9 env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = map_semicolon env v1 in
      let v2 = map_anon_choice_enum_case_defins_b7955e9 env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_semicolon env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3]

and map_enum_body (env : env) (x : CST.enum_body) =
  (match x with
  | `COLON_indent_enum_blk_outd (v1, v2, v3, v4) -> R.Case ("COLON_indent_enum_blk_outd",
      let v1 = (* ":" *) token env v1 in
      let v2 = (* indent *) token env v2 in
      let v3 = map_enum_block env v3 in
      let v4 = (* outdent *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `LCURL_opt_enum_blk_RCURL (v1, v2, v3) -> R.Case ("LCURL_opt_enum_blk_RCURL",
      let v1 = (* "{" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_enum_block env x
          ))
        | None -> R.Option None)
      in
      let v3 = (* "}" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_enum_definition (env : env) ((v1, v2, v3, v4, v5, v6) : CST.enum_definition) =
  let v1 = R.List (List.map (map_annotation env) v1) in
  let v2 = (* "enum" *) token env v2 in
  let v3 = map_class_constructor env v3 in
  let v4 =
    (match v4 with
    | Some x -> R.Option (Some (
        map_extends_clause env x
      ))
    | None -> R.Option None)
  in
  let v5 =
    (match v5 with
    | Some x -> R.Option (Some (
        map_derives_clause env x
      ))
    | None -> R.Option None)
  in
  let v6 = map_enum_body env v6 in
  R.Tuple [v1; v2; v3; v4; v5; v6]

and map_enumerator (env : env) (x : CST.enumerator) =
  (match x with
  | `Choice_opt_case_choice_choice_choice_id_choice_LTDASH_exp_opt_guard x -> R.Case ("Choice_opt_case_choice_choice_choice_id_choice_LTDASH_exp_opt_guard",
      (match x with
      | `Opt_case_choice_choice_choice_id_choice_LTDASH_exp_opt_guard (v1, v2, v3, v4, v5) -> R.Case ("Opt_case_choice_choice_choice_id_choice_LTDASH_exp_opt_guard",
          let v1 =
            (match v1 with
            | Some tok -> R.Option (Some (
                (* "case" *) token env tok
              ))
            | None -> R.Option None)
          in
          let v2 = map_pattern env v2 in
          let v3 =
            (match v3 with
            | `LTDASH tok -> R.Case ("LTDASH",
                (* "<-" *) token env tok
              )
            | `EQ tok -> R.Case ("EQ",
                (* "=" *) token env tok
              )
            )
          in
          let v4 = map_expression env v4 in
          let v5 =
            (match v5 with
            | Some x -> R.Option (Some (
                map_guard env x
              ))
            | None -> R.Option None)
          in
          R.Tuple [v1; v2; v3; v4; v5]
        )
      | `Rep1_guard xs -> R.Case ("Rep1_guard",
          R.List (List.map (map_guard env) xs)
        )
      )
    )
  | `Semg_ellips tok -> R.Case ("Semg_ellips",
      (* semgrep_ellipsis *) token env tok
    )
  )

and map_enumerators (env : env) (x : CST.enumerators) =
  (match x with
  | `Enum_rep_choice_SEMI_enum_opt_auto_semi (v1, v2, v3) -> R.Case ("Enum_rep_choice_SEMI_enum_opt_auto_semi",
      let v1 = map_enumerator env v1 in
      let v2 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = map_semicolon env v1 in
          let v2 = map_enumerator env v2 in
          R.Tuple [v1; v2]
        ) v2)
      in
      let v3 =
        (match v3 with
        | Some tok -> R.Option (Some (
            (* automatic_semicolon *) token env tok
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3]
    )
  | `Indent_enum_rep_choice_SEMI_enum_opt_auto_semi_outd (v1, v2, v3, v4, v5) -> R.Case ("Indent_enum_rep_choice_SEMI_enum_opt_auto_semi_outd",
      let v1 = (* indent *) token env v1 in
      let v2 = map_enumerator env v2 in
      let v3 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = map_semicolon env v1 in
          let v2 = map_enumerator env v2 in
          R.Tuple [v1; v2]
        ) v3)
      in
      let v4 =
        (match v4 with
        | Some tok -> R.Option (Some (
            (* automatic_semicolon *) token env tok
          ))
        | None -> R.Option None)
      in
      let v5 = (* outdent *) token env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  )

and map_export_declaration (env : env) ((v1, v2, v3) : CST.export_declaration) =
  let v1 = (* "export" *) token env v1 in
  let v2 = map_namespace_expression env v2 in
  let v3 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_namespace_expression env v2 in
      R.Tuple [v1; v2]
    ) v3)
  in
  R.Tuple [v1; v2; v3]

and map_expr_case_clause (env : env) ((v1, v2, v3) : CST.expr_case_clause) =
  let v1 = (* "case" *) token env v1 in
  let v2 = map_case_pattern env v2 in
  let v3 = map_expression env v3 in
  R.Tuple [v1; v2; v3]

and map_expression (env : env) (x : CST.expression) =
  (match x with
  | `If_exp (v1, v2, v3, v4, v5) -> R.Case ("If_exp",
      let v1 =
        (match v1 with
        | Some tok -> R.Option (Some (
            (* "inline" *) token env tok
          ))
        | None -> R.Option None)
      in
      let v2 = (* "if" *) token env v2 in
      let v3 = map_if_condition env v3 in
      let v4 = map_indentable_expression env v4 in
      let v5 =
        (match v5 with
        | Some (v1, v2, v3) -> R.Option (Some (
            let v1 =
              (match v1 with
              | Some tok -> R.Option (Some (
                  (* ";" *) token env tok
                ))
              | None -> R.Option None)
            in
            let v2 = (* "else" *) token env v2 in
            let v3 = map_indentable_expression env v3 in
            R.Tuple [v1; v2; v3]
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `Match_exp (v1, v2, v3, v4) -> R.Case ("Match_exp",
      let v1 =
        (match v1 with
        | Some tok -> R.Option (Some (
            (* "inline" *) token env tok
          ))
        | None -> R.Option None)
      in
      let v2 = map_expression env v2 in
      let v3 = (* "match" *) token env v3 in
      let v4 =
        (match v4 with
        | `Case_blk x -> R.Case ("Case_blk",
            map_case_block env x
          )
        | `Inde_cases x -> R.Case ("Inde_cases",
            map_indented_cases env x
          )
        )
      in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Try_exp (v1, v2, v3, v4) -> R.Case ("Try_exp",
      let v1 = (* "try" *) token env v1 in
      let v2 = map_indentable_expression env v2 in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_catch_clause env x
          ))
        | None -> R.Option None)
      in
      let v4 =
        (match v4 with
        | Some x -> R.Option (Some (
            map_finally_clause env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Assign_exp (v1, v2, v3) -> R.Case ("Assign_exp",
      let v1 =
        (match v1 with
        | `Prefix_exp x -> R.Case ("Prefix_exp",
            map_prefix_expression env x
          )
        | `Simple_exp x -> R.Case ("Simple_exp",
            map_simple_expression env x
          )
        )
      in
      let v2 = (* "=" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Lambda_exp (v1, v2, v3, v4) -> R.Case ("Lambda_exp",
      let v1 =
        (match v1 with
        | Some (v1, v2) -> R.Option (Some (
            let v1 = map_type_parameters env v1 in
            let v2 = (* "=>" *) token env v2 in
            R.Tuple [v1; v2]
          ))
        | None -> R.Option None)
      in
      let v2 =
        (match v2 with
        | `Bindis x -> R.Case ("Bindis",
            map_bindings env x
          )
        | `Opt_impl_choice_id (v1, v2) -> R.Case ("Opt_impl_choice_id",
            let v1 =
              (match v1 with
              | Some tok -> R.Option (Some (
                  (* "implicit" *) token env tok
                ))
              | None -> R.Option None)
            in
            let v2 = map_type_identifier env v2 in
            R.Tuple [v1; v2]
          )
        | `Wild tok -> R.Case ("Wild",
            (* "_" *) token env tok
          )
        )
      in
      let v3 = map_anon_choice_EQGT_ce418c1 env v3 in
      let v4 = map_indentable_expression env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Post_exp x -> R.Case ("Post_exp",
      map_postfix_expression env x
    )
  | `Ascr_exp (v1, v2, v3) -> R.Case ("Ascr_exp",
      let v1 = map_postfix_expression_choice env v1 in
      let v2 = (* ":" *) token env v2 in
      let v3 =
        (match v3 with
        | `Choice_type x -> R.Case ("Choice_type",
            map_param_type env x
          )
        | `Anno x -> R.Case ("Anno",
            map_annotation env x
          )
        )
      in
      R.Tuple [v1; v2; v3]
    )
  | `Infix_exp x -> R.Case ("Infix_exp",
      map_infix_expression env x
    )
  | `Prefix_exp x -> R.Case ("Prefix_exp",
      map_prefix_expression env x
    )
  | `Ret_exp (v1, v2) -> R.Case ("Ret_exp",
      let v1 = (* "return" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_expression env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2]
    )
  | `Throw_exp (v1, v2) -> R.Case ("Throw_exp",
      let v1 = (* "throw" *) token env v1 in
      let v2 = map_expression env v2 in
      R.Tuple [v1; v2]
    )
  | `While_exp x -> R.Case ("While_exp",
      map_while_expression env x
    )
  | `Do_while_exp (v1, v2, v3, v4) -> R.Case ("Do_while_exp",
      let v1 = (* "do" *) token env v1 in
      let v2 = map_expression env v2 in
      let v3 = (* "while" *) token env v3 in
      let v4 = map_parenthesized_expression env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `For_exp x -> R.Case ("For_exp",
      map_for_expression env x
    )
  | `Macro_body (v1, v2) -> R.Case ("Macro_body",
      let v1 = (* "macro" *) token env v1 in
      let v2 = map_anon_choice_infix_exp_dc476f6 env v2 in
      R.Tuple [v1; v2]
    )
  | `Simple_exp x -> R.Case ("Simple_exp",
      map_simple_expression env x
    )
  )

and map_exprs_in_parens (env : env) ((v1, v2, v3) : CST.exprs_in_parens) =
  let v1 = map_expression env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_expression env v2 in
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

and map_extends_clause (env : env) ((v1, v2, v3) : CST.extends_clause) =
  let v1 = (* "extends" *) token env v1 in
  let v2 = map_constructor_applications env v2 in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_arguments env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3]

and map_extension_definition (env : env) ((v1, v2, v3, v4) : CST.extension_definition) =
  let v1 = (* "extension" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_type_parameters env x
      ))
    | None -> R.Option None)
  in
  let v3 = R.List (List.map (map_given_conditional env) v3) in
  let v4 =
    (match v4 with
    | `Exte_temp_body x -> R.Case ("Exte_temp_body",
        map_extension_template_body env x
      )
    | `Func_defi x -> R.Case ("Func_defi",
        map_function_definition env x
      )
    | `Func_decl x -> R.Case ("Func_decl",
        map_function_declaration env x
      )
    )
  in
  R.Tuple [v1; v2; v3; v4]

and map_extension_template_body (env : env) (x : CST.extension_template_body) =
  (match x with
  | `Indent_blk_outd (v1, v2, v3) -> R.Case ("Indent_blk_outd",
      let v1 = (* indent *) token env v1 in
      let v2 = map_block env v2 in
      let v3 = (* outdent *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `LCURL_opt_blk_RCURL x -> R.Case ("LCURL_opt_blk_RCURL",
      map_block_ env x
    )
  )

and map_field_expression (env : env) ((v1, v2, v3) : CST.field_expression) =
  let v1 = map_simple_expression env v1 in
  let v2 = (* "." *) token env v2 in
  let v3 = map_type_identifier env v3 in
  R.Tuple [v1; v2; v3]

and map_finally_clause (env : env) ((v1, v2) : CST.finally_clause) =
  let v1 = (* "finally" *) token env v1 in
  let v2 = map_indentable_expression env v2 in
  R.Tuple [v1; v2]

and map_for_expression (env : env) (x : CST.for_expression) =
  (match x with
  | `For_choice_LPAR_enumes_RPAR_choice_exp (v1, v2, v3) -> R.Case ("For_choice_LPAR_enumes_RPAR_choice_exp",
      let v1 = (* "for" *) token env v1 in
      let v2 =
        (match v2 with
        | `LPAR_enumes_RPAR (v1, v2, v3) -> R.Case ("LPAR_enumes_RPAR",
            let v1 = (* "(" *) token env v1 in
            let v2 = map_enumerators env v2 in
            let v3 = (* ")" *) token env v3 in
            R.Tuple [v1; v2; v3]
          )
        | `LCURL_enumes_RCURL (v1, v2, v3) -> R.Case ("LCURL_enumes_RCURL",
            let v1 = (* "{" *) token env v1 in
            let v2 = map_enumerators env v2 in
            let v3 = (* "}" *) token env v3 in
            R.Tuple [v1; v2; v3]
          )
        )
      in
      let v3 =
        (match v3 with
        | `Exp v1 -> R.Case ("Exp",
            map_expression env v1
          )
        | `Yield_inde_exp (v1, v2) -> R.Case ("Yield_inde_exp",
            let v1 = (* "yield" *) token env v1 in
            let v2 = map_indentable_expression env v2 in
            R.Tuple [v1; v2]
          )
        )
      in
      R.Tuple [v1; v2; v3]
    )
  | `For_enumes_choice_do_inde_exp (v1, v2, v3) -> R.Case ("For_enumes_choice_do_inde_exp",
      let v1 = (* "for" *) token env v1 in
      let v2 = map_enumerators env v2 in
      let v3 =
        (match v3 with
        | `Do_inde_exp (v1, v2) -> R.Case ("Do_inde_exp",
            let v1 = (* "do" *) token env v1 in
            let v2 = map_indentable_expression env v2 in
            R.Tuple [v1; v2]
          )
        | `Yield_inde_exp (v1, v2) -> R.Case ("Yield_inde_exp",
            let v1 = (* "yield" *) token env v1 in
            let v2 = map_indentable_expression env v2 in
            R.Tuple [v1; v2]
          )
        )
      in
      R.Tuple [v1; v2; v3]
    )
  )

and map_full_enum_case (env : env) ((v1, v2) : CST.full_enum_case) =
  let v1 = map_type_identifier env v1 in
  let v2 = map_full_enum_def env v2 in
  R.Tuple [v1; v2]

and map_full_enum_def (env : env) ((v1, v2, v3) : CST.full_enum_def) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        map_type_parameters env x
      ))
    | None -> R.Option None)
  in
  let v2 = R.List (List.map (map_class_parameters env) v2) in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_extends_clause env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3]

and map_function_constructor (env : env) ((v1, v2, v3) : CST.function_constructor) =
  let v1 = map_type_identifier env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 =
        (match v1 with
        | Some tok -> R.Option (Some (
            (* automatic_semicolon *) token env tok
          ))
        | None -> R.Option None)
      in
      let v2 =
        (match v2 with
        | `Params x -> R.Case ("Params",
            map_given_conditional env x
          )
        | `Type_params x -> R.Case ("Type_params",
            map_type_parameters env x
          )
        )
      in
      R.Tuple [v1; v2]
    ) v2)
  in
  let v3 =
    (match v3 with
    | Some tok -> R.Option (Some (
        (* automatic_semicolon *) token env tok
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3]

and map_function_declaration (env : env) (x : CST.function_declaration) =
  map_function_declaration_ env x

and map_function_declaration_ (env : env) ((v1, v2, v3, v4, v5) : CST.function_declaration_) =
  let v1 = R.List (List.map (map_annotation env) v1) in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_modifiers env x
      ))
    | None -> R.Option None)
  in
  let v3 = (* "def" *) token env v3 in
  let v4 = map_function_constructor env v4 in
  let v5 =
    (match v5 with
    | Some x -> R.Option (Some (
        map_self_type_ascription env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3; v4; v5]

and map_function_definition (env : env) ((v1, v2) : CST.function_definition) =
  let v1 = map_function_declaration env v1 in
  let v2 =
    (match v2 with
    | `EQ_inde_exp (v1, v2) -> R.Case ("EQ_inde_exp",
        let v1 = (* "=" *) token env v1 in
        let v2 = map_indentable_expression env v2 in
        R.Tuple [v1; v2]
      )
    | `Blk_ x -> R.Case ("Blk_",
        map_block_ env x
      )
    )
  in
  R.Tuple [v1; v2]

and map_function_type (env : env) (x : CST.function_type) =
  (match x with
  | `Type_params_arrow_then_type (v1, v2) -> R.Case ("Type_params_arrow_then_type",
      let v1 = map_type_parameters env v1 in
      let v2 = map_arrow_then_type env v2 in
      R.Tuple [v1; v2]
    )
  | `Param_types_arrow_then_type (v1, v2) -> R.Case ("Param_types_arrow_then_type",
      let v1 = map_parameter_types env v1 in
      let v2 = map_arrow_then_type env v2 in
      R.Tuple [v1; v2]
    )
  )

and map_generic_function (env : env) ((v1, v2) : CST.generic_function) =
  let v1 = map_expression env v1 in
  let v2 = map_type_arguments env v2 in
  R.Tuple [v1; v2]

and map_given_conditional (env : env) (x : CST.given_conditional) =
  map_parameters env x

and map_given_constructor (env : env) ((v1, v2, v3, v4, v5) : CST.given_constructor) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        map_type_identifier env x
      ))
    | None -> R.Option None)
  in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_type_parameters env x
      ))
    | None -> R.Option None)
  in
  let v3 =
    R.List (List.map (fun (v1, v2) ->
      let v1 =
        (match v1 with
        | Some tok -> R.Option (Some (
            (* automatic_semicolon *) token env tok
          ))
        | None -> R.Option None)
      in
      let v2 = map_given_conditional env v2 in
      R.Tuple [v1; v2]
    ) v3)
  in
  let v4 =
    (match v4 with
    | Some tok -> R.Option (Some (
        (* automatic_semicolon *) token env tok
      ))
    | None -> R.Option None)
  in
  let v5 = (* ":" *) token env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

and map_given_definition (env : env) ((v1, v2, v3, v4, v5, v6) : CST.given_definition) =
  let v1 = R.List (List.map (map_annotation env) v1) in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_modifiers env x
      ))
    | None -> R.Option None)
  in
  let v3 = (* "given" *) token env v3 in
  let v4 =
    (match v4 with
    | Some x -> R.Option (Some (
        map_given_constructor env x
      ))
    | None -> R.Option None)
  in
  let v5 = R.List (List.map (map_given_sig env) v5) in
  let v6 =
    (match v6 with
    | `Stru_inst x -> R.Case ("Stru_inst",
        map_structural_instance env x
      )
    | `Anno_type_opt_EQ_inde_exp (v1, v2) -> R.Case ("Anno_type_opt_EQ_inde_exp",
        let v1 = map_annotated_type env v1 in
        let v2 =
          (match v2 with
          | Some (v1, v2) -> R.Option (Some (
              let v1 = (* "=" *) token env v1 in
              let v2 = map_indentable_expression env v2 in
              R.Tuple [v1; v2]
            ))
          | None -> R.Option None)
        in
        R.Tuple [v1; v2]
      )
    )
  in
  R.Tuple [v1; v2; v3; v4; v5; v6]

and map_given_pattern (env : env) ((v1, v2) : CST.given_pattern) =
  let v1 = (* "given" *) token env v1 in
  let v2 = map_type_ env v2 in
  R.Tuple [v1; v2]

and map_given_sig (env : env) ((v1, v2) : CST.given_sig) =
  let v1 = map_given_conditional env v1 in
  let v2 = (* "=>" *) token env v2 in
  R.Tuple [v1; v2]

and map_guard (env : env) ((v1, v2) : CST.guard) =
  let v1 = (* "if" *) token env v1 in
  let v2 = map_postfix_expression_choice env v2 in
  R.Tuple [v1; v2]

and map_if_condition (env : env) (x : CST.if_condition) =
  (match x with
  | `Paren_exp x -> R.Case ("Paren_exp",
      map_parenthesized_expression env x
    )
  | `Inde_exp_then (v1, v2) -> R.Case ("Inde_exp_then",
      let v1 = map_indentable_expression env v1 in
      let v2 = (* "then" *) token env v2 in
      R.Tuple [v1; v2]
    )
  )

and map_import_declaration (env : env) ((v1, v2, v3) : CST.import_declaration) =
  let v1 = (* "import" *) token env v1 in
  let v2 = map_namespace_expression env v2 in
  let v3 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_namespace_expression env v2 in
      R.Tuple [v1; v2]
    ) v3)
  in
  R.Tuple [v1; v2; v3]

and map_import_selectors (env : env) (x : CST.import_selectors) =
  map_namespace_selectors env x

and map_indentable_expression (env : env) (x : CST.indentable_expression) =
  (match x with
  | `Inde_blk x -> R.Case ("Inde_blk",
      map_indented_block env x
    )
  | `Inde_cases x -> R.Case ("Inde_cases",
      map_indented_cases env x
    )
  | `Exp x -> R.Case ("Exp",
      map_expression env x
    )
  )

and map_indented_block (env : env) ((v1, v2, v3, v4) : CST.indented_block) =
  let v1 = (* indent *) token env v1 in
  let v2 = map_block env v2 in
  let v3 = (* outdent *) token env v3 in
  let v4 =
    (match v4 with
    | Some x -> R.Option (Some (
        map_end_marker env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3; v4]

and map_indented_cases (env : env) ((v1, v2, v3) : CST.indented_cases) =
  let v1 = (* indent *) token env v1 in
  let v2 = R.List (List.map (map_case_clause env) v2) in
  let v3 = (* outdent *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_indented_type_cases (env : env) ((v1, v2, v3) : CST.indented_type_cases) =
  let v1 = (* indent *) token env v1 in
  let v2 = R.List (List.map (map_type_case_clause env) v2) in
  let v3 = (* outdent *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_infix_expression (env : env) ((v1, v2, v3) : CST.infix_expression) =
  let v1 = map_anon_choice_infix_exp_dc476f6 env v1 in
  let v2 = map_type_identifier env v2 in
  let v3 =
    (match v3 with
    | `Prefix_exp x -> R.Case ("Prefix_exp",
        map_prefix_expression env x
      )
    | `Simple_exp x -> R.Case ("Simple_exp",
        map_simple_expression env x
      )
    | `COLON_colon_arg (v1, v2) -> R.Case ("COLON_colon_arg",
        let v1 = (* ":" *) token env v1 in
        let v2 = map_colon_argument env v2 in
        R.Tuple [v1; v2]
      )
    )
  in
  R.Tuple [v1; v2; v3]

and map_infix_pattern (env : env) ((v1, v2, v3) : CST.infix_pattern) =
  let v1 = map_pattern env v1 in
  let v2 = map_type_identifier env v2 in
  let v3 = map_pattern env v3 in
  R.Tuple [v1; v2; v3]

and map_infix_type (env : env) ((v1, v2, v3) : CST.infix_type) =
  let v1 = map_infix_type_choice env v1 in
  let v2 = map_type_identifier env v2 in
  let v3 = map_infix_type_choice env v3 in
  R.Tuple [v1; v2; v3]

and map_infix_type_choice (env : env) (x : CST.infix_type_choice) =
  (match x with
  | `Comp_type x -> R.Case ("Comp_type",
      map_compound_type env x
    )
  | `Infix_type x -> R.Case ("Infix_type",
      map_infix_type env x
    )
  | `Anno_type x -> R.Case ("Anno_type",
      map_annotated_type env x
    )
  | `Lit_type x -> R.Case ("Lit_type",
      map_literal_type env x
    )
  )

and map_instance_expression (env : env) (x : CST.instance_expression) =
  (match x with
  | `New_cons_app_temp_body (v1, v2, v3) -> R.Case ("New_cons_app_temp_body",
      let v1 = (* "new" *) token env v1 in
      let v2 = map_constructor_application env v2 in
      let v3 = map_structural_type env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `New_temp_body (v1, v2) -> R.Case ("New_temp_body",
      let v1 = (* "new" *) token env v1 in
      let v2 = map_structural_type env v2 in
      R.Tuple [v1; v2]
    )
  | `New_cons_app (v1, v2) -> R.Case ("New_cons_app",
      let v1 = (* "new" *) token env v1 in
      let v2 = map_constructor_application env v2 in
      R.Tuple [v1; v2]
    )
  )

and map_interpolated_string (env : env) (x : CST.interpolated_string) =
  (match x with
  | `Imm_tok_dquot_rep_inte_str_middle_choice_dollar_esc_single_line_str_end (v1, v2, v3) -> R.Case ("Imm_tok_dquot_rep_inte_str_middle_choice_dollar_esc_single_line_str_end",
      let v1 = map_imm_tok_dquot env v1 in
      let v2 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = (* interpolated_string_middle *) token env v1 in
          let v2 =
            (match v2 with
            | `Dollar_esc x -> R.Case ("Dollar_esc",
                map_dollar_escape env x
              )
            | `Interp x -> R.Case ("Interp",
                map_interpolation env x
              )
            | `Esc_seq tok -> R.Case ("Esc_seq",
                (* escape_sequence *) token env tok
              )
            )
          in
          R.Tuple [v1; v2]
        ) v2)
      in
      let v3 = (* single_line_string_end *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Imm_tok_dquo_rep_inte_mult_str_middle_choice_dollar_esc_mult_str_end (v1, v2, v3) -> R.Case ("Imm_tok_dquo_rep_inte_mult_str_middle_choice_dollar_esc_mult_str_end",
      let v1 = map_imm_tok_dquotdquotdquot env v1 in
      let v2 =
        R.List (List.map (fun (v1, v2) ->
          let v1 =
            (* interpolated_multiline_string_middle *) token env v1
          in
          let v2 = map_anon_choice_dollar_esc_fba2882 env v2 in
          R.Tuple [v1; v2]
        ) v2)
      in
      let v3 = (* multiline_string_end *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_interpolated_string_expression (env : env) (x : CST.interpolated_string_expression) =
  (match x with
  | `Raw_str_start_raw_str (v1, v2) -> R.Case ("Raw_str_start_raw_str",
      let v1 = (* raw_string_start *) token env v1 in
      let v2 = map_raw_string env v2 in
      R.Tuple [v1; v2]
    )
  | `Id_inte_str (v1, v2) -> R.Case ("Id_inte_str",
      let v1 = map_identifier env v1 in
      let v2 = map_interpolated_string env v2 in
      R.Tuple [v1; v2]
    )
  )

and map_interpolation (env : env) ((v1, v2) : CST.interpolation) =
  let v1 = (* "$" *) token env v1 in
  let v2 =
    (match v2 with
    | `Alia_interp_id tok -> R.Case ("Alia_interp_id",
        (* pattern [\p{Lu}\p{Lt}\p{Nl}\p{Lo}\p{Lm}\p{Ll}_\u00AA\u00BB\u02B0-\u02B8\u02C0-\u02C1\u02E0-\u02E4\u037A\u1D78\u1D9B-\u1DBF\u2071\u207F\u2090-\u209C\u2C7C-\u2C7D\uA69C-\uA69D\uA770\uA7F8-\uA7F9\uAB5C-\uAB5F][\p{Lu}\p{Lt}\p{Nl}\p{Lo}\p{Lm}\p{Ll}_\u00AA\u00BB\u02B0-\u02B8\u02C0-\u02C1\u02E0-\u02E4\u037A\u1D78\u1D9B-\u1DBF\u2071\u207F\u2090-\u209C\u2C7C-\u2C7D\uA69C-\uA69D\uA770\uA7F8-\uA7F9\uAB5C-\uAB5F0-9_\p{Ll}]* *) token env tok
      )
    | `Blk_ x -> R.Case ("Blk_",
        map_block_ env x
      )
    )
  in
  R.Tuple [v1; v2]

and map_lazy_parameter_type (env : env) ((v1, v2) : CST.lazy_parameter_type) =
  let v1 = (* "=>" *) token env v1 in
  let v2 = map_type_ env v2 in
  R.Tuple [v1; v2]

and map_lower_bound (env : env) ((v1, v2) : CST.lower_bound) =
  let v1 = (* ">:" *) token env v1 in
  let v2 = map_type_ env v2 in
  R.Tuple [v1; v2]

and map_name_and_type (env : env) ((v1, v2, v3) : CST.name_and_type) =
  let v1 = map_type_identifier env v1 in
  let v2 = (* ":" *) token env v2 in
  let v3 = map_param_type env v3 in
  R.Tuple [v1; v2; v3]

and map_named_pattern (env : env) ((v1, v2, v3) : CST.named_pattern) =
  let v1 = map_type_identifier env v1 in
  let v2 = (* "=" *) token env v2 in
  let v3 = map_pattern env v3 in
  R.Tuple [v1; v2; v3]

and map_named_tuple_pattern (env : env) ((v1, v2, v3, v4, v5) : CST.named_tuple_pattern) =
  let v1 = (* "(" *) token env v1 in
  let v2 = map_named_pattern env v2 in
  let v3 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_named_pattern env v2 in
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
  let v5 = (* ")" *) token env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

and map_namespace_expression (env : env) ((v1, v2, v3) : CST.namespace_expression) =
  let v1 = map_type_identifier env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "." *) token env v1 in
      let v2 = map_type_identifier env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  let v3 =
    (match v3 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = (* "." *) token env v1 in
        let v2 =
          (match v2 with
          | `Name_wild x -> R.Case ("Name_wild",
              map_namespace_wildcard env x
            )
          | `Name_selecs x -> R.Case ("Name_selecs",
              map_import_selectors env x
            )
          | `As_rena_id x -> R.Case ("As_rena_id",
              map_as_renamed_identifier env x
            )
          )
        in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3]

and map_namespace_selectors (env : env) ((v1, v2, v3, v4, v5) : CST.namespace_selectors) =
  let v1 = (* "{" *) token env v1 in
  let v2 =
    map_anon_choice_name_given_by_type_ca66fd5 env v2
  in
  let v3 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 =
        map_anon_choice_name_given_by_type_ca66fd5 env v2
      in
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
  let v5 = (* "}" *) token env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

and map_object_definition (env : env) ((v1, v2, v3, v4, v5) : CST.object_definition) =
  let v1 = R.List (List.map (map_annotation env) v1) in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_modifiers env x
      ))
    | None -> R.Option None)
  in
  let v3 =
    (match v3 with
    | Some tok -> R.Option (Some (
        (* "case" *) token env tok
      ))
    | None -> R.Option None)
  in
  let v4 = (* "object" *) token env v4 in
  let v5 = map_object_definition_ env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

and map_object_definition_ (env : env) ((v1, v2, v3, v4) : CST.object_definition_) =
  let v1 = map_type_identifier env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_extends_clause env x
      ))
    | None -> R.Option None)
  in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_derives_clause env x
      ))
    | None -> R.Option None)
  in
  let v4 =
    (match v4 with
    | Some x -> R.Option (Some (
        map_definition_body env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3; v4]

and map_package_clause (env : env) ((v1, v2, v3) : CST.package_clause) =
  let v1 = (* "package" *) token env v1 in
  let v2 = map_package_identifier env v2 in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_structural_type env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3]

and map_package_object (env : env) ((v1, v2, v3) : CST.package_object) =
  let v1 = (* "package" *) token env v1 in
  let v2 = (* "object" *) token env v2 in
  let v3 = map_object_definition_ env v3 in
  R.Tuple [v1; v2; v3]

and map_param_type (env : env) (x : CST.param_type) =
  (match x with
  | `Type x -> R.Case ("Type",
      map_type_ env x
    )
  | `Lazy_param_type x -> R.Case ("Lazy_param_type",
      map_lazy_parameter_type env x
    )
  | `Repe_param_type (v1, v2) -> R.Case ("Repe_param_type",
      let v1 =
        (match v1 with
        | `Type x -> R.Case ("Type",
            map_type_ env x
          )
        | `Lazy_param_type x -> R.Case ("Lazy_param_type",
            map_lazy_parameter_type env x
          )
        )
      in
      let v2 = (* "*" *) token env v2 in
      R.Tuple [v1; v2]
    )
  )

and map_parameter (env : env) (x : CST.parameter) =
  (match x with
  | `Rep_anno_opt_inline_modi_choice_id_COLON_choice_type_opt_EQ_exp (v1, v2, v3, v4, v5, v6) -> R.Case ("Rep_anno_opt_inline_modi_choice_id_COLON_choice_type_opt_EQ_exp",
      let v1 = R.List (List.map (map_annotation env) v1) in
      let v2 =
        (match v2 with
        | Some tok -> R.Option (Some (
            (* "inline" *) token env tok
          ))
        | None -> R.Option None)
      in
      let v3 = map_type_identifier env v3 in
      let v4 = (* ":" *) token env v4 in
      let v5 = map_param_type env v5 in
      let v6 =
        (match v6 with
        | Some (v1, v2) -> R.Option (Some (
            let v1 = (* "=" *) token env v1 in
            let v2 = map_expression env v2 in
            R.Tuple [v1; v2]
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3; v4; v5; v6]
    )
  | `Semg_ellips tok -> R.Case ("Semg_ellips",
      (* semgrep_ellipsis *) token env tok
    )
  )

and map_parameter_types (env : env) (x : CST.parameter_types) =
  (match x with
  | `Anno_type x -> R.Case ("Anno_type",
      map_annotated_type env x
    )
  | `LPAR_opt_choice_type_rep_COMMA_choice_type_opt_COMMA_RPAR (v1, v2, v3) -> R.Case ("LPAR_opt_choice_type_rep_COMMA_choice_type_opt_COMMA_RPAR",
      let v1 = (* "(" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_anon_param_type_rep_COMMA_param_type_opt_COMMA_dbcd678 env x
          ))
        | None -> R.Option None)
      in
      let v3 = (* ")" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Comp_type x -> R.Case ("Comp_type",
      map_compound_type env x
    )
  | `Infix_type x -> R.Case ("Infix_type",
      map_infix_type env x
    )
  )

and map_parameters (env : env) (x : CST.parameters) =
  (match x with
  | `LPAR_opt_impl_opt_param_rep_COMMA_param_opt_COMMA_RPAR (v1, v2, v3, v4) -> R.Case ("LPAR_opt_impl_opt_param_rep_COMMA_param_opt_COMMA_RPAR",
      let v1 = (* "(" *) token env v1 in
      let v2 =
        (match v2 with
        | Some tok -> R.Option (Some (
            (* "implicit" *) token env tok
          ))
        | None -> R.Option None)
      in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_anon_param_rep_COMMA_param_opt_COMMA_bde8b1d env x
          ))
        | None -> R.Option None)
      in
      let v4 = (* ")" *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Using_params_clause (v1, v2, v3, v4) -> R.Case ("Using_params_clause",
      let v1 = (* "(" *) token env v1 in
      let v2 = (* "using" *) token env v2 in
      let v3 =
        (match v3 with
        | `Param_rep_COMMA_param_opt_COMMA x -> R.Case ("Param_rep_COMMA_param_opt_COMMA",
            map_anon_param_rep_COMMA_param_opt_COMMA_bde8b1d env x
          )
        | `Choice_type_rep_COMMA_choice_type_opt_COMMA x -> R.Case ("Choice_type_rep_COMMA_choice_type_opt_COMMA",
            map_anon_param_type_rep_COMMA_param_type_opt_COMMA_dbcd678 env x
          )
        )
      in
      let v4 = (* ")" *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  )

and map_parenthesized_expression (env : env) ((v1, v2, v3) : CST.parenthesized_expression) =
  let v1 = (* "(" *) token env v1 in
  let v2 = map_expression env v2 in
  let v3 = (* ")" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_pattern (env : env) (x : CST.pattern) =
  (match x with
  | `Choice_choice_id x -> R.Case ("Choice_choice_id",
      (match x with
      | `Choice_id x -> R.Case ("Choice_id",
          map_type_identifier env x
        )
      | `Stable_id x -> R.Case ("Stable_id",
          map_stable_identifier env x
        )
      | `Inte_str_exp x -> R.Case ("Inte_str_exp",
          map_interpolated_string_expression env x
        )
      | `Capt_pat x -> R.Case ("Capt_pat",
          map_capture_pattern env x
        )
      | `Tuple_pat x -> R.Case ("Tuple_pat",
          map_tuple_pattern env x
        )
      | `Named_tuple_pat x -> R.Case ("Named_tuple_pat",
          map_named_tuple_pattern env x
        )
      | `Case_class_pat x -> R.Case ("Case_class_pat",
          map_case_class_pattern env x
        )
      | `Infix_pat x -> R.Case ("Infix_pat",
          map_infix_pattern env x
        )
      | `Alt_pat x -> R.Case ("Alt_pat",
          map_alternative_pattern env x
        )
      | `Typed_pat x -> R.Case ("Typed_pat",
          map_typed_pattern env x
        )
      | `Given_pat x -> R.Case ("Given_pat",
          map_given_pattern env x
        )
      | `Quote_exp x -> R.Case ("Quote_exp",
          map_quote_expression env x
        )
      | `Choice_non_null_lit x -> R.Case ("Choice_non_null_lit",
          map_literal env x
        )
      | `Wild tok -> R.Case ("Wild",
          (* "_" *) token env tok
        )
      | `Repeat_pat x -> R.Case ("Repeat_pat",
          map_repeat_pattern env x
        )
      )
    )
  | `Semg_ellips tok -> R.Case ("Semg_ellips",
      (* semgrep_ellipsis *) token env tok
    )
  )

and map_postfix_expression (env : env) ((v1, v2) : CST.postfix_expression) =
  let v1 = map_anon_choice_infix_exp_dc476f6 env v1 in
  let v2 = map_type_identifier env v2 in
  R.Tuple [v1; v2]

and map_postfix_expression_choice (env : env) (x : CST.postfix_expression_choice) =
  (match x with
  | `Post_exp x -> R.Case ("Post_exp",
      map_postfix_expression env x
    )
  | `Infix_exp x -> R.Case ("Infix_exp",
      map_infix_expression env x
    )
  | `Prefix_exp x -> R.Case ("Prefix_exp",
      map_prefix_expression env x
    )
  | `Simple_exp x -> R.Case ("Simple_exp",
      map_simple_expression env x
    )
  )

and map_prefix_expression (env : env) ((v1, v2) : CST.prefix_expression) =
  let v1 =
    (match v1 with
    | `PLUS tok -> R.Case ("PLUS",
        (* "+" *) token env tok
      )
    | `DASH tok -> R.Case ("DASH",
        (* "-" *) token env tok
      )
    | `BANG tok -> R.Case ("BANG",
        (* "!" *) token env tok
      )
    | `TILDE tok -> R.Case ("TILDE",
        (* "~" *) token env tok
      )
    )
  in
  let v2 = map_simple_expression env v2 in
  R.Tuple [v1; v2]

and map_quote_expression (env : env) ((v1, v2) : CST.quote_expression) =
  let v1 = (* "'" *) token env v1 in
  let v2 =
    (match v2 with
    | `LCURL_opt_blk_RCURL x -> R.Case ("LCURL_opt_blk_RCURL",
        map_block_ env x
      )
    | `LBRACK_type_RBRACK (v1, v2, v3) -> R.Case ("LBRACK_type_RBRACK",
        let v1 = (* "[" *) token env v1 in
        let v2 = map_type_ env v2 in
        let v3 = (* "]" *) token env v3 in
        R.Tuple [v1; v2; v3]
      )
    | `Id x -> R.Case ("Id",
        map_identifier env x
      )
    )
  in
  R.Tuple [v1; v2]

and map_raw_string (env : env) (x : CST.raw_string) =
  (match x with
  | `Simple_str_start_rep_raw_str_middle_choice_dollar_esc_single_line_str_end (v1, v2, v3) -> R.Case ("Simple_str_start_rep_raw_str_middle_choice_dollar_esc_single_line_str_end",
      let v1 = (* simple_string_start *) token env v1 in
      let v2 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = (* raw_string_middle *) token env v1 in
          let v2 = map_anon_choice_dollar_esc_fba2882 env v2 in
          R.Tuple [v1; v2]
        ) v2)
      in
      let v3 = (* single_line_string_end *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Simple_mult_str_start_rep_raw_str_mult_middle_choice_dollar_esc_mult_str_end (v1, v2, v3) -> R.Case ("Simple_mult_str_start_rep_raw_str_mult_middle_choice_dollar_esc_mult_str_end",
      let v1 = (* simple_multiline_string_start *) token env v1 in
      let v2 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = (* raw_string_multiline_middle *) token env v1 in
          let v2 = map_anon_choice_dollar_esc_fba2882 env v2 in
          R.Tuple [v1; v2]
        ) v2)
      in
      let v3 = (* multiline_string_end *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_refinement (env : env) (x : CST.refinement) =
  map_template_body env x

and map_repeat_pattern (env : env) ((v1, v2) : CST.repeat_pattern) =
  let v1 = map_pattern env v1 in
  let v2 = (* "*" *) token env v2 in
  R.Tuple [v1; v2]

and map_self_type (env : env) ((v1, v2, v3) : CST.self_type) =
  let v1 = map_type_identifier env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_self_type_ascription env x
      ))
    | None -> R.Option None)
  in
  let v3 = (* "=>" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_self_type_ascription (env : env) ((v1, v2) : CST.self_type_ascription) =
  let v1 = (* ":" *) token env v1 in
  let v2 = map_type_ env v2 in
  R.Tuple [v1; v2]

and map_simple_enum_case (env : env) ((v1, v2) : CST.simple_enum_case) =
  let v1 = map_type_identifier env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_extends_clause env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2]

and map_simple_expression (env : env) (x : CST.simple_expression) =
  (match x with
  | `Choice_id x -> R.Case ("Choice_id",
      (match x with
      | `Id x -> R.Case ("Id",
          map_identifier env x
        )
      | `Op_id tok -> R.Case ("Op_id",
          (* operator_identifier *) token env tok
        )
      | `Choice_non_null_lit x -> R.Case ("Choice_non_null_lit",
          map_literal env x
        )
      | `Inte_str_exp x -> R.Case ("Inte_str_exp",
          map_interpolated_string_expression env x
        )
      | `Unit x -> R.Case ("Unit",
          map_unit_ env x
        )
      | `Tuple_exp x -> R.Case ("Tuple_exp",
          map_tuple_expression env x
        )
      | `Wild tok -> R.Case ("Wild",
          (* "_" *) token env tok
        )
      | `Blk_ x -> R.Case ("Blk_",
          map_block_ env x
        )
      | `Splice_exp x -> R.Case ("Splice_exp",
          map_splice_expression env x
        )
      | `Case_blk x -> R.Case ("Case_blk",
          map_case_block env x
        )
      | `Quote_exp x -> R.Case ("Quote_exp",
          map_quote_expression env x
        )
      | `Inst_exp x -> R.Case ("Inst_exp",
          map_instance_expression env x
        )
      | `Paren_exp x -> R.Case ("Paren_exp",
          map_parenthesized_expression env x
        )
      | `Field_exp x -> R.Case ("Field_exp",
          map_field_expression env x
        )
      | `Gene_func x -> R.Case ("Gene_func",
          map_generic_function env x
        )
      | `Call_exp x -> R.Case ("Call_exp",
          map_call_expression env x
        )
      )
    )
  | `Symb_lit (v1, v2) -> R.Case ("Symb_lit",
      let v1 = (* "'" *) token env v1 in
      let v2 = map_identifier env v2 in
      R.Tuple [v1; v2]
    )
  | `Semg_meta tok -> R.Case ("Semg_meta",
      (* semgrep_metavariable *) token env tok
    )
  | `Deep_exp (v1, v2, v3) -> R.Case ("Deep_exp",
      let v1 = (* "<..." *) token env v1 in
      let v2 = map_expression env v2 in
      let v3 = (* "...>" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Semg_ellips_meta tok -> R.Case ("Semg_ellips_meta",
      (* semgrep_ellipsis_metavariable *) token env tok
    )
  | `Semg_ellips tok -> R.Case ("Semg_ellips",
      (* semgrep_ellipsis *) token env tok
    )
  )

and map_simple_type (env : env) (x : CST.simple_type) =
  (match x with
  | `Gene_type (v1, v2) -> R.Case ("Gene_type",
      let v1 = map_simple_type env v1 in
      let v2 = map_type_arguments env v2 in
      R.Tuple [v1; v2]
    )
  | `Proj_type (v1, v2, v3) -> R.Case ("Proj_type",
      let v1 = map_simple_type env v1 in
      let v2 = (* "#" *) token env v2 in
      let v3 = map_type_identifier env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Tuple_type (v1, v2, v3, v4, v5) -> R.Case ("Tuple_type",
      let v1 = (* "(" *) token env v1 in
      let v2 = map_type_ env v2 in
      let v3 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = (* "," *) token env v1 in
          let v2 = map_type_ env v2 in
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
      let v5 = (* ")" *) token env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `Named_tuple_type (v1, v2, v3, v4, v5) -> R.Case ("Named_tuple_type",
      let v1 = (* "(" *) token env v1 in
      let v2 = map_name_and_type env v2 in
      let v3 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = (* "," *) token env v1 in
          let v2 = map_name_and_type env v2 in
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
      let v5 = (* ")" *) token env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `Sing_type (v1, v2, v3) -> R.Case ("Sing_type",
      let v1 = map_anon_choice_type_id_4bf0d65 env v1 in
      let v2 = (* "." *) token env v2 in
      let v3 = (* "type" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Stable_type_id x -> R.Case ("Stable_type_id",
      map_stable_type_identifier env x
    )
  | `Type_id x -> R.Case ("Type_id",
      map_type_identifier env x
    )
  | `Appl_cons_type (v1, v2) -> R.Case ("Appl_cons_type",
      let v1 = map_type_identifier env v1 in
      let v2 = map_arguments env v2 in
      R.Tuple [v1; v2]
    )
  | `Wild tok -> R.Case ("Wild",
      (* "_" *) token env tok
    )
  )

and map_splice_expression (env : env) ((v1, v2) : CST.splice_expression) =
  let v1 = (* "$" *) token env v1 in
  let v2 =
    (match v2 with
    | `LCURL_blk_RCURL (v1, v2, v3) -> R.Case ("LCURL_blk_RCURL",
        let v1 = (* "{" *) token env v1 in
        let v2 = map_block env v2 in
        let v3 = (* "}" *) token env v3 in
        R.Tuple [v1; v2; v3]
      )
    | `LBRACK_type_RBRACK (v1, v2, v3) -> R.Case ("LBRACK_type_RBRACK",
        let v1 = (* "[" *) token env v1 in
        let v2 = map_type_ env v2 in
        let v3 = (* "]" *) token env v3 in
        R.Tuple [v1; v2; v3]
      )
    | `Id x -> R.Case ("Id",
        map_identifier env x
      )
    )
  in
  R.Tuple [v1; v2]

and map_start_val (env : env) ((v1, v2, v3) : CST.start_val) =
  let v1 = R.List (List.map (map_annotation env) v1) in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_modifiers env x
      ))
    | None -> R.Option None)
  in
  let v3 = (* "val" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_start_var (env : env) ((v1, v2, v3) : CST.start_var) =
  let v1 = R.List (List.map (map_annotation env) v1) in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_modifiers env x
      ))
    | None -> R.Option None)
  in
  let v3 = (* "var" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_structural_instance (env : env) ((v1, v2, v3) : CST.structural_instance) =
  let v1 = map_constructor_application env v1 in
  let v2 =
    (match v2 with
    | `COLON tok -> R.Case ("COLON",
        (* ":" *) token env tok
      )
    | `With tok -> R.Case ("With",
        (* "with" *) token env tok
      )
    )
  in
  let v3 = map_with_template_body env v3 in
  R.Tuple [v1; v2; v3]

and map_structural_type (env : env) (x : CST.structural_type) =
  map_template_body env x

and map_template_body (env : env) (x : CST.template_body) =
  (match x with
  | `Inde_temp_body (v1, v2, v3, v4, v5) -> R.Case ("Inde_temp_body",
      let v1 = (* ":" *) token env v1 in
      let v2 = (* indent *) token env v2 in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_self_type env x
          ))
        | None -> R.Option None)
      in
      let v4 = map_block env v4 in
      let v5 = (* outdent *) token env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `Braced_temp_body (v1, v2, v3) -> R.Case ("Braced_temp_body",
      let v1 = (* "{" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            (match x with
            | `Braced_temp_body1 x -> R.Case ("Braced_temp_body1",
                map_braced_template_body1 env x
              )
            | `Braced_temp_body2 x -> R.Case ("Braced_temp_body2",
                map_braced_template_body2 env x
              )
            )
          ))
        | None -> R.Option None)
      in
      let v3 = (* "}" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_trait_definition (env : env) ((v1, v2, v3, v4) : CST.trait_definition) =
  let v1 = R.List (List.map (map_annotation env) v1) in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_modifiers env x
      ))
    | None -> R.Option None)
  in
  let v3 = (* "trait" *) token env v3 in
  let v4 = map_class_definition_ env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_tuple_expression (env : env) ((v1, v2, v3, v4, v5) : CST.tuple_expression) =
  let v1 = (* "(" *) token env v1 in
  let v2 = map_expression env v2 in
  let v3 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_expression env v2 in
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
  let v5 = (* ")" *) token env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

and map_tuple_pattern (env : env) ((v1, v2, v3, v4, v5) : CST.tuple_pattern) =
  let v1 = (* "(" *) token env v1 in
  let v2 = map_pattern env v2 in
  let v3 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_pattern env v2 in
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
  let v5 = (* ")" *) token env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

and map_type_ (env : env) (x : CST.type_) =
  (match x with
  | `Func_type x -> R.Case ("Func_type",
      map_function_type env x
    )
  | `Comp_type x -> R.Case ("Comp_type",
      map_compound_type env x
    )
  | `Infix_type x -> R.Case ("Infix_type",
      map_infix_type env x
    )
  | `Match_type (v1, v2, v3) -> R.Case ("Match_type",
      let v1 = map_infix_type_choice env v1 in
      let v2 = (* "match" *) token env v2 in
      let v3 = map_indented_type_cases env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Anno_type x -> R.Case ("Anno_type",
      map_annotated_type env x
    )
  | `Lit_type x -> R.Case ("Lit_type",
      map_literal_type env x
    )
  | `Stru_type x -> R.Case ("Stru_type",
      map_structural_type env x
    )
  | `Type_lambda x -> R.Case ("Type_lambda",
      map_type_lambda env x
    )
  )

and map_type_arguments (env : env) ((v1, v2, v3, v4, v5) : CST.type_arguments) =
  let v1 = (* "[" *) token env v1 in
  let v2 = map_type_ env v2 in
  let v3 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_type_ env v2 in
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
  let v5 = (* "]" *) token env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

and map_type_case_clause (env : env) ((v1, v2, v3) : CST.type_case_clause) =
  let v1 = (* "case" *) token env v1 in
  let v2 = map_infix_type_choice env v2 in
  let v3 = map_arrow_then_type env v3 in
  R.Tuple [v1; v2; v3]

and map_type_constructor (env : env) ((v1, v2, v3, v4, v5) : CST.type_constructor) =
  let v1 = map_type_identifier env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_type_parameters env x
      ))
    | None -> R.Option None)
  in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_lower_bound env x
      ))
    | None -> R.Option None)
  in
  let v4 =
    (match v4 with
    | Some x -> R.Option (Some (
        map_upper_bound env x
      ))
    | None -> R.Option None)
  in
  let v5 =
    (match v5 with
    | Some x -> R.Option (Some (
        map_context_bounds env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3; v4; v5]

and map_type_definition (env : env) ((v1, v2, v3, v4, v5, v6) : CST.type_definition) =
  let v1 = R.List (List.map (map_annotation env) v1) in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_modifiers env x
      ))
    | None -> R.Option None)
  in
  let v3 =
    (match v3 with
    | Some tok -> R.Option (Some (
        (* "opaque" *) token env tok
      ))
    | None -> R.Option None)
  in
  let v4 = (* "type" *) token env v4 in
  let v5 = map_type_constructor env v5 in
  let v6 =
    (match v6 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = (* "=" *) token env v1 in
        let v2 = map_type_ env v2 in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3; v4; v5; v6]

and map_type_lambda (env : env) ((v1, v2, v3, v4, v5, v6, v7) : CST.type_lambda) =
  let v1 = (* "[" *) token env v1 in
  let v2 = map_type_parameter env v2 in
  let v3 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_type_parameter env v2 in
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
  let v5 = (* "]" *) token env v5 in
  let v6 = (* "=>>" *) token env v6 in
  let v7 = map_type_ env v7 in
  R.Tuple [v1; v2; v3; v4; v5; v6; v7]

and map_type_parameter (env : env) ((v1, v2, v3, v4, v5, v6) : CST.type_parameter) =
  let v1 =
    (match v1 with
    | `Wild tok -> R.Case ("Wild",
        (* "_" *) token env tok
      )
    | `Choice_id x -> R.Case ("Choice_id",
        map_type_identifier env x
      )
    )
  in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_type_parameters env x
      ))
    | None -> R.Option None)
  in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_lower_bound env x
      ))
    | None -> R.Option None)
  in
  let v4 =
    (match v4 with
    | Some x -> R.Option (Some (
        map_upper_bound env x
      ))
    | None -> R.Option None)
  in
  let v5 =
    (match v5 with
    | Some xs -> R.Option (Some (
        R.List (List.map (map_view_bound env) xs)
      ))
    | None -> R.Option None)
  in
  let v6 =
    (match v6 with
    | Some x -> R.Option (Some (
        map_context_bounds env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3; v4; v5; v6]

and map_type_parameters (env : env) ((v1, v2, v3, v4, v5) : CST.type_parameters) =
  let v1 = (* "[" *) token env v1 in
  let v2 = map_variant_type_parameter env v2 in
  let v3 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_variant_type_parameter env v2 in
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
  let v5 = (* "]" *) token env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

and map_typed_pattern (env : env) ((v1, v2, v3) : CST.typed_pattern) =
  let v1 = map_pattern env v1 in
  let v2 = (* ":" *) token env v2 in
  let v3 = map_type_ env v3 in
  R.Tuple [v1; v2; v3]

and map_upper_bound (env : env) ((v1, v2) : CST.upper_bound) =
  let v1 = (* "<:" *) token env v1 in
  let v2 = map_type_ env v2 in
  R.Tuple [v1; v2]

and map_val_declaration (env : env) ((v1, v2, v3, v4, v5) : CST.val_declaration) =
  let v1 = map_start_val env v1 in
  let v2 = map_type_identifier env v2 in
  let v3 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_type_identifier env v2 in
      R.Tuple [v1; v2]
    ) v3)
  in
  let v4 = (* ":" *) token env v4 in
  let v5 = map_type_ env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

and map_val_definition (env : env) ((v1, v2, v3, v4, v5) : CST.val_definition) =
  let v1 = map_start_val env v1 in
  let v2 = map_anon_choice_pat_a6d147b env v2 in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_self_type_ascription env x
      ))
    | None -> R.Option None)
  in
  let v4 = (* "=" *) token env v4 in
  let v5 = map_indentable_expression env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

and map_var_declaration (env : env) ((v1, v2, v3, v4, v5) : CST.var_declaration) =
  let v1 = map_start_var env v1 in
  let v2 = map_type_identifier env v2 in
  let v3 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_type_identifier env v2 in
      R.Tuple [v1; v2]
    ) v3)
  in
  let v4 = (* ":" *) token env v4 in
  let v5 = map_type_ env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

and map_var_definition (env : env) ((v1, v2, v3, v4, v5) : CST.var_definition) =
  let v1 = map_start_var env v1 in
  let v2 = map_anon_choice_pat_a6d147b env v2 in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_self_type_ascription env x
      ))
    | None -> R.Option None)
  in
  let v4 = (* "=" *) token env v4 in
  let v5 = map_indentable_expression env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

and map_variant_type_parameter (env : env) ((v1, v2) : CST.variant_type_parameter) =
  let v1 = R.List (List.map (map_annotation env) v1) in
  let v2 =
    (match v2 with
    | `Cova_type_param x -> R.Case ("Cova_type_param",
        map_covariant_type_parameter env x
      )
    | `Cont_type_param x -> R.Case ("Cont_type_param",
        map_contravariant_type_parameter env x
      )
    | `Type_param x -> R.Case ("Type_param",
        map_type_parameter env x
      )
    | `Type_lambda x -> R.Case ("Type_lambda",
        map_type_lambda env x
      )
    )
  in
  R.Tuple [v1; v2]

and map_view_bound (env : env) ((v1, v2) : CST.view_bound) =
  let v1 = (* "<%" *) token env v1 in
  let v2 = map_type_ env v2 in
  R.Tuple [v1; v2]

and map_while_expression (env : env) (x : CST.while_expression) =
  (match x with
  | `While_paren_exp_exp (v1, v2, v3) -> R.Case ("While_paren_exp_exp",
      let v1 = (* "while" *) token env v1 in
      let v2 = map_parenthesized_expression env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `While_inde_exp_do_inde_exp (v1, v2, v3, v4) -> R.Case ("While_inde_exp_do_inde_exp",
      let v1 = (* "while" *) token env v1 in
      let v2 = map_indentable_expression env v2 in
      let v3 = (* "do" *) token env v3 in
      let v4 = map_indentable_expression env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  )

and map_with_template_body (env : env) ((v1, v2, v3, v4) : CST.with_template_body) =
  let v1 = (* indent *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_self_type env x
      ))
    | None -> R.Option None)
  in
  let v3 = map_block env v3 in
  let v4 = (* outdent *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

let map_top_level_definition (env : env) (x : CST.top_level_definition) =
  (match x with
  | `Semg_exp (v1, v2) -> R.Case ("Semg_exp",
      let v1 = map_tok_prec_p100___semgrep_expression env v1 in
      let v2 = map_expression env v2 in
      R.Tuple [v1; v2]
    )
  | `Semg_stmt (v1, v2) -> R.Case ("Semg_stmt",
      let v1 = map_tok_prec_p100___semgrep_statement env v1 in
      let v2 =
        (match v2 with
        | `Exp x -> R.Case ("Exp",
            map_expression env x
          )
        | `Choice_choice_given_defi x -> R.Case ("Choice_choice_given_defi",
            map_definition env x
          )
        )
      in
      R.Tuple [v1; v2]
    )
  | `Semg_member_decl (v1, v2) -> R.Case ("Semg_member_decl",
      let v1 = map_tok_prec_p100___semgrep_member_decl env v1 in
      let v2 =
        (match v2 with
        | `Func_defi x -> R.Case ("Func_defi",
            map_function_definition env x
          )
        | `Func_decl x -> R.Case ("Func_decl",
            map_function_declaration env x
          )
        | `Val_defi x -> R.Case ("Val_defi",
            map_val_definition env x
          )
        | `Val_decl x -> R.Case ("Val_decl",
            map_val_declaration env x
          )
        | `Var_defi x -> R.Case ("Var_defi",
            map_var_definition env x
          )
        | `Var_decl x -> R.Case ("Var_decl",
            map_var_declaration env x
          )
        )
      in
      R.Tuple [v1; v2]
    )
  | `Choice_choice_choice_given_defi x -> R.Case ("Choice_choice_choice_given_defi",
      (match x with
      | `Choice_choice_given_defi x -> R.Case ("Choice_choice_given_defi",
          map_definition env x
        )
      | `End_marker x -> R.Case ("End_marker",
          map_end_marker env x
        )
      | `Exp x -> R.Case ("Exp",
          map_expression env x
        )
      )
    )
  )

let map_compilation_unit (env : env) ((v1, v2) : CST.compilation_unit) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        map_shebang env x
      ))
    | None -> R.Option None)
  in
  let v2 =
    (match v2 with
    | Some (v1, v2, v3) -> R.Option (Some (
        let v1 = map_top_level_definition env v1 in
        let v2 =
          R.List (List.map (fun (v1, v2) ->
            let v1 = map_semicolon env v1 in
            let v2 = map_top_level_definition env v2 in
            R.Tuple [v1; v2]
          ) v2)
        in
        let v3 =
          (match v3 with
          | Some x -> R.Option (Some (
              map_semicolon env x
            ))
          | None -> R.Option None)
        in
        R.Tuple [v1; v2; v3]
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2]

let map_block_comment (env : env) ((v1, v2, v3) : CST.block_comment) =
  let v1 = map_tok_slashstar env v1 in
  let v2 =
    R.List (List.map (fun x ->
      (match x with
      | `Tok_pat_5058f1a x -> R.Case ("Tok_pat_5058f1a",
          map_tok_pat_5058f1a env x
        )
      | `Tok_slas x -> R.Case ("Tok_slas",
          map_tok_slashslash env x
        )
      )
    ) v2)
  in
  let v3 = map_tok_starslash env v3 in
  R.Tuple [v1; v2; v3]

let map_comment (env : env) ((v1, v2) : CST.comment) =
  let v1 = map_tok_slashslash env v1 in
  let v2 =
    (match v2 with
    | `Using_dire x -> R.Case ("Using_dire",
        map_using_directive env x
      )
    | `Comm_text tok -> R.Case ("Comm_text",
        (* comment_text *) token env tok
      )
    )
  in
  R.Tuple [v1; v2]

let dump_tree root =
  map_compilation_unit () root
  |> Tree_sitter_run.Raw_tree.to_channel stdout

let map_extra (env : env) (x : CST.extra) =
  match x with
  | `Comment (_loc, x) -> ("comment", "comment", map_comment env x)
  | `Block_comment (_loc, x) -> ("block_comment", "block_comment", map_block_comment env x)

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
