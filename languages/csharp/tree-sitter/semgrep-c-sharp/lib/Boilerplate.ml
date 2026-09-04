(**
   Boilerplate to be used as a template when mapping the c_sharp CST
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

let map_anon_choice_async_25087f5 (env : env) (x : CST.anon_choice_async_25087f5) =
  (match x with
  | `Async tok -> R.Case ("Async",
      (* "async" *) token env tok
    )
  | `Static tok -> R.Case ("Static",
      (* "static" *) token env tok
    )
  | `Async_static (v1, v2) -> R.Case ("Async_static",
      let v1 = (* "async" *) token env v1 in
      let v2 = (* "static" *) token env v2 in
      R.Tuple [v1; v2]
    )
  | `Static_async (v1, v2) -> R.Case ("Static_async",
      let v1 = (* "static" *) token env v1 in
      let v2 = (* "async" *) token env v2 in
      R.Tuple [v1; v2]
    )
  )

let map_integer_literal (env : env) (tok : CST.integer_literal) =
  (* integer_literal *) token env tok

let map_overloadable_operator (env : env) (x : CST.overloadable_operator) =
  (match x with
  | `BANG tok -> R.Case ("BANG",
      (* "!" *) token env tok
    )
  | `TILDE tok -> R.Case ("TILDE",
      (* "~" *) token env tok
    )
  | `PLUSPLUS tok -> R.Case ("PLUSPLUS",
      (* "++" *) token env tok
    )
  | `DASHDASH tok -> R.Case ("DASHDASH",
      (* "--" *) token env tok
    )
  | `True tok -> R.Case ("True",
      (* "true" *) token env tok
    )
  | `False tok -> R.Case ("False",
      (* "false" *) token env tok
    )
  | `PLUS tok -> R.Case ("PLUS",
      (* "+" *) token env tok
    )
  | `DASH tok -> R.Case ("DASH",
      (* "-" *) token env tok
    )
  | `STAR tok -> R.Case ("STAR",
      (* "*" *) token env tok
    )
  | `SLASH tok -> R.Case ("SLASH",
      (* "/" *) token env tok
    )
  | `PERC tok -> R.Case ("PERC",
      (* "%" *) token env tok
    )
  | `HAT tok -> R.Case ("HAT",
      (* "^" *) token env tok
    )
  | `BAR tok -> R.Case ("BAR",
      (* "|" *) token env tok
    )
  | `AMP tok -> R.Case ("AMP",
      (* "&" *) token env tok
    )
  | `LTLT tok -> R.Case ("LTLT",
      (* "<<" *) token env tok
    )
  | `GTGT tok -> R.Case ("GTGT",
      (* ">>" *) token env tok
    )
  | `GTGTGT tok -> R.Case ("GTGTGT",
      (* ">>>" *) token env tok
    )
  | `EQEQ tok -> R.Case ("EQEQ",
      (* "==" *) token env tok
    )
  | `BANGEQ tok -> R.Case ("BANGEQ",
      (* "!=" *) token env tok
    )
  | `GT tok -> R.Case ("GT",
      (* ">" *) token env tok
    )
  | `LT tok -> R.Case ("LT",
      (* "<" *) token env tok
    )
  | `GTEQ tok -> R.Case ("GTEQ",
      (* ">=" *) token env tok
    )
  | `LTEQ tok -> R.Case ("LTEQ",
      (* "<=" *) token env tok
    )
  )

let map_identifier_token (env : env) (tok : CST.identifier_token) =
  (* identifier_token *) token env tok

let map_nullable_directive (env : env) ((v1, v2, v3) : CST.nullable_directive) =
  let v1 = (* "nullable" *) token env v1 in
  let v2 =
    (match v2 with
    | `Disa tok -> R.Case ("Disa",
        (* "disable" *) token env tok
      )
    | `Enable tok -> R.Case ("Enable",
        (* "enable" *) token env tok
      )
    | `Rest tok -> R.Case ("Rest",
        (* "restore" *) token env tok
      )
    )
  in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        (match x with
        | `Annots tok -> R.Case ("Annots",
            (* "annotations" *) token env tok
          )
        | `Warnis tok -> R.Case ("Warnis",
            (* "warnings" *) token env tok
          )
        )
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3]

let map_anon_choice_ref_eec35e8 (env : env) (x : CST.anon_choice_ref_eec35e8) =
  (match x with
  | `Ref tok -> R.Case ("Ref",
      (* "ref" *) token env tok
    )
  | `Out tok -> R.Case ("Out",
      (* "out" *) token env tok
    )
  | `In tok -> R.Case ("In",
      (* "in" *) token env tok
    )
  )

let map_opt_semi (env : env) (tok : CST.opt_semi) =
  (* opt_semi *) token env tok

let map_anon_choice_DOT_2ad1dab (env : env) (x : CST.anon_choice_DOT_2ad1dab) =
  (match x with
  | `DOT tok -> R.Case ("DOT",
      (* "." *) token env tok
    )
  | `DASHGT tok -> R.Case ("DASHGT",
      (* "->" *) token env tok
    )
  )

let map_contextual_keywords (env : env) (x : CST.contextual_keywords) =
  (match x with
  | `Alias tok -> R.Case ("Alias",
      (* "alias" *) token env tok
    )
  | `Asce tok -> R.Case ("Asce",
      (* "ascending" *) token env tok
    )
  | `By tok -> R.Case ("By",
      (* "by" *) token env tok
    )
  | `Desc tok -> R.Case ("Desc",
      (* "descending" *) token env tok
    )
  | `Equals tok -> R.Case ("Equals",
      (* "equals" *) token env tok
    )
  | `File tok -> R.Case ("File",
      (* "file" *) token env tok
    )
  | `From tok -> R.Case ("From",
      (* "from" *) token env tok
    )
  | `Global tok -> R.Case ("Global",
      (* "global" *) token env tok
    )
  | `Group tok -> R.Case ("Group",
      (* "group" *) token env tok
    )
  | `Into tok -> R.Case ("Into",
      (* "into" *) token env tok
    )
  | `Join tok -> R.Case ("Join",
      (* "join" *) token env tok
    )
  | `Let tok -> R.Case ("Let",
      (* "let" *) token env tok
    )
  | `Notn tok -> R.Case ("Notn",
      (* "notnull" *) token env tok
    )
  | `On tok -> R.Case ("On",
      (* "on" *) token env tok
    )
  | `Orde tok -> R.Case ("Orde",
      (* "orderby" *) token env tok
    )
  | `Scoped tok -> R.Case ("Scoped",
      (* "scoped" *) token env tok
    )
  | `Select tok -> R.Case ("Select",
      (* "select" *) token env tok
    )
  | `Unma tok -> R.Case ("Unma",
      (* "unmanaged" *) token env tok
    )
  | `Var tok -> R.Case ("Var",
      (* "var" *) token env tok
    )
  | `When tok -> R.Case ("When",
      (* "when" *) token env tok
    )
  | `Where tok -> R.Case ("Where",
      (* "where" *) token env tok
    )
  | `Yield tok -> R.Case ("Yield",
      (* "yield" *) token env tok
    )
  )

let map_preproc_string_literal (env : env) (tok : CST.preproc_string_literal) =
  (* pattern "\"[^\"]*\"" *) token env tok

let map_string_literal_encoding (env : env) (tok : CST.string_literal_encoding) =
  (* pattern (u|U)8 *) token env tok

let map_pat_00238b3 (env : env) (tok : CST.pat_00238b3) =
  (* pattern [^\n\r]* *) token env tok

let map_raw_string_literal (env : env) (tok : CST.raw_string_literal) =
  (* raw_string_literal *) token env tok

let map_semgrep_variadic_metavariable (env : env) (tok : CST.semgrep_variadic_metavariable) =
  (* pattern \$\.\.\.[A-Z_][A-Z_0-9]* *) token env tok

let map_assignment_operator (env : env) (x : CST.assignment_operator) =
  (match x with
  | `EQ tok -> R.Case ("EQ",
      (* "=" *) token env tok
    )
  | `PLUSEQ tok -> R.Case ("PLUSEQ",
      (* "+=" *) token env tok
    )
  | `DASHEQ tok -> R.Case ("DASHEQ",
      (* "-=" *) token env tok
    )
  | `STAREQ tok -> R.Case ("STAREQ",
      (* "*=" *) token env tok
    )
  | `SLASHEQ tok -> R.Case ("SLASHEQ",
      (* "/=" *) token env tok
    )
  | `PERCEQ tok -> R.Case ("PERCEQ",
      (* "%=" *) token env tok
    )
  | `AMPEQ tok -> R.Case ("AMPEQ",
      (* "&=" *) token env tok
    )
  | `HATEQ tok -> R.Case ("HATEQ",
      (* "^=" *) token env tok
    )
  | `BAREQ tok -> R.Case ("BAREQ",
      (* "|=" *) token env tok
    )
  | `LTLTEQ tok -> R.Case ("LTLTEQ",
      (* "<<=" *) token env tok
    )
  | `GTGTEQ tok -> R.Case ("GTGTEQ",
      (* ">>=" *) token env tok
    )
  | `GTGTGTEQ tok -> R.Case ("GTGTGTEQ",
      (* ">>>=" *) token env tok
    )
  | `QMARKQMARKEQ tok -> R.Case ("QMARKQMARKEQ",
      (* "??=" *) token env tok
    )
  )

let map_boolean_literal (env : env) (x : CST.boolean_literal) =
  (match x with
  | `True tok -> R.Case ("True",
      (* "true" *) token env tok
    )
  | `False tok -> R.Case ("False",
      (* "false" *) token env tok
    )
  )

let map_character_literal_unescaped (env : env) (tok : CST.character_literal_unescaped) =
  (* pattern "[^'\\\\]" *) token env tok

let map_preproc_directive_end (env : env) (tok : CST.preproc_directive_end) =
  (* preproc_directive_end *) token env tok

let map_interpolated_verbatim_string_text_fragment (env : env) (tok : CST.interpolated_verbatim_string_text_fragment) =
  (* pattern "[^{\"]+" *) token env tok

let map_string_literal_fragment (env : env) (tok : CST.string_literal_fragment) =
  (* pattern "[^\"\\\\\\n]+" *) token env tok

let map_verbatim_string_literal (env : env) (tok : CST.verbatim_string_literal) =
  (* verbatim_string_literal *) token env tok

let map_default_switch_label (env : env) ((v1, v2) : CST.default_switch_label) =
  let v1 = (* "default" *) token env v1 in
  let v2 = (* ":" *) token env v2 in
  R.Tuple [v1; v2]

let map_modifier (env : env) (x : CST.modifier) =
  (match x with
  | `Abst tok -> R.Case ("Abst",
      (* "abstract" *) token env tok
    )
  | `Async tok -> R.Case ("Async",
      (* "async" *) token env tok
    )
  | `Const tok -> R.Case ("Const",
      (* "const" *) token env tok
    )
  | `Extern tok -> R.Case ("Extern",
      (* "extern" *) token env tok
    )
  | `File tok -> R.Case ("File",
      (* "file" *) token env tok
    )
  | `Fixed tok -> R.Case ("Fixed",
      (* "fixed" *) token env tok
    )
  | `Inte tok -> R.Case ("Inte",
      (* "internal" *) token env tok
    )
  | `New tok -> R.Case ("New",
      (* "new" *) token env tok
    )
  | `Over tok -> R.Case ("Over",
      (* "override" *) token env tok
    )
  | `Part tok -> R.Case ("Part",
      (* "partial" *) token env tok
    )
  | `Priv tok -> R.Case ("Priv",
      (* "private" *) token env tok
    )
  | `Prot tok -> R.Case ("Prot",
      (* "protected" *) token env tok
    )
  | `Public tok -> R.Case ("Public",
      (* "public" *) token env tok
    )
  | `Read tok -> R.Case ("Read",
      (* "readonly" *) token env tok
    )
  | `Requ tok -> R.Case ("Requ",
      (* "required" *) token env tok
    )
  | `Sealed tok -> R.Case ("Sealed",
      (* "sealed" *) token env tok
    )
  | `Static tok -> R.Case ("Static",
      (* "static" *) token env tok
    )
  | `Unsafe tok -> R.Case ("Unsafe",
      (* "unsafe" *) token env tok
    )
  | `Virt tok -> R.Case ("Virt",
      (* "virtual" *) token env tok
    )
  | `Vola tok -> R.Case ("Vola",
      (* "volatile" *) token env tok
    )
  )

let map_interpolated_string_text_fragment (env : env) (tok : CST.interpolated_string_text_fragment) =
  (* pattern "[^{\"\\\\\\n]+" *) token env tok

let map_preproc_directive_start (env : env) (tok : CST.preproc_directive_start) =
  (* pattern #[ \t]* *) token env tok

let map_real_literal (env : env) (tok : CST.real_literal) =
  (* real_literal *) token env tok

let map_preproc_message (env : env) (tok : CST.preproc_message) =
  (* pattern [^\n\r]+ *) token env tok

let map_preproc_integer_literal (env : env) (tok : CST.preproc_integer_literal) =
  (* pattern [0-9]+ *) token env tok

let map_attribute_target_specifier (env : env) ((v1, v2) : CST.attribute_target_specifier) =
  let v1 =
    (match v1 with
    | `Field tok -> R.Case ("Field",
        (* "field" *) token env tok
      )
    | `Event tok -> R.Case ("Event",
        (* "event" *) token env tok
      )
    | `Meth tok -> R.Case ("Meth",
        (* "method" *) token env tok
      )
    | `Param tok -> R.Case ("Param",
        (* "param" *) token env tok
      )
    | `Prop tok -> R.Case ("Prop",
        (* "property" *) token env tok
      )
    | `Ret tok -> R.Case ("Ret",
        (* "return" *) token env tok
      )
    | `Type tok -> R.Case ("Type",
        (* "type" *) token env tok
      )
    )
  in
  let v2 = (* ":" *) token env v2 in
  R.Tuple [v1; v2]

let map_escape_sequence (env : env) (tok : CST.escape_sequence) =
  (* escape_sequence *) token env tok

let map_predefined_type (env : env) (tok : CST.predefined_type) =
  (* predefined_type *) token env tok

let map_pat_52ffbd7 (env : env) (tok : CST.pat_52ffbd7) =
  (* pattern "[^}\"]+" *) token env tok

let map_identifier (env : env) (x : CST.identifier) =
  (match x with
  | `Choice_id_tok x -> R.Case ("Choice_id_tok",
      (match x with
      | `Id_tok tok -> R.Case ("Id_tok",
          (* identifier_token *) token env tok
        )
      | `Cont_keywos x -> R.Case ("Cont_keywos",
          map_contextual_keywords env x
        )
      )
    )
  | `Semg_meta tok -> R.Case ("Semg_meta",
      (* semgrep_metavariable *) token env tok
    )
  )

let map_reference_directive (env : env) ((v1, v2) : CST.reference_directive) =
  let v1 = (* "r" *) token env v1 in
  let v2 = (* pattern "\"[^\"]*\"" *) token env v2 in
  R.Tuple [v1; v2]

let map_load_directive (env : env) ((v1, v2) : CST.load_directive) =
  let v1 = (* "load" *) token env v1 in
  let v2 = (* pattern "\"[^\"]*\"" *) token env v2 in
  R.Tuple [v1; v2]

let map_shebang_directive (env : env) ((v1, v2) : CST.shebang_directive) =
  let v1 = (* "!" *) token env v1 in
  let v2 = map_pat_00238b3 env v2 in
  R.Tuple [v1; v2]

let map_interpolated_raw_string_text (env : env) (x : CST.interpolated_raw_string_text) =
  (match x with
  | `Inte_verb_str_text_frag tok -> R.Case ("Inte_verb_str_text_frag",
      (* pattern "[^{\"]+" *) token env tok
    )
  | `DQUOT tok -> R.Case ("DQUOT",
      (* "\"" *) token env tok
    )
  | `DQUOTDQUOT tok -> R.Case ("DQUOTDQUOT",
      (* "\"\"" *) token env tok
    )
  )

let map_interpolated_verbatim_string_text (env : env) (x : CST.interpolated_verbatim_string_text) =
  (match x with
  | `LCURLLCURL tok -> R.Case ("LCURLLCURL",
      (* "{{" *) token env tok
    )
  | `Inte_verb_str_text_frag tok -> R.Case ("Inte_verb_str_text_frag",
      (* pattern "[^{\"]+" *) token env tok
    )
  | `DQUOTDQUOT tok -> R.Case ("DQUOTDQUOT",
      (* "\"\"" *) token env tok
    )
  )

let map_region_directive (env : env) ((v1, v2) : CST.region_directive) =
  let v1 = (* "region" *) token env v1 in
  let v2 =
    (match v2 with
    | Some tok -> R.Option (Some (
        (* pattern [^\n\r]+ *) token env tok
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2]

let map_warning_directive (env : env) ((v1, v2) : CST.warning_directive) =
  let v1 = (* "warning" *) token env v1 in
  let v2 = (* pattern [^\n\r]+ *) token env v2 in
  R.Tuple [v1; v2]

let map_endregion_directive (env : env) ((v1, v2) : CST.endregion_directive) =
  let v1 = (* "endregion" *) token env v1 in
  let v2 =
    (match v2 with
    | Some tok -> R.Option (Some (
        (* pattern [^\n\r]+ *) token env tok
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2]

let map_error_directive (env : env) ((v1, v2) : CST.error_directive) =
  let v1 = (* "error" *) token env v1 in
  let v2 = (* pattern [^\n\r]+ *) token env v2 in
  R.Tuple [v1; v2]

let map_line_directive (env : env) ((v1, v2) : CST.line_directive) =
  let v1 = (* "line" *) token env v1 in
  let v2 =
    (match v2 with
    | `Defa tok -> R.Case ("Defa",
        (* "default" *) token env tok
      )
    | `Hidden tok -> R.Case ("Hidden",
        (* "hidden" *) token env tok
      )
    | `Prep_int_lit_opt_prep_str_lit (v1, v2) -> R.Case ("Prep_int_lit_opt_prep_str_lit",
        let v1 = (* pattern [0-9]+ *) token env v1 in
        let v2 =
          (match v2 with
          | Some tok -> R.Option (Some (
              (* pattern "\"[^\"]*\"" *) token env tok
            ))
          | None -> R.Option None)
        in
        R.Tuple [v1; v2]
      )
    | `LPAR_prep_int_lit_COMMA_prep_int_lit_RPAR_DASH_LPAR_prep_int_lit_COMMA_prep_int_lit_RPAR_opt_prep_int_lit_prep_str_lit (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13) -> R.Case ("LPAR_prep_int_lit_COMMA_prep_int_lit_RPAR_DASH_LPAR_prep_int_lit_COMMA_prep_int_lit_RPAR_opt_prep_int_lit_prep_str_lit",
        let v1 = (* "(" *) token env v1 in
        let v2 = (* pattern [0-9]+ *) token env v2 in
        let v3 = (* "," *) token env v3 in
        let v4 = (* pattern [0-9]+ *) token env v4 in
        let v5 = (* ")" *) token env v5 in
        let v6 = (* "-" *) token env v6 in
        let v7 = (* "(" *) token env v7 in
        let v8 = (* pattern [0-9]+ *) token env v8 in
        let v9 = (* "," *) token env v9 in
        let v10 = (* pattern [0-9]+ *) token env v10 in
        let v11 = (* ")" *) token env v11 in
        let v12 =
          (match v12 with
          | Some tok -> R.Option (Some (
              (* pattern [0-9]+ *) token env tok
            ))
          | None -> R.Option None)
        in
        let v13 = (* pattern "\"[^\"]*\"" *) token env v13 in
        R.Tuple [v1; v2; v3; v4; v5; v6; v7; v8; v9; v10; v11; v12; v13]
      )
    )
  in
  R.Tuple [v1; v2]

let map_interpolated_string_text (env : env) (x : CST.interpolated_string_text) =
  (match x with
  | `LCURLLCURL tok -> R.Case ("LCURLLCURL",
      (* "{{" *) token env tok
    )
  | `Inte_str_text_frag tok -> R.Case ("Inte_str_text_frag",
      (* pattern "[^{\"\\\\\\n]+" *) token env tok
    )
  | `Esc_seq tok -> R.Case ("Esc_seq",
      (* escape_sequence *) token env tok
    )
  )

let map_interpolation_format_clause (env : env) ((v1, v2) : CST.interpolation_format_clause) =
  let v1 = (* ":" *) token env v1 in
  let v2 = map_pat_52ffbd7 env v2 in
  R.Tuple [v1; v2]

let map_implicit_parameter (env : env) (x : CST.implicit_parameter) =
  map_identifier env x

let map_literal (env : env) (x : CST.literal) =
  (match x with
  | `Null_lit tok -> R.Case ("Null_lit",
      (* "null" *) token env tok
    )
  | `Bool_lit x -> R.Case ("Bool_lit",
      map_boolean_literal env x
    )
  | `Char_lit (v1, v2, v3) -> R.Case ("Char_lit",
      let v1 = (* "'" *) token env v1 in
      let v2 =
        (match v2 with
        | `Char_lit_unes tok -> R.Case ("Char_lit_unes",
            (* pattern "[^'\\\\]" *) token env tok
          )
        | `Esc_seq tok -> R.Case ("Esc_seq",
            (* escape_sequence *) token env tok
          )
        )
      in
      let v3 = (* "'" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Real_lit tok -> R.Case ("Real_lit",
      (* real_literal *) token env tok
    )
  | `Int_lit tok -> R.Case ("Int_lit",
      (* integer_literal *) token env tok
    )
  | `Str_lit (v1, v2, v3, v4) -> R.Case ("Str_lit",
      let v1 = (* "\"" *) token env v1 in
      let v2 =
        R.List (List.map (fun x ->
          (match x with
          | `Str_lit_frag tok -> R.Case ("Str_lit_frag",
              (* pattern "[^\"\\\\\\n]+" *) token env tok
            )
          | `Esc_seq tok -> R.Case ("Esc_seq",
              (* escape_sequence *) token env tok
            )
          )
        ) v2)
      in
      let v3 = (* "\"" *) token env v3 in
      let v4 =
        (match v4 with
        | Some tok -> R.Option (Some (
            (* pattern (u|U)8 *) token env tok
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Verb_str_lit tok -> R.Case ("Verb_str_lit",
      (* verbatim_string_literal *) token env tok
    )
  | `Raw_str_lit tok -> R.Case ("Raw_str_lit",
      (* raw_string_literal *) token env tok
    )
  )

let map_implicit_parameter_list (env : env) (x : CST.implicit_parameter_list) =
  map_implicit_parameter env x

let map_function_pointer_unmanaged_calling_convention (env : env) (x : CST.function_pointer_unmanaged_calling_convention) =
  (match x with
  | `Cdecl tok -> R.Case ("Cdecl",
      (* "Cdecl" *) token env tok
    )
  | `Stdc tok -> R.Case ("Stdc",
      (* "Stdcall" *) token env tok
    )
  | `This tok -> R.Case ("This",
      (* "Thiscall" *) token env tok
    )
  | `Fast tok -> R.Case ("Fast",
      (* "Fastcall" *) token env tok
    )
  | `Id x -> R.Case ("Id",
      map_implicit_parameter_list env x
    )
  )

let rec map_preproc_binary_expression (env : env) (x : CST.preproc_binary_expression) =
  (match x with
  | `Prep_exp_BARBAR_prep_exp (v1, v2, v3) -> R.Case ("Prep_exp_BARBAR_prep_exp",
      let v1 = map_preproc_expression env v1 in
      let v2 = (* "||" *) token env v2 in
      let v3 = map_preproc_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Prep_exp_AMPAMP_prep_exp (v1, v2, v3) -> R.Case ("Prep_exp_AMPAMP_prep_exp",
      let v1 = map_preproc_expression env v1 in
      let v2 = (* "&&" *) token env v2 in
      let v3 = map_preproc_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Prep_exp_EQEQ_prep_exp (v1, v2, v3) -> R.Case ("Prep_exp_EQEQ_prep_exp",
      let v1 = map_preproc_expression env v1 in
      let v2 = (* "==" *) token env v2 in
      let v3 = map_preproc_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Prep_exp_BANGEQ_prep_exp (v1, v2, v3) -> R.Case ("Prep_exp_BANGEQ_prep_exp",
      let v1 = map_preproc_expression env v1 in
      let v2 = (* "!=" *) token env v2 in
      let v3 = map_preproc_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_preproc_expression (env : env) (x : CST.preproc_expression) =
  (match x with
  | `Id x -> R.Case ("Id",
      map_implicit_parameter_list env x
    )
  | `Bool_lit x -> R.Case ("Bool_lit",
      map_boolean_literal env x
    )
  | `Prep_int_lit tok -> R.Case ("Prep_int_lit",
      (* pattern [0-9]+ *) token env tok
    )
  | `Prep_str_lit tok -> R.Case ("Prep_str_lit",
      (* pattern "\"[^\"]*\"" *) token env tok
    )
  | `Prep_un_exp (v1, v2) -> R.Case ("Prep_un_exp",
      let v1 = (* "!" *) token env v1 in
      let v2 = map_preproc_expression env v2 in
      R.Tuple [v1; v2]
    )
  | `Prep_bin_exp x -> R.Case ("Prep_bin_exp",
      map_preproc_binary_expression env x
    )
  | `Prep_paren_exp (v1, v2, v3) -> R.Case ("Prep_paren_exp",
      let v1 = (* "(" *) token env v1 in
      let v2 = map_preproc_expression env v2 in
      let v3 = (* ")" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

let map_join_into_clause (env : env) ((v1, v2) : CST.join_into_clause) =
  let v1 = (* "into" *) token env v1 in
  let v2 = map_implicit_parameter_list env v2 in
  R.Tuple [v1; v2]

let map_identifier_or_global (env : env) (x : CST.identifier_or_global) =
  (match x with
  | `Global tok -> R.Case ("Global",
      (* "global" *) token env tok
    )
  | `Id x -> R.Case ("Id",
      map_implicit_parameter_list env x
    )
  )

let rec map_anon_choice_impl_param_c036834 (env : env) (x : CST.anon_choice_impl_param_c036834) =
  (match x with
  | `Id x -> R.Case ("Id",
      map_implicit_parameter_list env x
    )
  | `Disc tok -> R.Case ("Disc",
      (* "_" *) token env tok
    )
  | `Tuple_pat x -> R.Case ("Tuple_pat",
      map_tuple_pattern env x
    )
  )

and map_tuple_pattern (env : env) ((v1, v2, v3, v4) : CST.tuple_pattern) =
  let v1 = (* "(" *) token env v1 in
  let v2 = map_anon_choice_impl_param_c036834 env v2 in
  let v3 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_anon_choice_impl_param_c036834 env v2 in
      R.Tuple [v1; v2]
    ) v3)
  in
  let v4 = (* ")" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

let map_undef_directive (env : env) ((v1, v2) : CST.undef_directive) =
  let v1 = (* "undef" *) token env v1 in
  let v2 = map_implicit_parameter_list env v2 in
  R.Tuple [v1; v2]

let rec map_variable_designation (env : env) (x : CST.variable_designation) =
  (match x with
  | `Disc tok -> R.Case ("Disc",
      (* "_" *) token env tok
    )
  | `Paren_var_desi (v1, v2, v3) -> R.Case ("Paren_var_desi",
      let v1 = (* "(" *) token env v1 in
      let v2 =
        (match v2 with
        | Some (v1, v2) -> R.Option (Some (
            let v1 = map_variable_designation env v1 in
            let v2 =
              R.List (List.map (fun (v1, v2) ->
                let v1 = (* "," *) token env v1 in
                let v2 = map_variable_designation env v2 in
                R.Tuple [v1; v2]
              ) v2)
            in
            R.Tuple [v1; v2]
          ))
        | None -> R.Option None)
      in
      let v3 = (* ")" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Id x -> R.Case ("Id",
      map_implicit_parameter_list env x
    )
  )

let map_define_directive (env : env) ((v1, v2) : CST.define_directive) =
  let v1 = (* "define" *) token env v1 in
  let v2 = map_implicit_parameter_list env v2 in
  R.Tuple [v1; v2]

let map_extern_alias_directive (env : env) ((v1, v2, v3, v4) : CST.extern_alias_directive) =
  let v1 = (* "extern" *) token env v1 in
  let v2 = (* "alias" *) token env v2 in
  let v3 = map_implicit_parameter_list env v3 in
  let v4 = (* ";" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

let map_anon_choice_impl_param_c290f8e (env : env) (x : CST.anon_choice_impl_param_c290f8e) =
  (match x with
  | `Id x -> R.Case ("Id",
      map_implicit_parameter_list env x
    )
  | `Prep_int_lit tok -> R.Case ("Prep_int_lit",
      (* pattern [0-9]+ *) token env tok
    )
  )

let map_function_pointer_unmanaged_calling_convention_list (env : env) ((v1, v2, v3, v4) : CST.function_pointer_unmanaged_calling_convention_list) =
  let v1 = (* "[" *) token env v1 in
  let v2 =
    map_function_pointer_unmanaged_calling_convention env v2
  in
  let v3 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 =
        map_function_pointer_unmanaged_calling_convention env v2
      in
      R.Tuple [v1; v2]
    ) v3)
  in
  let v4 = (* "]" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

let map_if_directive (env : env) ((v1, v2) : CST.if_directive) =
  let v1 = (* "if" *) token env v1 in
  let v2 = map_preproc_expression env v2 in
  R.Tuple [v1; v2]

let map_elif_directive (env : env) ((v1, v2) : CST.elif_directive) =
  let v1 = (* "elif" *) token env v1 in
  let v2 = map_preproc_expression env v2 in
  R.Tuple [v1; v2]

let map_name_equals (env : env) ((v1, v2) : CST.name_equals) =
  let v1 = map_identifier_or_global env v1 in
  let v2 = (* "=" *) token env v2 in
  R.Tuple [v1; v2]

let map_name_colon (env : env) ((v1, v2) : CST.name_colon) =
  let v1 = map_identifier_or_global env v1 in
  let v2 = (* ":" *) token env v2 in
  R.Tuple [v1; v2]

let map_anon_choice_impl_param_bf14316 (env : env) (x : CST.anon_choice_impl_param_bf14316) =
  (match x with
  | `Id x -> R.Case ("Id",
      map_implicit_parameter_list env x
    )
  | `Tuple_pat x -> R.Case ("Tuple_pat",
      map_tuple_pattern env x
    )
  )

let map_pragma_directive (env : env) ((v1, v2) : CST.pragma_directive) =
  let v1 = (* "pragma" *) token env v1 in
  let v2 =
    (match v2 with
    | `Warn_choice_disa_opt_choice_id_rep_COMMA_choice_id (v1, v2, v3) -> R.Case ("Warn_choice_disa_opt_choice_id_rep_COMMA_choice_id",
        let v1 = (* "warning" *) token env v1 in
        let v2 =
          (match v2 with
          | `Disa tok -> R.Case ("Disa",
              (* "disable" *) token env tok
            )
          | `Rest tok -> R.Case ("Rest",
              (* "restore" *) token env tok
            )
          )
        in
        let v3 =
          (match v3 with
          | Some (v1, v2) -> R.Option (Some (
              let v1 = map_anon_choice_impl_param_c290f8e env v1 in
              let v2 =
                R.List (List.map (fun (v1, v2) ->
                  let v1 = (* "," *) token env v1 in
                  let v2 = map_anon_choice_impl_param_c290f8e env v2 in
                  R.Tuple [v1; v2]
                ) v2)
              in
              R.Tuple [v1; v2]
            ))
          | None -> R.Option None)
        in
        R.Tuple [v1; v2; v3]
      )
    | `Chec_prep_str_lit_prep_str_lit_prep_str_lit (v1, v2, v3, v4) -> R.Case ("Chec_prep_str_lit_prep_str_lit_prep_str_lit",
        let v1 = (* "checksum" *) token env v1 in
        let v2 = (* pattern "\"[^\"]*\"" *) token env v2 in
        let v3 = (* pattern "\"[^\"]*\"" *) token env v3 in
        let v4 = (* pattern "\"[^\"]*\"" *) token env v4 in
        R.Tuple [v1; v2; v3; v4]
      )
    )
  in
  R.Tuple [v1; v2]

let map_function_pointer_calling_convention (env : env) (x : CST.function_pointer_calling_convention) =
  (match x with
  | `Mana tok -> R.Case ("Mana",
      (* "managed" *) token env tok
    )
  | `Unma_opt_func_poin_unma_call_conv_list (v1, v2) -> R.Case ("Unma_opt_func_poin_unma_call_conv_list",
      let v1 = (* "unmanaged" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_function_pointer_unmanaged_calling_convention_list env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2]
    )
  )

let rec map_anon_choice_exp_3bb8381 (env : env) (x : CST.anon_choice_exp_3bb8381) =
  (match x with
  | `Exp x -> R.Case ("Exp",
      map_expression env x
    )
  | `Pred_type tok -> R.Case ("Pred_type",
      (* predefined_type *) token env tok
    )
  | `Name x -> R.Case ("Name",
      map_type_name env x
    )
  )

and map_anon_choice_param_ce11a32 (env : env) (x : CST.anon_choice_param_ce11a32) =
  (match x with
  | `Param x -> R.Case ("Param",
      map_parameter env x
    )
  | `Param_array (v1, v2, v3, v4) -> R.Case ("Param_array",
      let v1 = R.List (List.map (map_attribute_list env) v1) in
      let v2 = (* "params" *) token env v2 in
      let v3 =
        (match v3 with
        | `Array_type x -> R.Case ("Array_type",
            map_array_type env x
          )
        | `Null_type x -> R.Case ("Null_type",
            map_nullable_type env x
          )
        )
      in
      let v4 = map_implicit_parameter_list env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  )

and map_anon_choice_pat_29be9ad (env : env) (x : CST.anon_choice_pat_29be9ad) =
  (match x with
  | `Pat x -> R.Case ("Pat",
      map_pattern env x
    )
  | `Slice_pat tok -> R.Case ("Slice_pat",
      (* ".." *) token env tok
    )
  )

and map_anon_opt_exp_rep_interp_alig_clause_cd88eaa (env : env) (opt : CST.anon_opt_exp_rep_interp_alig_clause_cd88eaa) =
  (match opt with
  | Some (v1, v2) -> R.Option (Some (
      let v1 = map_expression env v1 in
      let v2 =
        R.List (List.map (map_interpolation_alignment_clause env) v2)
      in
      R.Tuple [v1; v2]
    ))
  | None -> R.Option None)

and map_anonymous_object_member_declarator (env : env) (x : CST.anonymous_object_member_declarator) =
  (match x with
  | `Name_equals_exp (v1, v2) -> R.Case ("Name_equals_exp",
      let v1 = map_name_equals env v1 in
      let v2 = map_expression env v2 in
      R.Tuple [v1; v2]
    )
  | `Exp x -> R.Case ("Exp",
      map_expression env x
    )
  )

and map_argument (env : env) (x : CST.argument) =
  (match x with
  | `Opt_name_colon_opt_choice_ref_choice_exp (v1, v2, v3) -> R.Case ("Opt_name_colon_opt_choice_ref_choice_exp",
      let v1 =
        (match v1 with
        | Some x -> R.Option (Some (
            map_name_colon env x
          ))
        | None -> R.Option None)
      in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_anon_choice_ref_eec35e8 env x
          ))
        | None -> R.Option None)
      in
      let v3 =
        (match v3 with
        | `Exp x -> R.Case ("Exp",
            map_expression env x
          )
        | `Decl_exp x -> R.Case ("Decl_exp",
            map_declaration_expression env x
          )
        )
      in
      R.Tuple [v1; v2; v3]
    )
  | `Semg_vari_meta tok -> R.Case ("Semg_vari_meta",
      (* pattern \$\.\.\.[A-Z_][A-Z_0-9]* *) token env tok
    )
  )

and map_argument_list (env : env) ((v1, v2, v3) : CST.argument_list) =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = map_argument env v1 in
        let v2 =
          R.List (List.map (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_argument env v2 in
            R.Tuple [v1; v2]
          ) v2)
        in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  let v3 = (* ")" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_array_base_type (env : env) (x : CST.array_base_type) =
  (match x with
  | `Array_type x -> R.Case ("Array_type",
      map_array_type env x
    )
  | `Type_name x -> R.Case ("Type_name",
      map_type_name env x
    )
  | `Null_type x -> R.Case ("Null_type",
      map_nullable_type env x
    )
  | `Poin_type x -> R.Case ("Poin_type",
      map_pointer_type env x
    )
  | `Func_poin_type x -> R.Case ("Func_poin_type",
      map_function_pointer_type env x
    )
  | `Pred_type tok -> R.Case ("Pred_type",
      (* predefined_type *) token env tok
    )
  | `Tuple_type x -> R.Case ("Tuple_type",
      map_tuple_type env x
    )
  )

and map_array_rank_specifier (env : env) ((v1, v2, v3) : CST.array_rank_specifier) =
  let v1 = (* "[" *) token env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 =
          (match v1 with
          | Some x -> R.Option (Some (
              map_expression env x
            ))
          | None -> R.Option None)
        in
        let v2 =
          R.List (List.map (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 =
              (match v2 with
              | Some x -> R.Option (Some (
                  map_expression env x
                ))
              | None -> R.Option None)
            in
            R.Tuple [v1; v2]
          ) v2)
        in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  let v3 = (* "]" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_array_type (env : env) ((v1, v2) : CST.array_type) =
  let v1 = map_array_base_type env v1 in
  let v2 = map_array_rank_specifier env v2 in
  R.Tuple [v1; v2]

and map_arrow_expression_clause (env : env) ((v1, v2) : CST.arrow_expression_clause) =
  let v1 = (* "=>" *) token env v1 in
  let v2 = map_expression env v2 in
  R.Tuple [v1; v2]

and map_attribute (env : env) ((v1, v2) : CST.attribute) =
  let v1 = map_type_name env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_attribute_argument_list env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2]

and map_attribute_argument (env : env) ((v1, v2) : CST.attribute_argument) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        (match x with
        | `Name_equals x -> R.Case ("Name_equals",
            map_name_equals env x
          )
        | `Name_colon x -> R.Case ("Name_colon",
            map_name_colon env x
          )
        )
      ))
    | None -> R.Option None)
  in
  let v2 = map_expression env v2 in
  R.Tuple [v1; v2]

and map_attribute_argument_list (env : env) ((v1, v2, v3) : CST.attribute_argument_list) =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = map_attribute_argument env v1 in
        let v2 =
          R.List (List.map (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_attribute_argument env v2 in
            R.Tuple [v1; v2]
          ) v2)
        in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  let v3 = (* ")" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_attribute_list (env : env) ((v1, v2, v3, v4, v5, v6) : CST.attribute_list) =
  let v1 = (* "[" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_attribute_target_specifier env x
      ))
    | None -> R.Option None)
  in
  let v3 = map_attribute env v3 in
  let v4 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_attribute env v2 in
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
  let v6 = (* "]" *) token env v6 in
  R.Tuple [v1; v2; v3; v4; v5; v6]

and map_binary_expression (env : env) (x : CST.binary_expression) =
  (match x with
  | `Exp_AMPAMP_exp (v1, v2, v3) -> R.Case ("Exp_AMPAMP_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "&&" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_BARBAR_exp (v1, v2, v3) -> R.Case ("Exp_BARBAR_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "||" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_GTGT_exp (v1, v2, v3) -> R.Case ("Exp_GTGT_exp",
      let v1 = map_expression env v1 in
      let v2 = (* ">>" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_GTGTGT_exp (v1, v2, v3) -> R.Case ("Exp_GTGTGT_exp",
      let v1 = map_expression env v1 in
      let v2 = (* ">>>" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_LTLT_exp (v1, v2, v3) -> R.Case ("Exp_LTLT_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "<<" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_AMP_exp (v1, v2, v3) -> R.Case ("Exp_AMP_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "&" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_HAT_exp (v1, v2, v3) -> R.Case ("Exp_HAT_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "^" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_BAR_exp (v1, v2, v3) -> R.Case ("Exp_BAR_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "|" *) token env v2 in
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
  | `Exp_PERC_exp (v1, v2, v3) -> R.Case ("Exp_PERC_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "%" *) token env v2 in
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
  | `Exp_BANGEQ_exp (v1, v2, v3) -> R.Case ("Exp_BANGEQ_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "!=" *) token env v2 in
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
  | `Exp_QMARKQMARK_exp (v1, v2, v3) -> R.Case ("Exp_QMARKQMARK_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "??" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_block (env : env) ((v1, v2, v3) : CST.block) =
  let v1 = (* "{" *) token env v1 in
  let v2 = R.List (List.map (map_global_statement env) v2) in
  let v3 = (* "}" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_bracketed_argument_list (env : env) ((v1, v2, v3, v4) : CST.bracketed_argument_list) =
  let v1 = (* "[" *) token env v1 in
  let v2 = map_argument env v2 in
  let v3 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_argument env v2 in
      R.Tuple [v1; v2]
    ) v3)
  in
  let v4 = (* "]" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_case_pattern_switch_label (env : env) ((v1, v2, v3, v4) : CST.case_pattern_switch_label) =
  let v1 = (* "case" *) token env v1 in
  let v2 = map_pattern env v2 in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_when_clause env x
      ))
    | None -> R.Option None)
  in
  let v4 = (* ":" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_case_switch_label (env : env) ((v1, v2, v3) : CST.case_switch_label) =
  let v1 = (* "case" *) token env v1 in
  let v2 = map_expression env v2 in
  let v3 = (* ":" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_cast_expression (env : env) ((v1, v2, v3, v4) : CST.cast_expression) =
  let v1 = (* "(" *) token env v1 in
  let v2 = map_type_pattern env v2 in
  let v3 = (* ")" *) token env v3 in
  let v4 = map_expression env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_catch_clause (env : env) ((v1, v2, v3, v4) : CST.catch_clause) =
  let v1 = (* "catch" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_catch_declaration env x
      ))
    | None -> R.Option None)
  in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_catch_filter_clause env x
      ))
    | None -> R.Option None)
  in
  let v4 = map_block env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_catch_declaration (env : env) ((v1, v2, v3, v4) : CST.catch_declaration) =
  let v1 = (* "(" *) token env v1 in
  let v2 = map_type_pattern env v2 in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_implicit_parameter_list env x
      ))
    | None -> R.Option None)
  in
  let v4 = (* ")" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_catch_filter_clause (env : env) ((v1, v2, v3, v4) : CST.catch_filter_clause) =
  let v1 = (* "when" *) token env v1 in
  let v2 = (* "(" *) token env v2 in
  let v3 = map_expression env v3 in
  let v4 = (* ")" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_checked_expression (env : env) (x : CST.checked_expression) =
  (match x with
  | `Chec_LPAR_exp_RPAR (v1, v2, v3, v4) -> R.Case ("Chec_LPAR_exp_RPAR",
      let v1 = (* "checked" *) token env v1 in
      let v2 = (* "(" *) token env v2 in
      let v3 = map_expression env v3 in
      let v4 = (* ")" *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Unch_LPAR_exp_RPAR (v1, v2, v3, v4) -> R.Case ("Unch_LPAR_exp_RPAR",
      let v1 = (* "unchecked" *) token env v1 in
      let v2 = (* "(" *) token env v2 in
      let v3 = map_expression env v3 in
      let v4 = (* ")" *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  )

and map_constant_pattern (env : env) (x : CST.constant_pattern) =
  (match x with
  | `Bin_exp x -> R.Case ("Bin_exp",
      map_binary_expression env x
    )
  | `Defa_exp x -> R.Case ("Defa_exp",
      map_default_expression env x
    )
  | `Inte_str_exp x -> R.Case ("Inte_str_exp",
      map_interpolated_string_expression env x
    )
  | `Paren_exp x -> R.Case ("Paren_exp",
      map_parenthesized_expression env x
    )
  | `Post_un_exp x -> R.Case ("Post_un_exp",
      map_postfix_unary_expression env x
    )
  | `Prefix_un_exp x -> R.Case ("Prefix_un_exp",
      map_prefix_unary_expression env x
    )
  | `Size_of_exp x -> R.Case ("Size_of_exp",
      map_size_of_expression env x
    )
  | `Tuple_exp x -> R.Case ("Tuple_exp",
      map_tuple_expression env x
    )
  | `Type_of_exp x -> R.Case ("Type_of_exp",
      map_type_of_expression env x
    )
  | `Member_access_exp x -> R.Case ("Member_access_exp",
      map_member_access_expression env x
    )
  | `Invo_exp x -> R.Case ("Invo_exp",
      map_invocation_expression env x
    )
  | `Cast_exp x -> R.Case ("Cast_exp",
      map_cast_expression env x
    )
  | `Simple_name x -> R.Case ("Simple_name",
      map_simple_name env x
    )
  | `Lit x -> R.Case ("Lit",
      map_literal env x
    )
  )

and map_declaration_expression (env : env) ((v1, v2) : CST.declaration_expression) =
  let v1 = map_type_pattern env v1 in
  let v2 = map_implicit_parameter_list env v2 in
  R.Tuple [v1; v2]

and map_deep_ellipsis (env : env) ((v1, v2, v3) : CST.deep_ellipsis) =
  let v1 = (* "<..." *) token env v1 in
  let v2 = map_expression env v2 in
  let v3 = (* "...>" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_default_expression (env : env) ((v1, v2) : CST.default_expression) =
  let v1 = (* "default" *) token env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2, v3) -> R.Option (Some (
        let v1 = (* "(" *) token env v1 in
        let v2 = map_type_pattern env v2 in
        let v3 = (* ")" *) token env v3 in
        R.Tuple [v1; v2; v3]
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2]

and map_element_binding_expression (env : env) (x : CST.element_binding_expression) =
  map_bracketed_argument_list env x

and map_equals_value_clause (env : env) ((v1, v2) : CST.equals_value_clause) =
  let v1 = (* "=" *) token env v1 in
  let v2 = map_expression env v2 in
  R.Tuple [v1; v2]

and map_expression (env : env) (x : CST.expression) =
  (match x with
  | `Non_lvalue_exp x -> R.Case ("Non_lvalue_exp",
      map_non_lvalue_expression env x
    )
  | `Lvalue_exp x -> R.Case ("Lvalue_exp",
      map_lvalue_expression env x
    )
  | `Ellips tok -> R.Case ("Ellips",
      (* "..." *) token env tok
    )
  | `Deep_ellips x -> R.Case ("Deep_ellips",
      map_deep_ellipsis env x
    )
  | `Member_access_ellips_exp x -> R.Case ("Member_access_ellips_exp",
      map_member_access_ellipsis_expression env x
    )
  | `Typed_meta x -> R.Case ("Typed_meta",
      map_typed_metavariable env x
    )
  )

and map_expression_colon (env : env) ((v1, v2) : CST.expression_colon) =
  let v1 = map_expression env v1 in
  let v2 = (* ":" *) token env v2 in
  R.Tuple [v1; v2]

and map_expression_statement (env : env) (x : CST.expression_statement) =
  (match x with
  | `Exp_stmt_exp_SEMI (v1, v2) -> R.Case ("Exp_stmt_exp_SEMI",
      let v1 = map_expression_statement_expression env v1 in
      let v2 = (* ";" *) token env v2 in
      R.Tuple [v1; v2]
    )
  | `Ellips_SEMI (v1, v2) -> R.Case ("Ellips_SEMI",
      let v1 = (* "..." *) token env v1 in
      let v2 = (* ";" *) token env v2 in
      R.Tuple [v1; v2]
    )
  | `Ellips tok -> R.Case ("Ellips",
      (* "..." *) token env tok
    )
  | `Deep_ellips_SEMI (v1, v2) -> R.Case ("Deep_ellips_SEMI",
      let v1 = map_deep_ellipsis env v1 in
      let v2 = (* ";" *) token env v2 in
      R.Tuple [v1; v2]
    )
  | `Member_access_ellips_exp_SEMI (v1, v2) -> R.Case ("Member_access_ellips_exp_SEMI",
      let v1 = map_member_access_ellipsis_expression env v1 in
      let v2 = (* ";" *) token env v2 in
      R.Tuple [v1; v2]
    )
  | `Semg_meta_SEMI (v1, v2) -> R.Case ("Semg_meta_SEMI",
      let v1 = (* semgrep_metavariable *) token env v1 in
      let v2 = (* ";" *) token env v2 in
      R.Tuple [v1; v2]
    )
  | `Typed_meta_SEMI (v1, v2) -> R.Case ("Typed_meta_SEMI",
      let v1 = map_typed_metavariable env v1 in
      let v2 = (* ";" *) token env v2 in
      R.Tuple [v1; v2]
    )
  )

and map_expression_statement_expression (env : env) (x : CST.expression_statement_expression) =
  (match x with
  | `Assign_exp (v1, v2, v3) -> R.Case ("Assign_exp",
      let v1 = map_lvalue_expression env v1 in
      let v2 = map_assignment_operator env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Invo_exp x -> R.Case ("Invo_exp",
      map_invocation_expression env x
    )
  | `Post_un_exp x -> R.Case ("Post_un_exp",
      map_postfix_unary_expression env x
    )
  | `Prefix_un_exp x -> R.Case ("Prefix_un_exp",
      map_prefix_unary_expression env x
    )
  | `Await_exp (v1, v2) -> R.Case ("Await_exp",
      let v1 = (* "await" *) token env v1 in
      let v2 = map_expression env v2 in
      R.Tuple [v1; v2]
    )
  | `Obj_crea_exp (v1, v2, v3, v4) -> R.Case ("Obj_crea_exp",
      let v1 = (* "new" *) token env v1 in
      let v2 = map_object_creation_type env v2 in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_argument_list env x
          ))
        | None -> R.Option None)
      in
      let v4 =
        (match v4 with
        | Some x -> R.Option (Some (
            map_initializer_expression env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Paren_exp x -> R.Case ("Paren_exp",
      map_parenthesized_expression env x
    )
  )

and map_finally_clause (env : env) ((v1, v2) : CST.finally_clause) =
  let v1 = (* "finally" *) token env v1 in
  let v2 = map_block env v2 in
  R.Tuple [v1; v2]

and map_formal_parameter_list (env : env) ((v1, v2) : CST.formal_parameter_list) =
  let v1 = map_anon_choice_param_ce11a32 env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_anon_choice_param_ce11a32 env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  R.Tuple [v1; v2]

and map_from_clause (env : env) ((v1, v2, v3, v4, v5) : CST.from_clause) =
  let v1 = (* "from" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_type_pattern env x
      ))
    | None -> R.Option None)
  in
  let v3 = map_implicit_parameter_list env v3 in
  let v4 = (* "in" *) token env v4 in
  let v5 = map_expression env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

and map_function_body (env : env) (x : CST.function_body) =
  (match x with
  | `Blk x -> R.Case ("Blk",
      map_block env x
    )
  | `Arrow_exp_clause_SEMI (v1, v2) -> R.Case ("Arrow_exp_clause_SEMI",
      let v1 = map_arrow_expression_clause env v1 in
      let v2 = (* ";" *) token env v2 in
      R.Tuple [v1; v2]
    )
  | `SEMI tok -> R.Case ("SEMI",
      (* ";" *) token env tok
    )
  )

and map_function_pointer_parameter (env : env) ((v1, v2) : CST.function_pointer_parameter) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        map_anon_choice_ref_eec35e8 env x
      ))
    | None -> R.Option None)
  in
  let v2 = map_ref_base_type env v2 in
  R.Tuple [v1; v2]

and map_function_pointer_return_type (env : env) (x : CST.function_pointer_return_type) =
  map_type_ env x

and map_function_pointer_type (env : env) ((v1, v2, v3, v4, v5, v6, v7) : CST.function_pointer_type) =
  let v1 = (* "delegate" *) token env v1 in
  let v2 = (* "*" *) token env v2 in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_function_pointer_calling_convention env x
      ))
    | None -> R.Option None)
  in
  let v4 = (* "<" *) token env v4 in
  let v5 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = map_function_pointer_parameter env v1 in
      let v2 = (* "," *) token env v2 in
      R.Tuple [v1; v2]
    ) v5)
  in
  let v6 = map_function_pointer_return_type env v6 in
  let v7 = (* ">" *) token env v7 in
  R.Tuple [v1; v2; v3; v4; v5; v6; v7]

and map_global_statement (env : env) (x : CST.global_statement) =
  map_statement env x

and map_initializer_expression (env : env) ((v1, v2, v3, v4) : CST.initializer_expression) =
  let v1 = (* "{" *) token env v1 in
  let v2 =
    map_anon_opt_exp_rep_interp_alig_clause_cd88eaa env v2
  in
  let v3 =
    (match v3 with
    | Some tok -> R.Option (Some (
        (* "," *) token env tok
      ))
    | None -> R.Option None)
  in
  let v4 = (* "}" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_interpolated_raw_string_content (env : env) (x : CST.interpolated_raw_string_content) =
  (match x with
  | `Inte_raw_str_text x -> R.Case ("Inte_raw_str_text",
      map_interpolated_raw_string_text env x
    )
  | `Interp x -> R.Case ("Interp",
      map_interpolation env x
    )
  )

and map_interpolated_string_content (env : env) (x : CST.interpolated_string_content) =
  (match x with
  | `Inte_str_text x -> R.Case ("Inte_str_text",
      map_interpolated_string_text env x
    )
  | `Interp x -> R.Case ("Interp",
      map_interpolation env x
    )
  )

and map_interpolated_string_expression (env : env) (x : CST.interpolated_string_expression) =
  (match x with
  | `DOLLARDQUOT_rep_inte_str_content_DQUOT (v1, v2, v3) -> R.Case ("DOLLARDQUOT_rep_inte_str_content_DQUOT",
      let v1 = (* "$\"" *) token env v1 in
      let v2 =
        R.List (List.map (map_interpolated_string_content env) v2)
      in
      let v3 = (* "\"" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `DOLLARATDQUOT_rep_inte_verb_str_content_DQUOT (v1, v2, v3) -> R.Case ("DOLLARATDQUOT_rep_inte_verb_str_content_DQUOT",
      let v1 = (* "$@\"" *) token env v1 in
      let v2 =
        R.List (List.map (map_interpolated_verbatim_string_content env) v2)
      in
      let v3 = (* "\"" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `ATDOLLARDQUOT_rep_inte_verb_str_content_DQUOT (v1, v2, v3) -> R.Case ("ATDOLLARDQUOT_rep_inte_verb_str_content_DQUOT",
      let v1 = (* "@$\"" *) token env v1 in
      let v2 =
        R.List (List.map (map_interpolated_verbatim_string_content env) v2)
      in
      let v3 = (* "\"" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `DOLLARDQUOTDQUOTDQUOT_rep_inte_raw_str_content_DQUOTDQUOTDQUOT (v1, v2, v3) -> R.Case ("DOLLARDQUOTDQUOTDQUOT_rep_inte_raw_str_content_DQUOTDQUOTDQUOT",
      let v1 = (* "$\"\"\"" *) token env v1 in
      let v2 =
        R.List (List.map (map_interpolated_raw_string_content env) v2)
      in
      let v3 = (* "\"\"\"" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_interpolated_verbatim_string_content (env : env) (x : CST.interpolated_verbatim_string_content) =
  (match x with
  | `Inte_verb_str_text x -> R.Case ("Inte_verb_str_text",
      map_interpolated_verbatim_string_text env x
    )
  | `Interp x -> R.Case ("Interp",
      map_interpolation env x
    )
  )

and map_interpolation (env : env) ((v1, v2, v3, v4, v5) : CST.interpolation) =
  let v1 = (* "{" *) token env v1 in
  let v2 = map_expression env v2 in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_interpolation_alignment_clause env x
      ))
    | None -> R.Option None)
  in
  let v4 =
    (match v4 with
    | Some x -> R.Option (Some (
        map_interpolation_format_clause env x
      ))
    | None -> R.Option None)
  in
  let v5 = (* "}" *) token env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

and map_interpolation_alignment_clause (env : env) ((v1, v2) : CST.interpolation_alignment_clause) =
  let v1 = (* "," *) token env v1 in
  let v2 = map_expression env v2 in
  R.Tuple [v1; v2]

and map_invocation_expression (env : env) ((v1, v2) : CST.invocation_expression) =
  let v1 = map_expression env v1 in
  let v2 = map_argument_list env v2 in
  R.Tuple [v1; v2]

and map_lvalue_expression (env : env) (x : CST.lvalue_expression) =
  (match x with
  | `This_exp tok -> R.Case ("This_exp",
      (* "this" *) token env tok
    )
  | `Member_access_exp x -> R.Case ("Member_access_exp",
      map_member_access_expression env x
    )
  | `Tuple_exp x -> R.Case ("Tuple_exp",
      map_tuple_expression env x
    )
  | `Simple_name x -> R.Case ("Simple_name",
      map_simple_name env x
    )
  | `Elem_access_exp (v1, v2) -> R.Case ("Elem_access_exp",
      let v1 = map_expression env v1 in
      let v2 = map_element_binding_expression env v2 in
      R.Tuple [v1; v2]
    )
  | `Elem_bind_exp x -> R.Case ("Elem_bind_exp",
      map_element_binding_expression env x
    )
  | `Poin_indi_exp (v1, v2) -> R.Case ("Poin_indi_exp",
      let v1 = (* "*" *) token env v1 in
      let v2 = map_expression env v2 in
      R.Tuple [v1; v2]
    )
  | `Paren_lvalue_exp (v1, v2, v3) -> R.Case ("Paren_lvalue_exp",
      let v1 = (* "(" *) token env v1 in
      let v2 = map_lvalue_expression env v2 in
      let v3 = (* ")" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_member_access_ellipsis_expression (env : env) ((v1, v2, v3) : CST.member_access_ellipsis_expression) =
  let v1 = map_anon_choice_exp_3bb8381 env v1 in
  let v2 = map_anon_choice_DOT_2ad1dab env v2 in
  let v3 = (* "..." *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_member_access_expression (env : env) ((v1, v2, v3) : CST.member_access_expression) =
  let v1 = map_anon_choice_exp_3bb8381 env v1 in
  let v2 = map_anon_choice_DOT_2ad1dab env v2 in
  let v3 = map_simple_name env v3 in
  R.Tuple [v1; v2; v3]

and map_member_binding_expression (env : env) ((v1, v2) : CST.member_binding_expression) =
  let v1 = (* "." *) token env v1 in
  let v2 = map_simple_name env v2 in
  R.Tuple [v1; v2]

and map_name (env : env) (x : CST.name) =
  (match x with
  | `Alias_qual_name (v1, v2, v3) -> R.Case ("Alias_qual_name",
      let v1 = map_identifier_or_global env v1 in
      let v2 = (* "::" *) token env v2 in
      let v3 = map_simple_name env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Qual_name (v1, v2, v3) -> R.Case ("Qual_name",
      let v1 = map_type_name env v1 in
      let v2 = (* "." *) token env v2 in
      let v3 = map_simple_name env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Simple_name x -> R.Case ("Simple_name",
      map_simple_name env x
    )
  )

and map_non_lvalue_expression (env : env) (x : CST.non_lvalue_expression) =
  (match x with
  | `Anon_meth_exp (v1, v2, v3, v4) -> R.Case ("Anon_meth_exp",
      let v1 =
        (match v1 with
        | Some x -> R.Option (Some (
            map_anon_choice_async_25087f5 env x
          ))
        | None -> R.Option None)
      in
      let v2 = (* "delegate" *) token env v2 in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_parameter_list env x
          ))
        | None -> R.Option None)
      in
      let v4 = map_block env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Anon_obj_crea_exp (v1, v2, v3, v4, v5) -> R.Case ("Anon_obj_crea_exp",
      let v1 = (* "new" *) token env v1 in
      let v2 = (* "{" *) token env v2 in
      let v3 =
        (match v3 with
        | Some (v1, v2) -> R.Option (Some (
            let v1 = map_anonymous_object_member_declarator env v1 in
            let v2 =
              R.List (List.map (fun (v1, v2) ->
                let v1 = (* "," *) token env v1 in
                let v2 = map_anonymous_object_member_declarator env v2 in
                R.Tuple [v1; v2]
              ) v2)
            in
            R.Tuple [v1; v2]
          ))
        | None -> R.Option None)
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
    )
  | `Array_crea_exp (v1, v2, v3) -> R.Case ("Array_crea_exp",
      let v1 = (* "new" *) token env v1 in
      let v2 = map_array_type env v2 in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_initializer_expression env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3]
    )
  | `As_exp (v1, v2, v3) -> R.Case ("As_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "as" *) token env v2 in
      let v3 = map_type_pattern env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Base_exp tok -> R.Case ("Base_exp",
      (* "base" *) token env tok
    )
  | `Bin_exp x -> R.Case ("Bin_exp",
      map_binary_expression env x
    )
  | `Cast_exp x -> R.Case ("Cast_exp",
      map_cast_expression env x
    )
  | `Chec_exp x -> R.Case ("Chec_exp",
      map_checked_expression env x
    )
  | `Cond_access_exp (v1, v2, v3) -> R.Case ("Cond_access_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "?" *) token env v2 in
      let v3 =
        (match v3 with
        | `Member_bind_exp x -> R.Case ("Member_bind_exp",
            map_member_binding_expression env x
          )
        | `Elem_bind_exp x -> R.Case ("Elem_bind_exp",
            map_element_binding_expression env x
          )
        )
      in
      R.Tuple [v1; v2; v3]
    )
  | `Cond_exp (v1, v2, v3, v4, v5) -> R.Case ("Cond_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "?" *) token env v2 in
      let v3 = map_expression env v3 in
      let v4 = (* ":" *) token env v4 in
      let v5 = map_expression env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `Defa_exp x -> R.Case ("Defa_exp",
      map_default_expression env x
    )
  | `Impl_array_crea_exp (v1, v2, v3, v4, v5) -> R.Case ("Impl_array_crea_exp",
      let v1 = (* "new" *) token env v1 in
      let v2 = (* "[" *) token env v2 in
      let v3 = R.List (List.map (token env (* "," *)) v3) in
      let v4 = (* "]" *) token env v4 in
      let v5 = map_initializer_expression env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `Impl_obj_crea_exp (v1, v2, v3) -> R.Case ("Impl_obj_crea_exp",
      let v1 = (* "new" *) token env v1 in
      let v2 = map_argument_list env v2 in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_initializer_expression env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3]
    )
  | `Impl_stack_alloc_array_crea_exp (v1, v2, v3, v4) -> R.Case ("Impl_stack_alloc_array_crea_exp",
      let v1 = (* "stackalloc" *) token env v1 in
      let v2 = (* "[" *) token env v2 in
      let v3 = (* "]" *) token env v3 in
      let v4 = map_initializer_expression env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Init_exp x -> R.Case ("Init_exp",
      map_initializer_expression env x
    )
  | `Inte_str_exp x -> R.Case ("Inte_str_exp",
      map_interpolated_string_expression env x
    )
  | `Is_exp (v1, v2, v3) -> R.Case ("Is_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "is" *) token env v2 in
      let v3 = map_type_pattern env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Is_pat_exp (v1, v2, v3) -> R.Case ("Is_pat_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "is" *) token env v2 in
      let v3 = map_pattern env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Lambda_exp (v1, v2, v3, v4, v5, v6) -> R.Case ("Lambda_exp",
      let v1 = R.List (List.map (map_attribute_list env) v1) in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_anon_choice_async_25087f5 env x
          ))
        | None -> R.Option None)
      in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_type_pattern env x
          ))
        | None -> R.Option None)
      in
      let v4 =
        (match v4 with
        | `Param_list x -> R.Case ("Param_list",
            map_parameter_list env x
          )
        | `Impl_param_list x -> R.Case ("Impl_param_list",
            map_implicit_parameter_list env x
          )
        )
      in
      let v5 = (* "=>" *) token env v5 in
      let v6 =
        (match v6 with
        | `Blk x -> R.Case ("Blk",
            map_block env x
          )
        | `Exp x -> R.Case ("Exp",
            map_expression env x
          )
        )
      in
      R.Tuple [v1; v2; v3; v4; v5; v6]
    )
  | `Make_ref_exp (v1, v2, v3, v4) -> R.Case ("Make_ref_exp",
      let v1 = (* "__makeref" *) token env v1 in
      let v2 = (* "(" *) token env v2 in
      let v3 = map_expression env v3 in
      let v4 = (* ")" *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Query_exp (v1, v2) -> R.Case ("Query_exp",
      let v1 = map_from_clause env v1 in
      let v2 = map_query_body env v2 in
      R.Tuple [v1; v2]
    )
  | `Range_exp (v1, v2, v3) -> R.Case ("Range_exp",
      let v1 =
        (match v1 with
        | Some x -> R.Option (Some (
            map_expression env x
          ))
        | None -> R.Option None)
      in
      let v2 = (* ".." *) token env v2 in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_expression env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3]
    )
  | `Ref_exp (v1, v2) -> R.Case ("Ref_exp",
      let v1 = (* "ref" *) token env v1 in
      let v2 = map_expression env v2 in
      R.Tuple [v1; v2]
    )
  | `Ref_type_exp (v1, v2, v3, v4) -> R.Case ("Ref_type_exp",
      let v1 = (* "__reftype" *) token env v1 in
      let v2 = (* "(" *) token env v2 in
      let v3 = map_expression env v3 in
      let v4 = (* ")" *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Ref_value_exp (v1, v2, v3, v4, v5, v6) -> R.Case ("Ref_value_exp",
      let v1 = (* "__refvalue" *) token env v1 in
      let v2 = (* "(" *) token env v2 in
      let v3 = map_expression env v3 in
      let v4 = (* "," *) token env v4 in
      let v5 = map_type_pattern env v5 in
      let v6 = (* ")" *) token env v6 in
      R.Tuple [v1; v2; v3; v4; v5; v6]
    )
  | `Size_of_exp x -> R.Case ("Size_of_exp",
      map_size_of_expression env x
    )
  | `Stack_alloc_array_crea_exp (v1, v2, v3) -> R.Case ("Stack_alloc_array_crea_exp",
      let v1 = (* "stackalloc" *) token env v1 in
      let v2 = map_array_type env v2 in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_initializer_expression env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3]
    )
  | `Switch_exp (v1, v2, v3, v4, v5, v6) -> R.Case ("Switch_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "switch" *) token env v2 in
      let v3 = (* "{" *) token env v3 in
      let v4 =
        (match v4 with
        | Some (v1, v2) -> R.Option (Some (
            let v1 = map_switch_expression_arm env v1 in
            let v2 =
              R.List (List.map (fun (v1, v2) ->
                let v1 = (* "," *) token env v1 in
                let v2 = map_switch_expression_arm env v2 in
                R.Tuple [v1; v2]
              ) v2)
            in
            R.Tuple [v1; v2]
          ))
        | None -> R.Option None)
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
  | `Throw_exp (v1, v2) -> R.Case ("Throw_exp",
      let v1 = (* "throw" *) token env v1 in
      let v2 = map_expression env v2 in
      R.Tuple [v1; v2]
    )
  | `Type_of_exp x -> R.Case ("Type_of_exp",
      map_type_of_expression env x
    )
  | `With_exp (v1, v2, v3, v4, v5) -> R.Case ("With_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "with" *) token env v2 in
      let v3 = (* "{" *) token env v3 in
      let v4 =
        (match v4 with
        | Some x -> R.Option (Some (
            map_with_initializer_expression env x
          ))
        | None -> R.Option None)
      in
      let v5 = (* "}" *) token env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `Lit x -> R.Case ("Lit",
      map_literal env x
    )
  | `Exp_stmt_exp x -> R.Case ("Exp_stmt_exp",
      map_expression_statement_expression env x
    )
  )

and map_nullable_base_type (env : env) (x : CST.nullable_base_type) =
  (match x with
  | `Array_type x -> R.Case ("Array_type",
      map_array_type env x
    )
  | `Type_name x -> R.Case ("Type_name",
      map_type_name env x
    )
  | `Pred_type tok -> R.Case ("Pred_type",
      (* predefined_type *) token env tok
    )
  | `Tuple_type x -> R.Case ("Tuple_type",
      map_tuple_type env x
    )
  )

and map_nullable_type (env : env) ((v1, v2) : CST.nullable_type) =
  let v1 = map_nullable_base_type env v1 in
  let v2 = (* "?" *) token env v2 in
  R.Tuple [v1; v2]

and map_object_creation_type (env : env) (x : CST.object_creation_type) =
  (match x with
  | `Type_name x -> R.Case ("Type_name",
      map_type_name env x
    )
  | `Null_type x -> R.Case ("Null_type",
      map_nullable_type env x
    )
  | `Pred_type tok -> R.Case ("Pred_type",
      (* predefined_type *) token env tok
    )
  )

and map_ordering (env : env) ((v1, v2) : CST.ordering) =
  let v1 = map_expression env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        (match x with
        | `Asce tok -> R.Case ("Asce",
            (* "ascending" *) token env tok
          )
        | `Desc tok -> R.Case ("Desc",
            (* "descending" *) token env tok
          )
        )
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2]

and map_parameter (env : env) (x : CST.parameter) =
  (match x with
  | `Rep_attr_list_opt_param_type_with_modifs_id_opt_equals_value_clause (v1, v2, v3, v4) -> R.Case ("Rep_attr_list_opt_param_type_with_modifs_id_opt_equals_value_clause",
      let v1 = R.List (List.map (map_attribute_list env) v1) in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_parameter_type_with_modifiers env x
          ))
        | None -> R.Option None)
      in
      let v3 = map_implicit_parameter_list env v3 in
      let v4 =
        (match v4 with
        | Some x -> R.Option (Some (
            map_equals_value_clause env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Ellips tok -> R.Case ("Ellips",
      (* "..." *) token env tok
    )
  )

and map_parameter_list (env : env) ((v1, v2, v3) : CST.parameter_list) =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_formal_parameter_list env x
      ))
    | None -> R.Option None)
  in
  let v3 = (* ")" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_parameter_type_with_modifiers (env : env) ((v1, v2, v3, v4) : CST.parameter_type_with_modifiers) =
  let v1 =
    (match v1 with
    | Some tok -> R.Option (Some (
        (* "this" *) token env tok
      ))
    | None -> R.Option None)
  in
  let v2 =
    (match v2 with
    | Some tok -> R.Option (Some (
        (* "scoped" *) token env tok
      ))
    | None -> R.Option None)
  in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_anon_choice_ref_eec35e8 env x
      ))
    | None -> R.Option None)
  in
  let v4 = map_ref_base_type env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_parenthesized_expression (env : env) ((v1, v2, v3) : CST.parenthesized_expression) =
  let v1 = (* "(" *) token env v1 in
  let v2 = map_non_lvalue_expression env v2 in
  let v3 = (* ")" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_pattern (env : env) (x : CST.pattern) =
  (match x with
  | `Cst_pat x -> R.Case ("Cst_pat",
      map_constant_pattern env x
    )
  | `Decl_pat (v1, v2) -> R.Case ("Decl_pat",
      let v1 = map_type_pattern env v1 in
      let v2 = map_variable_designation env v2 in
      R.Tuple [v1; v2]
    )
  | `Disc tok -> R.Case ("Disc",
      (* "_" *) token env tok
    )
  | `Recu_pat (v1, v2, v3) -> R.Case ("Recu_pat",
      let v1 =
        (match v1 with
        | Some x -> R.Option (Some (
            map_type_pattern env x
          ))
        | None -> R.Option None)
      in
      let v2 =
        (match v2 with
        | `Posi_pat_clause_opt_prop_pat_clause (v1, v2) -> R.Case ("Posi_pat_clause_opt_prop_pat_clause",
            let v1 = map_positional_pattern_clause env v1 in
            let v2 =
              (match v2 with
              | Some x -> R.Option (Some (
                  map_property_pattern_clause env x
                ))
              | None -> R.Option None)
            in
            R.Tuple [v1; v2]
          )
        | `Prop_pat_clause x -> R.Case ("Prop_pat_clause",
            map_property_pattern_clause env x
          )
        )
      in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_variable_designation env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3]
    )
  | `Var_pat (v1, v2) -> R.Case ("Var_pat",
      let v1 = (* "var" *) token env v1 in
      let v2 = map_variable_designation env v2 in
      R.Tuple [v1; v2]
    )
  | `Nega_pat (v1, v2) -> R.Case ("Nega_pat",
      let v1 = (* "not" *) token env v1 in
      let v2 = map_pattern env v2 in
      R.Tuple [v1; v2]
    )
  | `Paren_pat (v1, v2, v3) -> R.Case ("Paren_pat",
      let v1 = (* "(" *) token env v1 in
      let v2 = map_pattern env v2 in
      let v3 = (* ")" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Rela_pat x -> R.Case ("Rela_pat",
      map_relational_pattern env x
    )
  | `Or_pat (v1, v2, v3) -> R.Case ("Or_pat",
      let v1 = map_pattern env v1 in
      let v2 = (* "or" *) token env v2 in
      let v3 = map_pattern env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `And_pat (v1, v2, v3) -> R.Case ("And_pat",
      let v1 = map_pattern env v1 in
      let v2 = (* "and" *) token env v2 in
      let v3 = map_pattern env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `List_pat (v1, v2, v3) -> R.Case ("List_pat",
      let v1 = (* "[" *) token env v1 in
      let v2 =
        (match v2 with
        | Some (v1, v2, v3) -> R.Option (Some (
            let v1 = map_anon_choice_pat_29be9ad env v1 in
            let v2 =
              R.List (List.map (fun (v1, v2) ->
                let v1 = (* "," *) token env v1 in
                let v2 = map_anon_choice_pat_29be9ad env v2 in
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
  | `Type_pat x -> R.Case ("Type_pat",
      map_type_pattern env x
    )
  )

and map_pointer_base_type (env : env) (x : CST.pointer_base_type) =
  (match x with
  | `Type_name x -> R.Case ("Type_name",
      map_type_name env x
    )
  | `Null_type x -> R.Case ("Null_type",
      map_nullable_type env x
    )
  | `Poin_type x -> R.Case ("Poin_type",
      map_pointer_type env x
    )
  | `Func_poin_type x -> R.Case ("Func_poin_type",
      map_function_pointer_type env x
    )
  | `Pred_type tok -> R.Case ("Pred_type",
      (* predefined_type *) token env tok
    )
  | `Tuple_type x -> R.Case ("Tuple_type",
      map_tuple_type env x
    )
  )

and map_pointer_type (env : env) ((v1, v2) : CST.pointer_type) =
  let v1 = map_pointer_base_type env v1 in
  let v2 = (* "*" *) token env v2 in
  R.Tuple [v1; v2]

and map_positional_pattern_clause (env : env) ((v1, v2, v3) : CST.positional_pattern_clause) =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2, v3, v4) -> R.Option (Some (
        let v1 = map_subpattern env v1 in
        let v2 = (* "," *) token env v2 in
        let v3 = map_subpattern env v3 in
        let v4 =
          R.List (List.map (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_subpattern env v2 in
            R.Tuple [v1; v2]
          ) v4)
        in
        R.Tuple [v1; v2; v3; v4]
      ))
    | None -> R.Option None)
  in
  let v3 = (* ")" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_postfix_unary_expression (env : env) (x : CST.postfix_unary_expression) =
  (match x with
  | `Exp_PLUSPLUS (v1, v2) -> R.Case ("Exp_PLUSPLUS",
      let v1 = map_expression env v1 in
      let v2 = (* "++" *) token env v2 in
      R.Tuple [v1; v2]
    )
  | `Exp_DASHDASH (v1, v2) -> R.Case ("Exp_DASHDASH",
      let v1 = map_expression env v1 in
      let v2 = (* "--" *) token env v2 in
      R.Tuple [v1; v2]
    )
  | `Exp_BANG (v1, v2) -> R.Case ("Exp_BANG",
      let v1 = map_expression env v1 in
      let v2 = (* "!" *) token env v2 in
      R.Tuple [v1; v2]
    )
  )

and map_prefix_unary_expression (env : env) (x : CST.prefix_unary_expression) =
  (match x with
  | `BANG_exp (v1, v2) -> R.Case ("BANG_exp",
      let v1 = (* "!" *) token env v1 in
      let v2 = map_expression env v2 in
      R.Tuple [v1; v2]
    )
  | `AMP_exp (v1, v2) -> R.Case ("AMP_exp",
      let v1 = (* "&" *) token env v1 in
      let v2 = map_expression env v2 in
      R.Tuple [v1; v2]
    )
  | `PLUS_exp (v1, v2) -> R.Case ("PLUS_exp",
      let v1 = (* "+" *) token env v1 in
      let v2 = map_expression env v2 in
      R.Tuple [v1; v2]
    )
  | `PLUSPLUS_exp (v1, v2) -> R.Case ("PLUSPLUS_exp",
      let v1 = (* "++" *) token env v1 in
      let v2 = map_expression env v2 in
      R.Tuple [v1; v2]
    )
  | `DASH_exp (v1, v2) -> R.Case ("DASH_exp",
      let v1 = (* "-" *) token env v1 in
      let v2 = map_expression env v2 in
      R.Tuple [v1; v2]
    )
  | `DASHDASH_exp (v1, v2) -> R.Case ("DASHDASH_exp",
      let v1 = (* "--" *) token env v1 in
      let v2 = map_expression env v2 in
      R.Tuple [v1; v2]
    )
  | `HAT_exp (v1, v2) -> R.Case ("HAT_exp",
      let v1 = (* "^" *) token env v1 in
      let v2 = map_expression env v2 in
      R.Tuple [v1; v2]
    )
  | `TILDE_exp (v1, v2) -> R.Case ("TILDE_exp",
      let v1 = (* "~" *) token env v1 in
      let v2 = map_expression env v2 in
      R.Tuple [v1; v2]
    )
  )

and map_property_pattern_clause (env : env) ((v1, v2, v3, v4) : CST.property_pattern_clause) =
  let v1 = (* "{" *) token env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = map_subpattern env v1 in
        let v2 =
          R.List (List.map (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_subpattern env v2 in
            R.Tuple [v1; v2]
          ) v2)
        in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  let v3 =
    (match v3 with
    | Some tok -> R.Option (Some (
        (* "," *) token env tok
      ))
    | None -> R.Option None)
  in
  let v4 = (* "}" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_query_body (env : env) (x : CST.query_body) =
  (match x with
  | `Rectype (v1, v2, v3) -> R.Case ("Rectype",
      let v1 = R.List (List.map (map_query_clause env) v1) in
      let v2 = map_select_or_group_clause env v2 in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_query_continuation env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3]
    )
  )

and map_query_clause (env : env) (x : CST.query_clause) =
  (match x with
  | `From_clause x -> R.Case ("From_clause",
      map_from_clause env x
    )
  | `Join_clause (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10) -> R.Case ("Join_clause",
      let v1 = (* "join" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_type_pattern env x
          ))
        | None -> R.Option None)
      in
      let v3 = map_implicit_parameter_list env v3 in
      let v4 = (* "in" *) token env v4 in
      let v5 = map_expression env v5 in
      let v6 = (* "on" *) token env v6 in
      let v7 = map_expression env v7 in
      let v8 = (* "equals" *) token env v8 in
      let v9 = map_expression env v9 in
      let v10 =
        (match v10 with
        | Some x -> R.Option (Some (
            map_join_into_clause env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3; v4; v5; v6; v7; v8; v9; v10]
    )
  | `Let_clause (v1, v2, v3, v4) -> R.Case ("Let_clause",
      let v1 = (* "let" *) token env v1 in
      let v2 = map_implicit_parameter_list env v2 in
      let v3 = (* "=" *) token env v3 in
      let v4 = map_expression env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Order_by_clause (v1, v2, v3) -> R.Case ("Order_by_clause",
      let v1 = (* "orderby" *) token env v1 in
      let v2 = map_ordering env v2 in
      let v3 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = (* "," *) token env v1 in
          let v2 = map_ordering env v2 in
          R.Tuple [v1; v2]
        ) v3)
      in
      R.Tuple [v1; v2; v3]
    )
  | `Where_clause (v1, v2) -> R.Case ("Where_clause",
      let v1 = (* "where" *) token env v1 in
      let v2 = map_expression env v2 in
      R.Tuple [v1; v2]
    )
  )

and map_query_continuation (env : env) (x : CST.query_continuation) =
  (match x with
  | `Rectype (v1, v2, v3) -> R.Case ("Rectype",
      let v1 = (* "into" *) token env v1 in
      let v2 = map_implicit_parameter_list env v2 in
      let v3 = map_query_body env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_ref_base_type (env : env) (x : CST.ref_base_type) =
  (match x with
  | `Impl_type tok -> R.Case ("Impl_type",
      (* "var" *) token env tok
    )
  | `Array_type x -> R.Case ("Array_type",
      map_array_type env x
    )
  | `Type_name x -> R.Case ("Type_name",
      map_type_name env x
    )
  | `Null_type x -> R.Case ("Null_type",
      map_nullable_type env x
    )
  | `Poin_type x -> R.Case ("Poin_type",
      map_pointer_type env x
    )
  | `Func_poin_type x -> R.Case ("Func_poin_type",
      map_function_pointer_type env x
    )
  | `Pred_type tok -> R.Case ("Pred_type",
      (* predefined_type *) token env tok
    )
  | `Tuple_type x -> R.Case ("Tuple_type",
      map_tuple_type env x
    )
  )

and map_ref_type (env : env) ((v1, v2, v3) : CST.ref_type) =
  let v1 = (* "ref" *) token env v1 in
  let v2 =
    (match v2 with
    | Some tok -> R.Option (Some (
        (* "readonly" *) token env tok
      ))
    | None -> R.Option None)
  in
  let v3 = map_ref_base_type env v3 in
  R.Tuple [v1; v2; v3]

and map_relational_pattern (env : env) (x : CST.relational_pattern) =
  (match x with
  | `LT_exp (v1, v2) -> R.Case ("LT_exp",
      let v1 = (* "<" *) token env v1 in
      let v2 = map_expression env v2 in
      R.Tuple [v1; v2]
    )
  | `LTEQ_exp (v1, v2) -> R.Case ("LTEQ_exp",
      let v1 = (* "<=" *) token env v1 in
      let v2 = map_expression env v2 in
      R.Tuple [v1; v2]
    )
  | `GT_exp (v1, v2) -> R.Case ("GT_exp",
      let v1 = (* ">" *) token env v1 in
      let v2 = map_expression env v2 in
      R.Tuple [v1; v2]
    )
  | `GTEQ_exp (v1, v2) -> R.Case ("GTEQ_exp",
      let v1 = (* ">=" *) token env v1 in
      let v2 = map_expression env v2 in
      R.Tuple [v1; v2]
    )
  )

and map_scoped_base_type (env : env) (x : CST.scoped_base_type) =
  (match x with
  | `Type_name x -> R.Case ("Type_name",
      map_type_name env x
    )
  | `Ref_type x -> R.Case ("Ref_type",
      map_ref_type env x
    )
  )

and map_select_or_group_clause (env : env) (x : CST.select_or_group_clause) =
  (match x with
  | `Group_clause (v1, v2, v3, v4) -> R.Case ("Group_clause",
      let v1 = (* "group" *) token env v1 in
      let v2 = map_expression env v2 in
      let v3 = (* "by" *) token env v3 in
      let v4 = map_expression env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Select_clause (v1, v2) -> R.Case ("Select_clause",
      let v1 = (* "select" *) token env v1 in
      let v2 = map_expression env v2 in
      R.Tuple [v1; v2]
    )
  )

and map_simple_assignment_expression (env : env) ((v1, v2, v3) : CST.simple_assignment_expression) =
  let v1 = map_implicit_parameter_list env v1 in
  let v2 = (* "=" *) token env v2 in
  let v3 = map_expression env v3 in
  R.Tuple [v1; v2; v3]

and map_simple_name (env : env) (x : CST.simple_name) =
  (match x with
  | `Gene_name (v1, v2) -> R.Case ("Gene_name",
      let v1 = map_implicit_parameter_list env v1 in
      let v2 = map_type_argument_list env v2 in
      R.Tuple [v1; v2]
    )
  | `Choice_global x -> R.Case ("Choice_global",
      map_identifier_or_global env x
    )
  )

and map_size_of_expression (env : env) ((v1, v2, v3, v4) : CST.size_of_expression) =
  let v1 = (* "sizeof" *) token env v1 in
  let v2 = (* "(" *) token env v2 in
  let v3 = map_type_pattern env v3 in
  let v4 = (* ")" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_statement (env : env) (x : CST.statement) =
  (match x with
  | `Blk x -> R.Case ("Blk",
      map_block env x
    )
  | `Brk_stmt (v1, v2) -> R.Case ("Brk_stmt",
      let v1 = (* "break" *) token env v1 in
      let v2 = (* ";" *) token env v2 in
      R.Tuple [v1; v2]
    )
  | `Chec_stmt (v1, v2) -> R.Case ("Chec_stmt",
      let v1 =
        (match v1 with
        | `Chec tok -> R.Case ("Chec",
            (* "checked" *) token env tok
          )
        | `Unch tok -> R.Case ("Unch",
            (* "unchecked" *) token env tok
          )
        )
      in
      let v2 = map_block env v2 in
      R.Tuple [v1; v2]
    )
  | `Cont_stmt (v1, v2) -> R.Case ("Cont_stmt",
      let v1 = (* "continue" *) token env v1 in
      let v2 = (* ";" *) token env v2 in
      R.Tuple [v1; v2]
    )
  | `Do_stmt (v1, v2, v3, v4, v5, v6, v7) -> R.Case ("Do_stmt",
      let v1 = (* "do" *) token env v1 in
      let v2 = map_global_statement env v2 in
      let v3 = (* "while" *) token env v3 in
      let v4 = (* "(" *) token env v4 in
      let v5 = map_expression env v5 in
      let v6 = (* ")" *) token env v6 in
      let v7 = (* ";" *) token env v7 in
      R.Tuple [v1; v2; v3; v4; v5; v6; v7]
    )
  | `Empty_stmt tok -> R.Case ("Empty_stmt",
      (* ";" *) token env tok
    )
  | `Exp_stmt x -> R.Case ("Exp_stmt",
      map_expression_statement env x
    )
  | `Fixed_stmt (v1, v2, v3, v4, v5) -> R.Case ("Fixed_stmt",
      let v1 = (* "fixed" *) token env v1 in
      let v2 = (* "(" *) token env v2 in
      let v3 = map_variable_declaration env v3 in
      let v4 = (* ")" *) token env v4 in
      let v5 = map_global_statement env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `For_each_stmt (v1, v2, v3, v4, v5, v6, v7, v8) -> R.Case ("For_each_stmt",
      let v1 =
        (match v1 with
        | Some tok -> R.Option (Some (
            (* "await" *) token env tok
          ))
        | None -> R.Option None)
      in
      let v2 = (* "foreach" *) token env v2 in
      let v3 = (* "(" *) token env v3 in
      let v4 =
        (match v4 with
        | `Type_choice_id (v1, v2) -> R.Case ("Type_choice_id",
            let v1 = map_type_pattern env v1 in
            let v2 = map_anon_choice_impl_param_bf14316 env v2 in
            R.Tuple [v1; v2]
          )
        | `Exp x -> R.Case ("Exp",
            map_expression env x
          )
        )
      in
      let v5 = (* "in" *) token env v5 in
      let v6 = map_expression env v6 in
      let v7 = (* ")" *) token env v7 in
      let v8 = map_global_statement env v8 in
      R.Tuple [v1; v2; v3; v4; v5; v6; v7; v8]
    )
  | `For_stmt (v1, v2, v3, v4, v5, v6, v7, v8, v9) -> R.Case ("For_stmt",
      let v1 = (* "for" *) token env v1 in
      let v2 = (* "(" *) token env v2 in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            (match x with
            | `Var_decl x -> R.Case ("Var_decl",
                map_variable_declaration env x
              )
            | `Exp_rep_COMMA_exp (v1, v2) -> R.Case ("Exp_rep_COMMA_exp",
                let v1 = map_expression env v1 in
                let v2 =
                  R.List (List.map (map_interpolation_alignment_clause env) v2)
                in
                R.Tuple [v1; v2]
              )
            )
          ))
        | None -> R.Option None)
      in
      let v4 = (* ";" *) token env v4 in
      let v5 =
        (match v5 with
        | Some x -> R.Option (Some (
            map_expression env x
          ))
        | None -> R.Option None)
      in
      let v6 = (* ";" *) token env v6 in
      let v7 =
        map_anon_opt_exp_rep_interp_alig_clause_cd88eaa env v7
      in
      let v8 = (* ")" *) token env v8 in
      let v9 = map_global_statement env v9 in
      R.Tuple [v1; v2; v3; v4; v5; v6; v7; v8; v9]
    )
  | `Goto_stmt (v1, v2, v3, v4) -> R.Case ("Goto_stmt",
      let v1 = (* "goto" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            (match x with
            | `Case tok -> R.Case ("Case",
                (* "case" *) token env tok
              )
            | `Defa tok -> R.Case ("Defa",
                (* "default" *) token env tok
              )
            )
          ))
        | None -> R.Option None)
      in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_expression env x
          ))
        | None -> R.Option None)
      in
      let v4 = (* ";" *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `If_stmt (v1, v2, v3, v4, v5, v6) -> R.Case ("If_stmt",
      let v1 = (* "if" *) token env v1 in
      let v2 = (* "(" *) token env v2 in
      let v3 = map_expression env v3 in
      let v4 = (* ")" *) token env v4 in
      let v5 = map_global_statement env v5 in
      let v6 =
        (match v6 with
        | Some (v1, v2) -> R.Option (Some (
            let v1 = (* "else" *) token env v1 in
            let v2 = map_global_statement env v2 in
            R.Tuple [v1; v2]
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3; v4; v5; v6]
    )
  | `Labe_stmt (v1, v2, v3) -> R.Case ("Labe_stmt",
      let v1 = map_implicit_parameter_list env v1 in
      let v2 = (* ":" *) token env v2 in
      let v3 = map_global_statement env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Local_decl_stmt (v1, v2, v3, v4, v5) -> R.Case ("Local_decl_stmt",
      let v1 =
        (match v1 with
        | Some tok -> R.Option (Some (
            (* "await" *) token env tok
          ))
        | None -> R.Option None)
      in
      let v2 =
        (match v2 with
        | Some tok -> R.Option (Some (
            (* "using" *) token env tok
          ))
        | None -> R.Option None)
      in
      let v3 = R.List (List.map (map_modifier env) v3) in
      let v4 = map_variable_declaration env v4 in
      let v5 = (* ";" *) token env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `Local_func_stmt (v1, v2, v3, v4, v5, v6, v7, v8) -> R.Case ("Local_func_stmt",
      let v1 = R.List (List.map (map_attribute_list env) v1) in
      let v2 = R.List (List.map (map_modifier env) v2) in
      let v3 = map_type_pattern env v3 in
      let v4 = map_implicit_parameter_list env v4 in
      let v5 =
        (match v5 with
        | Some x -> R.Option (Some (
            map_type_parameter_list env x
          ))
        | None -> R.Option None)
      in
      let v6 = map_parameter_list env v6 in
      let v7 =
        R.List (List.map (map_type_parameter_constraints_clause env) v7)
      in
      let v8 = map_function_body env v8 in
      R.Tuple [v1; v2; v3; v4; v5; v6; v7; v8]
    )
  | `Lock_stmt (v1, v2, v3, v4, v5) -> R.Case ("Lock_stmt",
      let v1 = (* "lock" *) token env v1 in
      let v2 = (* "(" *) token env v2 in
      let v3 = map_expression env v3 in
      let v4 = (* ")" *) token env v4 in
      let v5 = map_global_statement env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `Ret_stmt (v1, v2, v3) -> R.Case ("Ret_stmt",
      let v1 = (* "return" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_expression env x
          ))
        | None -> R.Option None)
      in
      let v3 = (* ";" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Switch_stmt (v1, v2, v3) -> R.Case ("Switch_stmt",
      let v1 = (* "switch" *) token env v1 in
      let v2 =
        (match v2 with
        | `LPAR_exp_RPAR (v1, v2, v3) -> R.Case ("LPAR_exp_RPAR",
            let v1 = (* "(" *) token env v1 in
            let v2 = map_expression env v2 in
            let v3 = (* ")" *) token env v3 in
            R.Tuple [v1; v2; v3]
          )
        | `Tuple_exp x -> R.Case ("Tuple_exp",
            map_tuple_expression env x
          )
        )
      in
      let v3 = map_switch_body env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Throw_stmt (v1, v2, v3) -> R.Case ("Throw_stmt",
      let v1 = (* "throw" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_expression env x
          ))
        | None -> R.Option None)
      in
      let v3 = (* ";" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Try_stmt (v1, v2, v3, v4) -> R.Case ("Try_stmt",
      let v1 = (* "try" *) token env v1 in
      let v2 = map_block env v2 in
      let v3 = R.List (List.map (map_catch_clause env) v3) in
      let v4 =
        (match v4 with
        | Some x -> R.Option (Some (
            map_finally_clause env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Unsafe_stmt (v1, v2) -> R.Case ("Unsafe_stmt",
      let v1 = (* "unsafe" *) token env v1 in
      let v2 = map_block env v2 in
      R.Tuple [v1; v2]
    )
  | `Using_stmt (v1, v2, v3, v4, v5, v6) -> R.Case ("Using_stmt",
      let v1 =
        (match v1 with
        | Some tok -> R.Option (Some (
            (* "await" *) token env tok
          ))
        | None -> R.Option None)
      in
      let v2 = (* "using" *) token env v2 in
      let v3 = (* "(" *) token env v3 in
      let v4 =
        (match v4 with
        | `Var_decl x -> R.Case ("Var_decl",
            map_variable_declaration env x
          )
        | `Exp x -> R.Case ("Exp",
            map_expression env x
          )
        )
      in
      let v5 = (* ")" *) token env v5 in
      let v6 = map_global_statement env v6 in
      R.Tuple [v1; v2; v3; v4; v5; v6]
    )
  | `While_stmt (v1, v2, v3, v4, v5) -> R.Case ("While_stmt",
      let v1 = (* "while" *) token env v1 in
      let v2 = (* "(" *) token env v2 in
      let v3 = map_expression env v3 in
      let v4 = (* ")" *) token env v4 in
      let v5 = map_global_statement env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `Yield_stmt (v1, v2, v3) -> R.Case ("Yield_stmt",
      let v1 = (* "yield" *) token env v1 in
      let v2 =
        (match v2 with
        | `Ret_exp (v1, v2) -> R.Case ("Ret_exp",
            let v1 = (* "return" *) token env v1 in
            let v2 = map_expression env v2 in
            R.Tuple [v1; v2]
          )
        | `Brk tok -> R.Case ("Brk",
            (* "break" *) token env tok
          )
        )
      in
      let v3 = (* ";" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_subpattern (env : env) ((v1, v2) : CST.subpattern) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        map_expression_colon env x
      ))
    | None -> R.Option None)
  in
  let v2 = map_pattern env v2 in
  R.Tuple [v1; v2]

and map_switch_body (env : env) ((v1, v2, v3) : CST.switch_body) =
  let v1 = (* "{" *) token env v1 in
  let v2 = R.List (List.map (map_switch_section env) v2) in
  let v3 = (* "}" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_switch_expression_arm (env : env) ((v1, v2, v3, v4) : CST.switch_expression_arm) =
  let v1 = map_pattern env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_when_clause env x
      ))
    | None -> R.Option None)
  in
  let v3 = (* "=>" *) token env v3 in
  let v4 = map_expression env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_switch_section (env : env) ((v1, v2) : CST.switch_section) =
  let v1 =
    R.List (List.map (fun x ->
      (match x with
      | `Case_switch_label x -> R.Case ("Case_switch_label",
          map_case_switch_label env x
        )
      | `Case_pat_switch_label x -> R.Case ("Case_pat_switch_label",
          map_case_pattern_switch_label env x
        )
      | `Defa_switch_label x -> R.Case ("Defa_switch_label",
          map_default_switch_label env x
        )
      )
    ) v1)
  in
  let v2 = R.List (List.map (map_global_statement env) v2) in
  R.Tuple [v1; v2]

and map_tuple_element (env : env) ((v1, v2) : CST.tuple_element) =
  let v1 = map_type_pattern env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_implicit_parameter_list env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2]

and map_tuple_expression (env : env) ((v1, v2, v3, v4, v5, v6) : CST.tuple_expression) =
  let v1 = (* "(" *) token env v1 in
  let v2 = map_argument env v2 in
  let v3 = (* "," *) token env v3 in
  let v4 = map_argument env v4 in
  let v5 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_argument env v2 in
      R.Tuple [v1; v2]
    ) v5)
  in
  let v6 = (* ")" *) token env v6 in
  R.Tuple [v1; v2; v3; v4; v5; v6]

and map_tuple_type (env : env) ((v1, v2, v3, v4, v5, v6) : CST.tuple_type) =
  let v1 = (* "(" *) token env v1 in
  let v2 = map_tuple_element env v2 in
  let v3 = (* "," *) token env v3 in
  let v4 = map_tuple_element env v4 in
  let v5 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_tuple_element env v2 in
      R.Tuple [v1; v2]
    ) v5)
  in
  let v6 = (* ")" *) token env v6 in
  R.Tuple [v1; v2; v3; v4; v5; v6]

and map_type_ (env : env) (x : CST.type_) =
  (match x with
  | `Impl_type tok -> R.Case ("Impl_type",
      (* "var" *) token env tok
    )
  | `Array_type x -> R.Case ("Array_type",
      map_array_type env x
    )
  | `Type_name x -> R.Case ("Type_name",
      map_type_name env x
    )
  | `Null_type x -> R.Case ("Null_type",
      map_nullable_type env x
    )
  | `Poin_type x -> R.Case ("Poin_type",
      map_pointer_type env x
    )
  | `Func_poin_type x -> R.Case ("Func_poin_type",
      map_function_pointer_type env x
    )
  | `Pred_type tok -> R.Case ("Pred_type",
      (* predefined_type *) token env tok
    )
  | `Tuple_type x -> R.Case ("Tuple_type",
      map_tuple_type env x
    )
  | `Ref_type x -> R.Case ("Ref_type",
      map_ref_type env x
    )
  | `Scoped_type (v1, v2) -> R.Case ("Scoped_type",
      let v1 = (* "scoped" *) token env v1 in
      let v2 = map_scoped_base_type env v2 in
      R.Tuple [v1; v2]
    )
  )

and map_type_argument_list (env : env) ((v1, v2, v3) : CST.type_argument_list) =
  let v1 = (* "<" *) token env v1 in
  let v2 =
    (match v2 with
    | `Rep_COMMA xs -> R.Case ("Rep_COMMA",
        R.List (List.map (token env (* "," *)) xs)
      )
    | `Type_rep_COMMA_type (v1, v2) -> R.Case ("Type_rep_COMMA_type",
        let v1 = map_type_pattern env v1 in
        let v2 =
          R.List (List.map (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_type_pattern env v2 in
            R.Tuple [v1; v2]
          ) v2)
        in
        R.Tuple [v1; v2]
      )
    )
  in
  let v3 = (* ">" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_type_constraint (env : env) (x : CST.type_constraint) =
  map_type_ env x

and map_type_name (env : env) (x : CST.type_name) =
  map_name env x

and map_type_of_expression (env : env) ((v1, v2, v3, v4) : CST.type_of_expression) =
  let v1 = (* "typeof" *) token env v1 in
  let v2 = (* "(" *) token env v2 in
  let v3 = map_type_pattern env v3 in
  let v4 = (* ")" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_type_parameter (env : env) ((v1, v2, v3) : CST.type_parameter) =
  let v1 = R.List (List.map (map_attribute_list env) v1) in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        (match x with
        | `In tok -> R.Case ("In",
            (* "in" *) token env tok
          )
        | `Out tok -> R.Case ("Out",
            (* "out" *) token env tok
          )
        )
      ))
    | None -> R.Option None)
  in
  let v3 = map_implicit_parameter_list env v3 in
  R.Tuple [v1; v2; v3]

and map_type_parameter_constraint (env : env) (x : CST.type_parameter_constraint) =
  (match x with
  | `Class_opt_QMARK (v1, v2) -> R.Case ("Class_opt_QMARK",
      let v1 = (* "class" *) token env v1 in
      let v2 =
        (match v2 with
        | Some tok -> R.Option (Some (
            (* "?" *) token env tok
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2]
    )
  | `Struct tok -> R.Case ("Struct",
      (* "struct" *) token env tok
    )
  | `Notn tok -> R.Case ("Notn",
      (* "notnull" *) token env tok
    )
  | `Unma tok -> R.Case ("Unma",
      (* "unmanaged" *) token env tok
    )
  | `Cons_cons (v1, v2, v3) -> R.Case ("Cons_cons",
      let v1 = (* "new" *) token env v1 in
      let v2 = (* "(" *) token env v2 in
      let v3 = (* ")" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Type_cons x -> R.Case ("Type_cons",
      map_type_constraint env x
    )
  )

and map_type_parameter_constraints_clause (env : env) ((v1, v2, v3, v4, v5) : CST.type_parameter_constraints_clause) =
  let v1 = (* "where" *) token env v1 in
  let v2 = map_identifier_or_global env v2 in
  let v3 = (* ":" *) token env v3 in
  let v4 = map_type_parameter_constraint env v4 in
  let v5 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_type_parameter_constraint env v2 in
      R.Tuple [v1; v2]
    ) v5)
  in
  R.Tuple [v1; v2; v3; v4; v5]

and map_type_parameter_list (env : env) ((v1, v2, v3, v4) : CST.type_parameter_list) =
  let v1 = (* "<" *) token env v1 in
  let v2 = map_type_parameter env v2 in
  let v3 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_type_parameter env v2 in
      R.Tuple [v1; v2]
    ) v3)
  in
  let v4 = (* ">" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_type_pattern (env : env) (x : CST.type_pattern) =
  map_type_ env x

and map_typed_metavariable (env : env) ((v1, v2, v3, v4) : CST.typed_metavariable) =
  let v1 = (* "(" *) token env v1 in
  let v2 = map_type_pattern env v2 in
  let v3 = (* semgrep_metavariable *) token env v3 in
  let v4 = (* ")" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_variable_declaration (env : env) ((v1, v2, v3) : CST.variable_declaration) =
  let v1 = map_type_pattern env v1 in
  let v2 = map_variable_declarator env v2 in
  let v3 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_variable_declarator env v2 in
      R.Tuple [v1; v2]
    ) v3)
  in
  R.Tuple [v1; v2; v3]

and map_variable_declarator (env : env) ((v1, v2, v3) : CST.variable_declarator) =
  let v1 = map_anon_choice_impl_param_bf14316 env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_element_binding_expression env x
      ))
    | None -> R.Option None)
  in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_equals_value_clause env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3]

and map_when_clause (env : env) ((v1, v2) : CST.when_clause) =
  let v1 = (* "when" *) token env v1 in
  let v2 = map_expression env v2 in
  R.Tuple [v1; v2]

and map_with_initializer_expression (env : env) ((v1, v2) : CST.with_initializer_expression) =
  let v1 = map_simple_assignment_expression env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_simple_assignment_expression env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  R.Tuple [v1; v2]

let map_bracketed_parameter_list (env : env) ((v1, v2, v3) : CST.bracketed_parameter_list) =
  let v1 = (* "[" *) token env v1 in
  let v2 = map_formal_parameter_list env v2 in
  let v3 = (* "]" *) token env v3 in
  R.Tuple [v1; v2; v3]

let map_base_list (env : env) ((v1, v2, v3) : CST.base_list) =
  let v1 = (* ":" *) token env v1 in
  let v2 = map_type_pattern env v2 in
  let v3 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_type_pattern env v2 in
      R.Tuple [v1; v2]
    ) v3)
  in
  R.Tuple [v1; v2; v3]

let map_primary_constructor_base_type (env : env) ((v1, v2) : CST.primary_constructor_base_type) =
  let v1 = map_type_name env v1 in
  let v2 = map_argument_list env v2 in
  R.Tuple [v1; v2]

let map_explicit_interface_specifier (env : env) ((v1, v2) : CST.explicit_interface_specifier) =
  let v1 = map_type_name env v1 in
  let v2 = (* "." *) token env v2 in
  R.Tuple [v1; v2]

let map_constructor_initializer (env : env) ((v1, v2, v3) : CST.constructor_initializer) =
  let v1 = (* ":" *) token env v1 in
  let v2 =
    (match v2 with
    | `Base tok -> R.Case ("Base",
        (* "base" *) token env tok
      )
    | `This tok -> R.Case ("This",
        (* "this" *) token env tok
      )
    )
  in
  let v3 = map_argument_list env v3 in
  R.Tuple [v1; v2; v3]

let map_global_attribute_list (env : env) ((v1, v2, v3, v4, v5) : CST.global_attribute_list) =
  let v1 = (* "[" *) token env v1 in
  let v2 =
    (match v2 with
    | `Asse tok -> R.Case ("Asse",
        (* "assembly" *) token env tok
      )
    | `Module tok -> R.Case ("Module",
        (* "module" *) token env tok
      )
    )
  in
  let v3 = (* ":" *) token env v3 in
  let v4 =
    (match v4 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = map_attribute env v1 in
        let v2 =
          R.List (List.map (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_attribute env v2 in
            R.Tuple [v1; v2]
          ) v2)
        in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  let v5 = (* "]" *) token env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

let map_using_directive (env : env) ((v1, v2, v3, v4, v5) : CST.using_directive) =
  let v1 =
    (match v1 with
    | Some tok -> R.Option (Some (
        (* "global" *) token env tok
      ))
    | None -> R.Option None)
  in
  let v2 = (* "using" *) token env v2 in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        (match x with
        | `Static tok -> R.Case ("Static",
            (* "static" *) token env tok
          )
        | `Name_equals x -> R.Case ("Name_equals",
            map_name_equals env x
          )
        )
      ))
    | None -> R.Option None)
  in
  let v4 = map_type_name env v4 in
  let v5 = (* ";" *) token env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

let map_accessor_declaration (env : env) ((v1, v2, v3, v4) : CST.accessor_declaration) =
  let v1 = R.List (List.map (map_attribute_list env) v1) in
  let v2 = R.List (List.map (map_modifier env) v2) in
  let v3 =
    (match v3 with
    | `Get tok -> R.Case ("Get",
        (* "get" *) token env tok
      )
    | `Set tok -> R.Case ("Set",
        (* "set" *) token env tok
      )
    | `Add tok -> R.Case ("Add",
        (* "add" *) token env tok
      )
    | `Remove tok -> R.Case ("Remove",
        (* "remove" *) token env tok
      )
    | `Init tok -> R.Case ("Init",
        (* "init" *) token env tok
      )
    | `Id x -> R.Case ("Id",
        map_implicit_parameter_list env x
      )
    )
  in
  let v4 = map_function_body env v4 in
  R.Tuple [v1; v2; v3; v4]

let map_enum_member_declaration (env : env) (x : CST.enum_member_declaration) =
  (match x with
  | `Rep_attr_list_id_opt_EQ_exp (v1, v2, v3) -> R.Case ("Rep_attr_list_id_opt_EQ_exp",
      let v1 = R.List (List.map (map_attribute_list env) v1) in
      let v2 = map_implicit_parameter_list env v2 in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_equals_value_clause env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3]
    )
  | `Ellips tok -> R.Case ("Ellips",
      (* "..." *) token env tok
    )
  )

let map_delegate_declaration (env : env) ((v1, v2, v3, v4, v5, v6, v7, v8, v9) : CST.delegate_declaration) =
  let v1 = R.List (List.map (map_attribute_list env) v1) in
  let v2 = R.List (List.map (map_modifier env) v2) in
  let v3 = (* "delegate" *) token env v3 in
  let v4 = map_type_pattern env v4 in
  let v5 = map_implicit_parameter_list env v5 in
  let v6 =
    (match v6 with
    | Some x -> R.Option (Some (
        map_type_parameter_list env x
      ))
    | None -> R.Option None)
  in
  let v7 = map_parameter_list env v7 in
  let v8 =
    R.List (List.map (map_type_parameter_constraints_clause env) v8)
  in
  let v9 = (* ";" *) token env v9 in
  R.Tuple [v1; v2; v3; v4; v5; v6; v7; v8; v9]

let map_record_base (env : env) (x : CST.record_base) =
  (match x with
  | `COLON_type_name_rep_COMMA_type_name (v1, v2, v3) -> R.Case ("COLON_type_name_rep_COMMA_type_name",
      let v1 = (* ":" *) token env v1 in
      let v2 = map_type_name env v2 in
      let v3 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = (* "," *) token env v1 in
          let v2 = map_type_name env v2 in
          R.Tuple [v1; v2]
        ) v3)
      in
      R.Tuple [v1; v2; v3]
    )
  | `COLON_prim_cons_base_type_opt_COMMA_type_name_rep_COMMA_type_name (v1, v2, v3) -> R.Case ("COLON_prim_cons_base_type_opt_COMMA_type_name_rep_COMMA_type_name",
      let v1 = (* ":" *) token env v1 in
      let v2 = map_primary_constructor_base_type env v2 in
      let v3 =
        (match v3 with
        | Some (v1, v2, v3) -> R.Option (Some (
            let v1 = (* "," *) token env v1 in
            let v2 = map_type_name env v2 in
            let v3 =
              R.List (List.map (fun (v1, v2) ->
                let v1 = (* "," *) token env v1 in
                let v2 = map_type_name env v2 in
                R.Tuple [v1; v2]
              ) v3)
            in
            R.Tuple [v1; v2; v3]
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3]
    )
  )

let map_accessor_list (env : env) ((v1, v2, v3) : CST.accessor_list) =
  let v1 = (* "{" *) token env v1 in
  let v2 =
    R.List (List.map (map_accessor_declaration env) v2)
  in
  let v3 = (* "}" *) token env v3 in
  R.Tuple [v1; v2; v3]

let map_enum_member_declaration_list (env : env) ((v1, v2, v3, v4) : CST.enum_member_declaration_list) =
  let v1 = (* "{" *) token env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = map_enum_member_declaration env v1 in
        let v2 =
          R.List (List.map (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_enum_member_declaration env v2 in
            R.Tuple [v1; v2]
          ) v2)
        in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  let v3 =
    (match v3 with
    | Some tok -> R.Option (Some (
        (* "," *) token env tok
      ))
    | None -> R.Option None)
  in
  let v4 = (* "}" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

let map_enum_declaration (env : env) ((v1, v2, v3, v4, v5, v6, v7) : CST.enum_declaration) =
  let v1 = R.List (List.map (map_attribute_list env) v1) in
  let v2 = R.List (List.map (map_modifier env) v2) in
  let v3 = (* "enum" *) token env v3 in
  let v4 = map_implicit_parameter_list env v4 in
  let v5 =
    (match v5 with
    | Some x -> R.Option (Some (
        map_base_list env x
      ))
    | None -> R.Option None)
  in
  let v6 = map_enum_member_declaration_list env v6 in
  let v7 = (* opt_semi *) token env v7 in
  R.Tuple [v1; v2; v3; v4; v5; v6; v7]

let rec map_class_declaration (env : env) ((v1, v2, v3, v4, v5, v6, v7, v8, v9) : CST.class_declaration) =
  let v1 = R.List (List.map (map_attribute_list env) v1) in
  let v2 = R.List (List.map (map_modifier env) v2) in
  let v3 = (* "class" *) token env v3 in
  let v4 = map_implicit_parameter_list env v4 in
  let v5 =
    (match v5 with
    | Some x -> R.Option (Some (
        map_type_parameter_list env x
      ))
    | None -> R.Option None)
  in
  let v6 =
    (match v6 with
    | Some x -> R.Option (Some (
        map_base_list env x
      ))
    | None -> R.Option None)
  in
  let v7 =
    R.List (List.map (map_type_parameter_constraints_clause env) v7)
  in
  let v8 = map_declaration_list env v8 in
  let v9 = (* opt_semi *) token env v9 in
  R.Tuple [v1; v2; v3; v4; v5; v6; v7; v8; v9]

and map_declaration (env : env) (x : CST.declaration) =
  (match x with
  | `Class_decl x -> R.Case ("Class_decl",
      map_class_declaration env x
    )
  | `Cons_decl (v1, v2, v3, v4, v5, v6) -> R.Case ("Cons_decl",
      let v1 = R.List (List.map (map_attribute_list env) v1) in
      let v2 = R.List (List.map (map_modifier env) v2) in
      let v3 = map_implicit_parameter_list env v3 in
      let v4 = map_parameter_list env v4 in
      let v5 =
        (match v5 with
        | Some x -> R.Option (Some (
            map_constructor_initializer env x
          ))
        | None -> R.Option None)
      in
      let v6 = map_function_body env v6 in
      R.Tuple [v1; v2; v3; v4; v5; v6]
    )
  | `Conv_op_decl (v1, v2, v3, v4, v5, v6, v7, v8, v9) -> R.Case ("Conv_op_decl",
      let v1 = R.List (List.map (map_attribute_list env) v1) in
      let v2 = R.List (List.map (map_modifier env) v2) in
      let v3 =
        (match v3 with
        | `Impl tok -> R.Case ("Impl",
            (* "implicit" *) token env tok
          )
        | `Expl tok -> R.Case ("Expl",
            (* "explicit" *) token env tok
          )
        )
      in
      let v4 =
        (match v4 with
        | Some x -> R.Option (Some (
            map_explicit_interface_specifier env x
          ))
        | None -> R.Option None)
      in
      let v5 = (* "operator" *) token env v5 in
      let v6 =
        (match v6 with
        | Some tok -> R.Option (Some (
            (* "checked" *) token env tok
          ))
        | None -> R.Option None)
      in
      let v7 = map_type_pattern env v7 in
      let v8 = map_parameter_list env v8 in
      let v9 = map_function_body env v9 in
      R.Tuple [v1; v2; v3; v4; v5; v6; v7; v8; v9]
    )
  | `Dele_decl x -> R.Case ("Dele_decl",
      map_delegate_declaration env x
    )
  | `Dest_decl (v1, v2, v3, v4, v5, v6) -> R.Case ("Dest_decl",
      let v1 = R.List (List.map (map_attribute_list env) v1) in
      let v2 =
        (match v2 with
        | Some tok -> R.Option (Some (
            (* "extern" *) token env tok
          ))
        | None -> R.Option None)
      in
      let v3 = (* "~" *) token env v3 in
      let v4 = map_implicit_parameter_list env v4 in
      let v5 = map_parameter_list env v5 in
      let v6 = map_function_body env v6 in
      R.Tuple [v1; v2; v3; v4; v5; v6]
    )
  | `Enum_decl x -> R.Case ("Enum_decl",
      map_enum_declaration env x
    )
  | `Event_decl (v1, v2, v3, v4, v5, v6, v7) -> R.Case ("Event_decl",
      let v1 = R.List (List.map (map_attribute_list env) v1) in
      let v2 = R.List (List.map (map_modifier env) v2) in
      let v3 = (* "event" *) token env v3 in
      let v4 = map_type_pattern env v4 in
      let v5 =
        (match v5 with
        | Some x -> R.Option (Some (
            map_explicit_interface_specifier env x
          ))
        | None -> R.Option None)
      in
      let v6 = map_implicit_parameter_list env v6 in
      let v7 =
        (match v7 with
        | `Acce_list x -> R.Case ("Acce_list",
            map_accessor_list env x
          )
        | `SEMI tok -> R.Case ("SEMI",
            (* ";" *) token env tok
          )
        )
      in
      R.Tuple [v1; v2; v3; v4; v5; v6; v7]
    )
  | `Event_field_decl (v1, v2, v3, v4, v5) -> R.Case ("Event_field_decl",
      let v1 = R.List (List.map (map_attribute_list env) v1) in
      let v2 = R.List (List.map (map_modifier env) v2) in
      let v3 = (* "event" *) token env v3 in
      let v4 = map_variable_declaration env v4 in
      let v5 = (* ";" *) token env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `Field_decl (v1, v2, v3, v4) -> R.Case ("Field_decl",
      let v1 = R.List (List.map (map_attribute_list env) v1) in
      let v2 = R.List (List.map (map_modifier env) v2) in
      let v3 = map_variable_declaration env v3 in
      let v4 = (* ";" *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Inde_decl (v1, v2, v3, v4, v5, v6, v7) -> R.Case ("Inde_decl",
      let v1 = R.List (List.map (map_attribute_list env) v1) in
      let v2 = R.List (List.map (map_modifier env) v2) in
      let v3 = map_type_pattern env v3 in
      let v4 =
        (match v4 with
        | Some x -> R.Option (Some (
            map_explicit_interface_specifier env x
          ))
        | None -> R.Option None)
      in
      let v5 = (* "this" *) token env v5 in
      let v6 = map_bracketed_parameter_list env v6 in
      let v7 =
        (match v7 with
        | `Acce_list x -> R.Case ("Acce_list",
            map_accessor_list env x
          )
        | `Arrow_exp_clause_SEMI (v1, v2) -> R.Case ("Arrow_exp_clause_SEMI",
            let v1 = map_arrow_expression_clause env v1 in
            let v2 = (* ";" *) token env v2 in
            R.Tuple [v1; v2]
          )
        )
      in
      R.Tuple [v1; v2; v3; v4; v5; v6; v7]
    )
  | `Inte_decl x -> R.Case ("Inte_decl",
      map_interface_declaration env x
    )
  | `Meth_decl (v1, v2, v3, v4, v5, v6, v7, v8, v9) -> R.Case ("Meth_decl",
      let v1 = R.List (List.map (map_attribute_list env) v1) in
      let v2 = R.List (List.map (map_modifier env) v2) in
      let v3 = map_type_pattern env v3 in
      let v4 =
        (match v4 with
        | Some x -> R.Option (Some (
            map_explicit_interface_specifier env x
          ))
        | None -> R.Option None)
      in
      let v5 = map_implicit_parameter_list env v5 in
      let v6 =
        (match v6 with
        | Some x -> R.Option (Some (
            map_type_parameter_list env x
          ))
        | None -> R.Option None)
      in
      let v7 = map_parameter_list env v7 in
      let v8 =
        R.List (List.map (map_type_parameter_constraints_clause env) v8)
      in
      let v9 = map_function_body env v9 in
      R.Tuple [v1; v2; v3; v4; v5; v6; v7; v8; v9]
    )
  | `Name_decl x -> R.Case ("Name_decl",
      map_namespace_declaration env x
    )
  | `Op_decl (v1, v2, v3, v4, v5, v6, v7, v8, v9) -> R.Case ("Op_decl",
      let v1 = R.List (List.map (map_attribute_list env) v1) in
      let v2 = R.List (List.map (map_modifier env) v2) in
      let v3 = map_type_pattern env v3 in
      let v4 =
        (match v4 with
        | Some x -> R.Option (Some (
            map_explicit_interface_specifier env x
          ))
        | None -> R.Option None)
      in
      let v5 = (* "operator" *) token env v5 in
      let v6 =
        (match v6 with
        | Some tok -> R.Option (Some (
            (* "checked" *) token env tok
          ))
        | None -> R.Option None)
      in
      let v7 = map_overloadable_operator env v7 in
      let v8 = map_parameter_list env v8 in
      let v9 = map_function_body env v9 in
      R.Tuple [v1; v2; v3; v4; v5; v6; v7; v8; v9]
    )
  | `Prop_decl (v1, v2, v3, v4, v5, v6) -> R.Case ("Prop_decl",
      let v1 = R.List (List.map (map_attribute_list env) v1) in
      let v2 = R.List (List.map (map_modifier env) v2) in
      let v3 = map_type_pattern env v3 in
      let v4 =
        (match v4 with
        | Some x -> R.Option (Some (
            map_explicit_interface_specifier env x
          ))
        | None -> R.Option None)
      in
      let v5 = map_implicit_parameter_list env v5 in
      let v6 =
        (match v6 with
        | `Acce_list_opt_EQ_exp_SEMI (v1, v2) -> R.Case ("Acce_list_opt_EQ_exp_SEMI",
            let v1 = map_accessor_list env v1 in
            let v2 =
              (match v2 with
              | Some (v1, v2, v3) -> R.Option (Some (
                  let v1 = (* "=" *) token env v1 in
                  let v2 = map_expression env v2 in
                  let v3 = (* ";" *) token env v3 in
                  R.Tuple [v1; v2; v3]
                ))
              | None -> R.Option None)
            in
            R.Tuple [v1; v2]
          )
        | `Arrow_exp_clause_SEMI (v1, v2) -> R.Case ("Arrow_exp_clause_SEMI",
            let v1 = map_arrow_expression_clause env v1 in
            let v2 = (* ";" *) token env v2 in
            R.Tuple [v1; v2]
          )
        )
      in
      R.Tuple [v1; v2; v3; v4; v5; v6]
    )
  | `Record_decl x -> R.Case ("Record_decl",
      map_record_declaration env x
    )
  | `Record_struct_decl x -> R.Case ("Record_struct_decl",
      map_record_struct_declaration env x
    )
  | `Struct_decl x -> R.Case ("Struct_decl",
      map_struct_declaration env x
    )
  | `Using_dire x -> R.Case ("Using_dire",
      map_using_directive env x
    )
  | `Ellips tok -> R.Case ("Ellips",
      (* "..." *) token env tok
    )
  )

and map_declaration_list (env : env) ((v1, v2, v3) : CST.declaration_list) =
  let v1 = (* "{" *) token env v1 in
  let v2 = R.List (List.map (map_declaration env) v2) in
  let v3 = (* "}" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_interface_declaration (env : env) ((v1, v2, v3, v4, v5, v6, v7, v8, v9) : CST.interface_declaration) =
  let v1 = R.List (List.map (map_attribute_list env) v1) in
  let v2 = R.List (List.map (map_modifier env) v2) in
  let v3 = (* "interface" *) token env v3 in
  let v4 = map_implicit_parameter_list env v4 in
  let v5 =
    (match v5 with
    | Some x -> R.Option (Some (
        map_type_parameter_list env x
      ))
    | None -> R.Option None)
  in
  let v6 =
    (match v6 with
    | Some x -> R.Option (Some (
        map_base_list env x
      ))
    | None -> R.Option None)
  in
  let v7 =
    R.List (List.map (map_type_parameter_constraints_clause env) v7)
  in
  let v8 = map_declaration_list env v8 in
  let v9 = (* opt_semi *) token env v9 in
  R.Tuple [v1; v2; v3; v4; v5; v6; v7; v8; v9]

and map_namespace_declaration (env : env) ((v1, v2, v3, v4) : CST.namespace_declaration) =
  let v1 = (* "namespace" *) token env v1 in
  let v2 = map_type_name env v2 in
  let v3 = map_declaration_list env v3 in
  let v4 = (* opt_semi *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_record_body (env : env) (x : CST.record_body) =
  (match x with
  | `Decl_list x -> R.Case ("Decl_list",
      map_declaration_list env x
    )
  | `SEMI tok -> R.Case ("SEMI",
      (* ";" *) token env tok
    )
  )

and map_record_declaration (env : env) ((v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11) : CST.record_declaration) =
  let v1 = R.List (List.map (map_attribute_list env) v1) in
  let v2 = R.List (List.map (map_modifier env) v2) in
  let v3 = (* "record" *) token env v3 in
  let v4 =
    (match v4 with
    | Some tok -> R.Option (Some (
        (* "class" *) token env tok
      ))
    | None -> R.Option None)
  in
  let v5 = map_implicit_parameter_list env v5 in
  let v6 =
    (match v6 with
    | Some x -> R.Option (Some (
        map_type_parameter_list env x
      ))
    | None -> R.Option None)
  in
  let v7 =
    (match v7 with
    | Some x -> R.Option (Some (
        map_parameter_list env x
      ))
    | None -> R.Option None)
  in
  let v8 =
    (match v8 with
    | Some x -> R.Option (Some (
        map_record_base env x
      ))
    | None -> R.Option None)
  in
  let v9 =
    R.List (List.map (map_type_parameter_constraints_clause env) v9)
  in
  let v10 = map_record_body env v10 in
  let v11 = (* opt_semi *) token env v11 in
  R.Tuple [v1; v2; v3; v4; v5; v6; v7; v8; v9; v10; v11]

and map_record_struct_declaration (env : env) ((v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11) : CST.record_struct_declaration) =
  let v1 = R.List (List.map (map_attribute_list env) v1) in
  let v2 = R.List (List.map (map_modifier env) v2) in
  let v3 = (* "record" *) token env v3 in
  let v4 = (* "struct" *) token env v4 in
  let v5 = map_implicit_parameter_list env v5 in
  let v6 =
    (match v6 with
    | Some x -> R.Option (Some (
        map_type_parameter_list env x
      ))
    | None -> R.Option None)
  in
  let v7 =
    (match v7 with
    | Some x -> R.Option (Some (
        map_parameter_list env x
      ))
    | None -> R.Option None)
  in
  let v8 =
    (match v8 with
    | Some x -> R.Option (Some (
        map_record_base env x
      ))
    | None -> R.Option None)
  in
  let v9 =
    R.List (List.map (map_type_parameter_constraints_clause env) v9)
  in
  let v10 = map_record_body env v10 in
  let v11 = (* opt_semi *) token env v11 in
  R.Tuple [v1; v2; v3; v4; v5; v6; v7; v8; v9; v10; v11]

and map_struct_declaration (env : env) ((v1, v2, v3, v4, v5, v6, v7, v8, v9, v10) : CST.struct_declaration) =
  let v1 = R.List (List.map (map_attribute_list env) v1) in
  let v2 = R.List (List.map (map_modifier env) v2) in
  let v3 =
    (match v3 with
    | Some tok -> R.Option (Some (
        (* "ref" *) token env tok
      ))
    | None -> R.Option None)
  in
  let v4 = (* "struct" *) token env v4 in
  let v5 = map_implicit_parameter_list env v5 in
  let v6 =
    (match v6 with
    | Some x -> R.Option (Some (
        map_type_parameter_list env x
      ))
    | None -> R.Option None)
  in
  let v7 =
    (match v7 with
    | Some x -> R.Option (Some (
        map_base_list env x
      ))
    | None -> R.Option None)
  in
  let v8 =
    R.List (List.map (map_type_parameter_constraints_clause env) v8)
  in
  let v9 = map_declaration_list env v9 in
  let v10 = (* opt_semi *) token env v10 in
  R.Tuple [v1; v2; v3; v4; v5; v6; v7; v8; v9; v10]

let map_type_declaration (env : env) (x : CST.type_declaration) =
  (match x with
  | `Class_decl x -> R.Case ("Class_decl",
      map_class_declaration env x
    )
  | `Struct_decl x -> R.Case ("Struct_decl",
      map_struct_declaration env x
    )
  | `Inte_decl x -> R.Case ("Inte_decl",
      map_interface_declaration env x
    )
  | `Enum_decl x -> R.Case ("Enum_decl",
      map_enum_declaration env x
    )
  | `Dele_decl x -> R.Case ("Dele_decl",
      map_delegate_declaration env x
    )
  | `Record_decl x -> R.Case ("Record_decl",
      map_record_declaration env x
    )
  | `Record_struct_decl x -> R.Case ("Record_struct_decl",
      map_record_struct_declaration env x
    )
  | `Ellips tok -> R.Case ("Ellips",
      (* "..." *) token env tok
    )
  )

let map_namespace_member_declaration (env : env) (x : CST.namespace_member_declaration) =
  (match x with
  | `Name_decl x -> R.Case ("Name_decl",
      map_namespace_declaration env x
    )
  | `Type_decl x -> R.Case ("Type_decl",
      map_type_declaration env x
    )
  )

let map_file_scoped_namespace_declaration (env : env) ((v1, v2, v3, v4, v5, v6, v7, v8) : CST.file_scoped_namespace_declaration) =
  let v1 = R.List (List.map (map_global_statement env) v1) in
  let v2 =
    R.List (List.map (map_namespace_member_declaration env) v2)
  in
  let v3 = (* "namespace" *) token env v3 in
  let v4 = map_type_name env v4 in
  let v5 = (* ";" *) token env v5 in
  let v6 =
    R.List (List.map (map_extern_alias_directive env) v6)
  in
  let v7 = R.List (List.map (map_using_directive env) v7) in
  let v8 = R.List (List.map (map_type_declaration env) v8) in
  R.Tuple [v1; v2; v3; v4; v5; v6; v7; v8]

let map_compilation_unit (env : env) (x : CST.compilation_unit) =
  (match x with
  | `Rep_extern_alias_dire_rep_using_dire_rep_global_attr_list_choice_rep_global_stmt_rep_name_member_decl (v1, v2, v3, v4) -> R.Case ("Rep_extern_alias_dire_rep_using_dire_rep_global_attr_list_choice_rep_global_stmt_rep_name_member_decl",
      let v1 =
        R.List (List.map (map_extern_alias_directive env) v1)
      in
      let v2 = R.List (List.map (map_using_directive env) v2) in
      let v3 =
        R.List (List.map (map_global_attribute_list env) v3)
      in
      let v4 =
        (match v4 with
        | `Rep_global_stmt_rep_name_member_decl (v1, v2) -> R.Case ("Rep_global_stmt_rep_name_member_decl",
            let v1 = R.List (List.map (map_global_statement env) v1) in
            let v2 =
              R.List (List.map (map_namespace_member_declaration env) v2)
            in
            R.Tuple [v1; v2]
          )
        | `File_scoped_name_decl x -> R.Case ("File_scoped_name_decl",
            map_file_scoped_namespace_declaration env x
          )
        )
      in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Semg_exp (v1, v2) -> R.Case ("Semg_exp",
      let v1 = (* "__SEMGREP_EXPRESSION" *) token env v1 in
      let v2 = map_expression env v2 in
      R.Tuple [v1; v2]
    )
  )

let map_comment (env : env) (tok : CST.comment) =
  (* comment *) token env tok

let map_preprocessor_call (env : env) ((v1, v2, v3) : CST.preprocessor_call) =
  let v1 = (* pattern #[ \t]* *) token env v1 in
  let v2 =
    (match v2 with
    | `Null_dire x -> R.Case ("Null_dire",
        map_nullable_directive env x
      )
    | `Define_dire x -> R.Case ("Define_dire",
        map_define_directive env x
      )
    | `Undef_dire x -> R.Case ("Undef_dire",
        map_undef_directive env x
      )
    | `If_dire x -> R.Case ("If_dire",
        map_if_directive env x
      )
    | `Else_dire tok -> R.Case ("Else_dire",
        (* "else" *) token env tok
      )
    | `Elif_dire x -> R.Case ("Elif_dire",
        map_elif_directive env x
      )
    | `Endif_dire tok -> R.Case ("Endif_dire",
        (* "endif" *) token env tok
      )
    | `Region_dire x -> R.Case ("Region_dire",
        map_region_directive env x
      )
    | `Endr_dire x -> R.Case ("Endr_dire",
        map_endregion_directive env x
      )
    | `Error_dire x -> R.Case ("Error_dire",
        map_error_directive env x
      )
    | `Warn_dire x -> R.Case ("Warn_dire",
        map_warning_directive env x
      )
    | `Line_dire x -> R.Case ("Line_dire",
        map_line_directive env x
      )
    | `Pragma_dire x -> R.Case ("Pragma_dire",
        map_pragma_directive env x
      )
    | `Ref_dire x -> R.Case ("Ref_dire",
        map_reference_directive env x
      )
    | `Load_dire x -> R.Case ("Load_dire",
        map_load_directive env x
      )
    | `Sheb_dire x -> R.Case ("Sheb_dire",
        map_shebang_directive env x
      )
    )
  in
  let v3 = (* preproc_directive_end *) token env v3 in
  R.Tuple [v1; v2; v3]

let dump_tree root =
  map_compilation_unit () root
  |> Tree_sitter_run.Raw_tree.to_channel stdout

let map_extra (env : env) (x : CST.extra) =
  match x with
  | `Comment (_loc, x) -> ("comment", "comment", map_comment env x)
  | `Preprocessor_call (_loc, x) -> ("preprocessor_call", "preprocessor_call", map_preprocessor_call env x)

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
