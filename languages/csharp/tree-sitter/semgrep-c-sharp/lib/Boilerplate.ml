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

let map_interpolation_string_content (env : env) (tok : CST.interpolation_string_content) =
  (* interpolation_string_content *) token env tok

let map_verbatim_string_literal (env : env) (tok : CST.verbatim_string_literal) =
  (* verbatim_string_literal *) token env tok

let map_tok_choice_pat_8eb8081 (env : env) (tok : CST.tok_choice_pat_8eb8081) =
  (* tok_choice_pat_8eb8081 *) token env tok

let map_directive_separator (env : env) (tok : CST.directive_separator) =
  (* directive_separator *) token env tok

let map_interpolation_start_quote (env : env) (tok : CST.interpolation_start_quote) =
  (* interpolation_start_quote *) token env tok

let map_pat_4946ad9 (env : env) (tok : CST.pat_4946ad9) =
  (* pattern #[ 	]*undef *) token env tok

let map_string_literal_encoding (env : env) (tok : CST.string_literal_encoding) =
  (* pattern (u|U)8 *) token env tok

let map_string_literal_content (env : env) (tok : CST.string_literal_content) =
  (* pattern "[^\"\\\\\\n]+" *) token env tok

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
    | `Type_599dcce tok -> R.Case ("Type_599dcce",
        (* "type" *) token env tok
      )
    | `Type_2a87ba5 tok -> R.Case ("Type_2a87ba5",
        (* "typevar" *) token env tok
      )
    )
  in
  let v2 = (* ":" *) token env v2 in
  R.Tuple [v1; v2]

let map_boolean_literal (env : env) (x : CST.boolean_literal) =
  (match x with
  | `True tok -> R.Case ("True",
      (* "true" *) token env tok
    )
  | `False tok -> R.Case ("False",
      (* "false" *) token env tok
    )
  )

let map_anon_choice_static_d4628a0 (env : env) (x : CST.anon_choice_static_d4628a0) =
  (match x with
  | `Static tok -> R.Case ("Static",
      (* "static" *) token env tok
    )
  | `Async tok -> R.Case ("Async",
      (* "async" *) token env tok
    )
  )

let map_pat_c1fe926 (env : env) (tok : CST.pat_c1fe926) =
  (* pattern (u|U)8 *) token env tok

let map_anon_choice_out_a7988a7 (env : env) (x : CST.anon_choice_out_a7988a7) =
  (match x with
  | `Out tok -> R.Case ("Out",
      (* "out" *) token env tok
    )
  | `In tok -> R.Case ("In",
      (* "in" *) token env tok
    )
  )

let map_reserved_identifier (env : env) (x : CST.reserved_identifier) =
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
  | `Field tok -> R.Case ("Field",
      (* "field" *) token env tok
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
  | `Meth tok -> R.Case ("Meth",
      (* "method" *) token env tok
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
  | `Param tok -> R.Case ("Param",
      (* "param" *) token env tok
    )
  | `Prop tok -> R.Case ("Prop",
      (* "property" *) token env tok
    )
  | `Scoped tok -> R.Case ("Scoped",
      (* "scoped" *) token env tok
    )
  | `Select tok -> R.Case ("Select",
      (* "select" *) token env tok
    )
  | `Type_599dcce tok -> R.Case ("Type_599dcce",
      (* "type" *) token env tok
    )
  | `Type_2a87ba5 tok -> R.Case ("Type_2a87ba5",
      (* "typevar" *) token env tok
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
  | `Exte tok -> R.Case ("Exte",
      (* "extension" *) token env tok
    )
  )

let map_preproc_else_if_start (env : env) (tok : CST.preproc_else_if_start) =
  (* preproc_else_if_start *) token env tok

let map_preproc_arg (env : env) (tok : CST.preproc_arg) =
  (* preproc_arg *) token env tok

let map_pat_c3ea183 (env : env) (tok : CST.pat_c3ea183) =
  (* pattern #[ 	]*define *) token env tok

let map_anon_choice_chec_7987657 (env : env) (x : CST.anon_choice_chec_7987657) =
  (match x with
  | `Chec tok -> R.Case ("Chec",
      (* "checked" *) token env tok
    )
  | `Unch tok -> R.Case ("Unch",
      (* "unchecked" *) token env tok
    )
  )

let map_directive_name (env : env) (tok : CST.directive_name) =
  (* directive_name *) token env tok

let map_raw_string_start (env : env) (tok : CST.raw_string_start) =
  (* raw_string_start *) token env tok

let map_imm_tok_pat_5381807 (env : env) (tok : CST.imm_tok_pat_5381807) =
  (* pattern [ \t]+[^\s]([^\r\n]*[^\s])? *) token env tok

let map_predefined_type (env : env) (tok : CST.predefined_type) =
  (* predefined_type *) token env tok

let map_pat_bfeb4bb (env : env) (tok : CST.pat_bfeb4bb) =
  (* pattern #[ 	]*elif *) token env tok

let map_shebang_directive (env : env) (tok : CST.shebang_directive) =
  (* shebang_directive *) token env tok

let map_directive_value (env : env) (tok : CST.directive_value) =
  (* directive_value *) token env tok

let map_semgrep_metavariable (env : env) (tok : CST.semgrep_metavariable) =
  (* semgrep_metavariable *) token env tok

let map_tok_pat_245b2d0 (env : env) (tok : CST.tok_pat_245b2d0) =
  (* tok_pat_245b2d0 *) token env tok

let map_anon_choice_scoped_3da72f4 (env : env) (x : CST.anon_choice_scoped_3da72f4) =
  (match x with
  | `Scoped tok -> R.Case ("Scoped",
      (* "scoped" *) token env tok
    )
  | `Ref tok -> R.Case ("Ref",
      (* "ref" *) token env tok
    )
  | `Out tok -> R.Case ("Out",
      (* "out" *) token env tok
    )
  | `In tok -> R.Case ("In",
      (* "in" *) token env tok
    )
  | `Read tok -> R.Case ("Read",
      (* "readonly" *) token env tok
    )
  )

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

let map_anon_choice_this_c32f042 (env : env) (x : CST.anon_choice_this_c32f042) =
  (match x with
  | `This tok -> R.Case ("This",
      (* "this" *) token env tok
    )
  | `Scoped tok -> R.Case ("Scoped",
      (* "scoped" *) token env tok
    )
  | `Ref tok -> R.Case ("Ref",
      (* "ref" *) token env tok
    )
  | `Read tok -> R.Case ("Read",
      (* "readonly" *) token env tok
    )
  )

let map_pat_52ffbd7 (env : env) (tok : CST.pat_52ffbd7) =
  (* pattern "[^}\"]+" *) token env tok

let map_interpolation_verbatim_start (env : env) (tok : CST.interpolation_verbatim_start) =
  (* interpolation_verbatim_start *) token env tok

let map_character_literal_content (env : env) (tok : CST.character_literal_content) =
  (* pattern "[^'\\\\]" *) token env tok

let map_pat_b56baf5 (env : env) (tok : CST.pat_b56baf5) =
  (* pattern #[ 	]*region *) token env tok

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

let map_pat_1d78758 (env : env) (tok : CST.pat_1d78758) =
  (* pattern \n *) token env tok

let map_imm_tok_prop (env : env) (tok : CST.imm_tok_prop) =
  (* "property" *) token env tok

let map_pat_3df6e71 (env : env) (tok : CST.pat_3df6e71) =
  (* pattern #[ 	]*if *) token env tok

let map_optional_semi (env : env) (tok : CST.optional_semi) =
  (* optional_semi *) token env tok

let map_pat_eb92600 (env : env) (tok : CST.pat_eb92600) =
  (* pattern #[ 	]*pragma *) token env tok

let map_pat_fdf8f81 (env : env) (tok : CST.pat_fdf8f81) =
  (* pattern #[ 	]*nullable *) token env tok

let map_interpolation_regular_start (env : env) (tok : CST.interpolation_regular_start) =
  (* interpolation_regular_start *) token env tok

let map_pat_c46d1b2 (env : env) (tok : CST.pat_c46d1b2) =
  (* pattern #[ 	]*endif *) token env tok

let map_lambda_paren_open (env : env) (tok : CST.lambda_paren_open) =
  (* lambda_paren_open *) token env tok

let map_pat_721a115 (env : env) (tok : CST.pat_721a115) =
  (* pattern #[ 	]*endregion *) token env tok

let map_interpolation_raw_start (env : env) (tok : CST.interpolation_raw_start) =
  (* interpolation_raw_start *) token env tok

let map_pat_d299690 (env : env) (tok : CST.pat_d299690) =
  (* pattern #[ 	]*line *) token env tok

let map_interpolation_end_quote (env : env) (tok : CST.interpolation_end_quote) =
  (* interpolation_end_quote *) token env tok

let map_raw_string_content (env : env) (tok : CST.raw_string_content) =
  (* raw_string_content *) token env tok

let map_pat_ca788a8 (env : env) (tok : CST.pat_ca788a8) =
  (* pattern #[ 	]*warning *) token env tok

let map_semgrep_variadic_metavariable (env : env) (tok : CST.semgrep_variadic_metavariable) =
  (* pattern \$\.\.\.[A-Z_][A-Z_0-9]* *) token env tok

let map_identifier_token (env : env) (tok : CST.identifier_token) =
  (* identifier_token *) token env tok

let map_imm_tok_pat_555404d (env : env) (tok : CST.imm_tok_pat_555404d) =
  (* pattern [A-Za-z_][A-Za-z0-9_-]* *) token env tok

let map_pat_56631e5 (env : env) (tok : CST.pat_56631e5) =
  (* pattern #[ 	]*else *) token env tok

let map_real_literal (env : env) (tok : CST.real_literal) =
  (* real_literal *) token env tok

let map_raw_string_end (env : env) (tok : CST.raw_string_end) =
  (* raw_string_end *) token env tok

let map_integer_literal (env : env) (tok : CST.integer_literal) =
  (* integer_literal *) token env tok

let map_interpolation_open_brace (env : env) (tok : CST.interpolation_open_brace) =
  (* interpolation_open_brace *) token env tok

let map_imm_tok_sdk (env : env) (tok : CST.imm_tok_sdk) =
  (* "sdk" *) token env tok

let map_pat_8a5c9c8 (env : env) (tok : CST.pat_8a5c9c8) =
  (* pattern #[ 	]*error *) token env tok

let map_imm_tok_pack (env : env) (tok : CST.imm_tok_pack) =
  (* "package" *) token env tok

let map_interpolation_close_brace (env : env) (tok : CST.interpolation_close_brace) =
  (* interpolation_close_brace *) token env tok

let map_anon_choice_DOT_2ad1dab (env : env) (x : CST.anon_choice_DOT_2ad1dab) =
  (match x with
  | `DOT tok -> R.Case ("DOT",
      (* "." *) token env tok
    )
  | `DASHGT tok -> R.Case ("DASHGT",
      (* "->" *) token env tok
    )
  )

let map_escape_sequence (env : env) (x : CST.escape_sequence) =
  (match x with
  | `Tok_choice_pat_8eb8081 x -> R.Case ("Tok_choice_pat_8eb8081",
      map_tok_choice_pat_8eb8081 env x
    )
  | `Tok_pat_245b2d0 x -> R.Case ("Tok_pat_245b2d0",
      map_tok_pat_245b2d0 env x
    )
  )

let map_anon_choice_choice_this_6a06011 (env : env) (x : CST.anon_choice_choice_this_6a06011) =
  (match x with
  | `Choice_this x -> R.Case ("Choice_this",
      map_anon_choice_this_c32f042 env x
    )
  | `Choice_out x -> R.Case ("Choice_out",
      map_anon_choice_out_a7988a7 env x
    )
  )

let map_interpolation_format_clause (env : env) ((v1, v2) : CST.interpolation_format_clause) =
  let v1 = (* ":" *) token env v1 in
  let v2 = map_pat_52ffbd7 env v2 in
  R.Tuple [v1; v2]

let map_identifier (env : env) (x : CST.identifier) =
  (match x with
  | `Choice_id_tok x -> R.Case ("Choice_id_tok",
      (match x with
      | `Id_tok tok -> R.Case ("Id_tok",
          (* identifier_token *) token env tok
        )
      | `Rese_id x -> R.Case ("Rese_id",
          map_reserved_identifier env x
        )
      )
    )
  | `Semg_meta tok -> R.Case ("Semg_meta",
      (* semgrep_metavariable *) token env tok
    )
  )

let map_ignored_directive (env : env) ((v1, v2, v3) : CST.ignored_directive) =
  let v1 = (* "#:" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        (match x with
        | `Choice_imm_tok_pack_opt_dire_name_opt_dire_sepa_opt_dire_value (v1, v2) -> R.Case ("Choice_imm_tok_pack_opt_dire_name_opt_dire_sepa_opt_dire_value",
            let v1 =
              (match v1 with
              | `Imm_tok_pack x -> R.Case ("Imm_tok_pack",
                  map_imm_tok_pack env x
                )
              | `Imm_tok_sdk x -> R.Case ("Imm_tok_sdk",
                  map_imm_tok_sdk env x
                )
              | `Imm_tok_prop x -> R.Case ("Imm_tok_prop",
                  map_imm_tok_prop env x
                )
              )
            in
            let v2 =
              (match v2 with
              | Some (v1, v2, v3) -> R.Option (Some (
                  let v1 = (* directive_name *) token env v1 in
                  let v2 =
                    (match v2 with
                    | Some tok -> R.Option (Some (
                        (* directive_separator *) token env tok
                      ))
                    | None -> R.Option None)
                  in
                  let v3 =
                    (match v3 with
                    | Some tok -> R.Option (Some (
                        (* directive_value *) token env tok
                      ))
                    | None -> R.Option None)
                  in
                  R.Tuple [v1; v2; v3]
                ))
              | None -> R.Option None)
            in
            R.Tuple [v1; v2]
          )
        | `Imm_tok_pat_555404d_opt_dire_value (v1, v2) -> R.Case ("Imm_tok_pat_555404d_opt_dire_value",
            let v1 = map_imm_tok_pat_555404d env v1 in
            let v2 =
              (match v2 with
              | Some tok -> R.Option (Some (
                  (* directive_value *) token env tok
                ))
              | None -> R.Option None)
            in
            R.Tuple [v1; v2]
          )
        | `Imm_tok_pat_5381807 x -> R.Case ("Imm_tok_pat_5381807",
            map_imm_tok_pat_5381807 env x
          )
        )
      ))
    | None -> R.Option None)
  in
  let v3 = map_pat_1d78758 env v3 in
  R.Tuple [v1; v2; v3]

let map_character_literal (env : env) ((v1, v2, v3) : CST.character_literal) =
  let v1 = (* "'" *) token env v1 in
  let v2 =
    (match v2 with
    | `Char_lit_content tok -> R.Case ("Char_lit_content",
        (* pattern "[^'\\\\]" *) token env tok
      )
    | `Esc_seq x -> R.Case ("Esc_seq",
        map_escape_sequence env x
      )
    )
  in
  let v3 = (* "'" *) token env v3 in
  R.Tuple [v1; v2; v3]

let map_string_literal (env : env) ((v1, v2, v3, v4) : CST.string_literal) =
  let v1 = (* "\"" *) token env v1 in
  let v2 =
    R.List (List.map (fun x ->
      (match x with
      | `Str_lit_content tok -> R.Case ("Str_lit_content",
          (* pattern "[^\"\\\\\\n]+" *) token env tok
        )
      | `Esc_seq x -> R.Case ("Esc_seq",
          map_escape_sequence env x
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

let map_join_into_clause (env : env) ((v1, v2) : CST.join_into_clause) =
  let v1 = (* "into" *) token env v1 in
  let v2 = map_identifier env v2 in
  R.Tuple [v1; v2]

let map_anon_choice_Cdecl_63b6085 (env : env) (x : CST.anon_choice_Cdecl_63b6085) =
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
      map_identifier env x
    )
  )

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
      map_identifier env x
    )
  )

let rec map_anon_choice_id_c036834 (env : env) (x : CST.anon_choice_id_c036834) =
  (match x with
  | `Id x -> R.Case ("Id",
      map_identifier env x
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
  let v2 = map_anon_choice_id_c036834 env v2 in
  let v3 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_anon_choice_id_c036834 env v2 in
      R.Tuple [v1; v2]
    ) v3)
  in
  let v4 = (* ")" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

let map_anon_choice_rep1_choice_scoped_id_5b70ee9 (env : env) (x : CST.anon_choice_rep1_choice_scoped_id_5b70ee9) =
  (match x with
  | `Rep1_choice_scoped_id (v1, v2) -> R.Case ("Rep1_choice_scoped_id",
      let v1 =
        R.List (List.map (map_anon_choice_scoped_3da72f4 env) v1)
      in
      let v2 = map_identifier env v2 in
      R.Tuple [v1; v2]
    )
  | `Id x -> R.Case ("Id",
      map_identifier env x
    )
  )

let map_anon_choice_id_f2cdd14 (env : env) (x : CST.anon_choice_id_f2cdd14) =
  (match x with
  | `Id x -> R.Case ("Id",
      map_identifier env x
    )
  | `Int_lit tok -> R.Case ("Int_lit",
      (* integer_literal *) token env tok
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
      map_identifier env x
    )
  | `Bool_lit x -> R.Case ("Bool_lit",
      map_boolean_literal env x
    )
  | `Int_lit tok -> R.Case ("Int_lit",
      (* integer_literal *) token env tok
    )
  | `Char_lit x -> R.Case ("Char_lit",
      map_character_literal env x
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

let map_literal (env : env) (x : CST.literal) =
  (match x with
  | `Null_lit tok -> R.Case ("Null_lit",
      (* "null" *) token env tok
    )
  | `Char_lit x -> R.Case ("Char_lit",
      map_character_literal env x
    )
  | `Int_lit tok -> R.Case ("Int_lit",
      (* integer_literal *) token env tok
    )
  | `Real_lit tok -> R.Case ("Real_lit",
      (* real_literal *) token env tok
    )
  | `Bool_lit x -> R.Case ("Bool_lit",
      map_boolean_literal env x
    )
  | `Str_lit x -> R.Case ("Str_lit",
      map_string_literal env x
    )
  | `Verb_str_lit tok -> R.Case ("Verb_str_lit",
      (* verbatim_string_literal *) token env tok
    )
  | `Raw_str_lit (v1, v2, v3, v4) -> R.Case ("Raw_str_lit",
      let v1 = (* raw_string_start *) token env v1 in
      let v2 = (* raw_string_content *) token env v2 in
      let v3 = (* raw_string_end *) token env v3 in
      let v4 =
        (match v4 with
        | Some x -> R.Option (Some (
            map_pat_c1fe926 env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3; v4]
    )
  )

let map_calling_convention (env : env) (x : CST.calling_convention) =
  (match x with
  | `Mana tok -> R.Case ("Mana",
      (* "managed" *) token env tok
    )
  | `Unma_opt_LBRACK_choice_Cdecl_rep_COMMA_choice_Cdecl_RBRACK (v1, v2) -> R.Case ("Unma_opt_LBRACK_choice_Cdecl_rep_COMMA_choice_Cdecl_RBRACK",
      let v1 = (* "unmanaged" *) token env v1 in
      let v2 =
        (match v2 with
        | Some (v1, v2, v3, v4) -> R.Option (Some (
            let v1 = (* "[" *) token env v1 in
            let v2 = map_anon_choice_Cdecl_63b6085 env v2 in
            let v3 =
              R.List (List.map (fun (v1, v2) ->
                let v1 = (* "," *) token env v1 in
                let v2 = map_anon_choice_Cdecl_63b6085 env v2 in
                R.Tuple [v1; v2]
              ) v3)
            in
            let v4 = (* "]" *) token env v4 in
            R.Tuple [v1; v2; v3; v4]
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2]
    )
  )

let map_anon_choice_id_bf14316 (env : env) (x : CST.anon_choice_id_bf14316) =
  (match x with
  | `Id x -> R.Case ("Id",
      map_identifier env x
    )
  | `Tuple_pat x -> R.Case ("Tuple_pat",
      map_tuple_pattern env x
    )
  )

let rec map_accessor_declaration (env : env) ((v1, v2, v3, v4) : CST.accessor_declaration) =
  let v1 = R.List (List.map (map_attribute_list_ env) v1) in
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
        map_identifier env x
      )
    )
  in
  let v4 = map_function_body env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_accessor_list (env : env) ((v1, v2, v3) : CST.accessor_list) =
  let v1 = (* "{" *) token env v1 in
  let v2 =
    R.List (List.map (map_accessor_declaration env) v2)
  in
  let v3 = (* "}" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_address_of_expression (env : env) ((v1, v2) : CST.address_of_expression) =
  let v1 = (* "&" *) token env v1 in
  let v2 = map_lvalue_expression env v2 in
  R.Tuple [v1; v2]

and map_anon_choice_choice_top_level_item_no_stmt_485bee8 (env : env) (x : CST.anon_choice_choice_top_level_item_no_stmt_485bee8) =
  (match x with
  | `Choice_choice_extern_alias_dire x -> R.Case ("Choice_choice_extern_alias_dire",
      map_anon_choice_top_level_item_no_stmt_65e2afd env x
    )
  | `Igno_dire x -> R.Case ("Igno_dire",
      map_ignored_directive env x
    )
  )

and map_anon_choice_exp_elem_3bb8381 (env : env) (x : CST.anon_choice_exp_elem_3bb8381) =
  (match x with
  | `Exp x -> R.Case ("Exp",
      map_expression_element env x
    )
  | `Pred_type tok -> R.Case ("Pred_type",
      (* predefined_type *) token env tok
    )
  | `Name x -> R.Case ("Name",
      map_name env x
    )
  )

and map_anon_choice_param_ce11a32 (env : env) (x : CST.anon_choice_param_ce11a32) =
  (match x with
  | `Param x -> R.Case ("Param",
      map_parameter env x
    )
  | `Param_array (v1, v2, v3, v4) -> R.Case ("Param_array",
      let v1 = R.List (List.map (map_attribute_list_ env) v1) in
      let v2 = (* "params" *) token env v2 in
      let v3 = map_type_pattern env v3 in
      let v4 = map_identifier env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  )

and map_anon_choice_param_rep_COMMA_choice_param_6325c4d (env : env) ((v1, v2) : CST.anon_choice_param_rep_COMMA_choice_param_6325c4d) =
  let v1 = map_anon_choice_param_ce11a32 env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_anon_choice_param_ce11a32 env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  R.Tuple [v1; v2]

and map_anon_choice_pat_dee696d (env : env) (x : CST.anon_choice_pat_dee696d) =
  (match x with
  | `Pat x -> R.Case ("Pat",
      map_pattern env x
    )
  | `Slice_pat x -> R.Case ("Slice_pat",
      map_slice_pattern env x
    )
  | `Ellips tok -> R.Case ("Ellips",
      (* "..." *) token env tok
    )
  )

and map_anon_choice_prep_else_8b52b0f (env : env) (x : CST.anon_choice_prep_else_8b52b0f) =
  (match x with
  | `Prep_else (v1, v2) -> R.Case ("Prep_else",
      let v1 = map_pat_56631e5 env v1 in
      let v2 = R.List (List.map (map_declaration env) v2) in
      R.Tuple [v1; v2]
    )
  | `Prep_elif (v1, v2, v3, v4, v5) -> R.Case ("Prep_elif",
      let v1 = map_pat_bfeb4bb env v1 in
      let v2 = map_preproc_expression env v2 in
      let v3 = map_pat_1d78758 env v3 in
      let v4 = R.List (List.map (map_declaration env) v4) in
      let v5 =
        (match v5 with
        | Some x -> R.Option (Some (
            map_anon_choice_prep_else_8b52b0f env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  )

and map_anon_choice_prep_else_in_attr_list_44dddc1 (env : env) (x : CST.anon_choice_prep_else_in_attr_list_44dddc1) =
  (match x with
  | `Prep_else_in_attr_list (v1, v2) -> R.Case ("Prep_else_in_attr_list",
      let v1 = map_pat_56631e5 env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_attribute_list env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2]
    )
  | `Prep_elif_in_attr_list (v1, v2, v3, v4, v5) -> R.Case ("Prep_elif_in_attr_list",
      let v1 = map_pat_bfeb4bb env v1 in
      let v2 = map_preproc_expression env v2 in
      let v3 = map_pat_1d78758 env v3 in
      let v4 =
        (match v4 with
        | Some x -> R.Option (Some (
            map_attribute_list env x
          ))
        | None -> R.Option None)
      in
      let v5 =
        (match v5 with
        | Some x -> R.Option (Some (
            map_anon_choice_prep_else_in_attr_list_44dddc1 env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  )

and map_anon_choice_prep_else_in_coll_8cdb09f (env : env) (x : CST.anon_choice_prep_else_in_coll_8cdb09f) =
  (match x with
  | `Prep_else_in_coll (v1, v2) -> R.Case ("Prep_else_in_coll",
      let v1 = map_pat_56631e5 env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_collection_items env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2]
    )
  | `Prep_elif_in_coll (v1, v2, v3, v4, v5) -> R.Case ("Prep_elif_in_coll",
      let v1 = map_pat_bfeb4bb env v1 in
      let v2 = map_preproc_expression env v2 in
      let v3 = map_pat_1d78758 env v3 in
      let v4 =
        (match v4 with
        | Some x -> R.Option (Some (
            map_collection_items env x
          ))
        | None -> R.Option None)
      in
      let v5 =
        (match v5 with
        | Some x -> R.Option (Some (
            map_anon_choice_prep_else_in_coll_8cdb09f env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  )

and map_anon_choice_prep_else_in_else_e2f3ee0 (env : env) (x : CST.anon_choice_prep_else_in_else_e2f3ee0) =
  (match x with
  | `Prep_else_in_else (v1, v2) -> R.Case ("Prep_else_in_else",
      let v1 = map_pat_56631e5 env v1 in
      let v2 =
        (match v2 with
        | Some (v1, v2) -> R.Option (Some (
            let v1 = (* "else" *) token env v1 in
            let v2 = map_global_statement env v2 in
            R.Tuple [v1; v2]
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2]
    )
  | `Prep_elif_in_else (v1, v2, v3, v4, v5) -> R.Case ("Prep_elif_in_else",
      let v1 = map_pat_bfeb4bb env v1 in
      let v2 = map_preproc_expression env v2 in
      let v3 = map_pat_1d78758 env v3 in
      let v4 =
        (match v4 with
        | Some (v1, v2) -> R.Option (Some (
            let v1 = (* "else" *) token env v1 in
            let v2 = map_global_statement env v2 in
            R.Tuple [v1; v2]
          ))
        | None -> R.Option None)
      in
      let v5 =
        (match v5 with
        | Some x -> R.Option (Some (
            map_anon_choice_prep_else_in_else_e2f3ee0 env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  )

and map_anon_choice_prep_else_in_enum_member_decl_ef97f12 (env : env) (x : CST.anon_choice_prep_else_in_enum_member_decl_ef97f12) =
  (match x with
  | `Prep_else_in_enum_member_decl (v1, v2) -> R.Case ("Prep_else_in_enum_member_decl",
      let v1 = map_pat_56631e5 env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_enum_member_items env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2]
    )
  | `Prep_elif_in_enum_member_decl (v1, v2, v3, v4, v5) -> R.Case ("Prep_elif_in_enum_member_decl",
      let v1 = map_pat_bfeb4bb env v1 in
      let v2 = map_preproc_expression env v2 in
      let v3 = map_pat_1d78758 env v3 in
      let v4 =
        (match v4 with
        | Some x -> R.Option (Some (
            map_enum_member_items env x
          ))
        | None -> R.Option None)
      in
      let v5 =
        (match v5 with
        | Some x -> R.Option (Some (
            map_anon_choice_prep_else_in_enum_member_decl_ef97f12 env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  )

and map_anon_choice_prep_else_in_exp_678167b (env : env) (x : CST.anon_choice_prep_else_in_exp_678167b) =
  (match x with
  | `Prep_else_in_exp (v1, v2) -> R.Case ("Prep_else_in_exp",
      let v1 = map_pat_56631e5 env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_expression_element env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2]
    )
  | `Prep_elif_in_exp (v1, v2, v3, v4, v5) -> R.Case ("Prep_elif_in_exp",
      let v1 = map_pat_bfeb4bb env v1 in
      let v2 = map_preproc_expression env v2 in
      let v3 = map_pat_1d78758 env v3 in
      let v4 =
        (match v4 with
        | Some x -> R.Option (Some (
            map_expression_element env x
          ))
        | None -> R.Option None)
      in
      let v5 =
        (match v5 with
        | Some x -> R.Option (Some (
            map_anon_choice_prep_else_in_exp_678167b env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  )

and map_anon_choice_prep_else_in_func_body_66df6e1 (env : env) (x : CST.anon_choice_prep_else_in_func_body_66df6e1) =
  (match x with
  | `Prep_else_in_func_body (v1, v2) -> R.Case ("Prep_else_in_func_body",
      let v1 = map_pat_56631e5 env v1 in
      let v2 =
        map_anon_opt_rep_type_param_consts_clause_plain_func_body_888cfe6 env v2
      in
      R.Tuple [v1; v2]
    )
  | `Prep_elif_in_func_body (v1, v2, v3, v4, v5) -> R.Case ("Prep_elif_in_func_body",
      let v1 = map_pat_bfeb4bb env v1 in
      let v2 = map_preproc_expression env v2 in
      let v3 = map_pat_1d78758 env v3 in
      let v4 =
        map_anon_opt_rep_type_param_consts_clause_plain_func_body_888cfe6 env v4
      in
      let v5 =
        (match v5 with
        | Some x -> R.Option (Some (
            map_anon_choice_prep_else_in_func_body_66df6e1 env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  )

and map_anon_choice_prep_else_in_init_085a8ad (env : env) (x : CST.anon_choice_prep_else_in_init_085a8ad) =
  (match x with
  | `Prep_else_in_init (v1, v2) -> R.Case ("Prep_else_in_init",
      let v1 = map_pat_56631e5 env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_initializer_items env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2]
    )
  | `Prep_elif_in_init (v1, v2, v3, v4, v5) -> R.Case ("Prep_elif_in_init",
      let v1 = map_pat_bfeb4bb env v1 in
      let v2 = map_preproc_expression env v2 in
      let v3 = map_pat_1d78758 env v3 in
      let v4 =
        (match v4 with
        | Some x -> R.Option (Some (
            map_initializer_items env x
          ))
        | None -> R.Option None)
      in
      let v5 =
        (match v5 with
        | Some x -> R.Option (Some (
            map_anon_choice_prep_else_in_init_085a8ad env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  )

and map_anon_choice_prep_else_in_stmt_tail_95f56b9 (env : env) (x : CST.anon_choice_prep_else_in_stmt_tail_95f56b9) =
  (match x with
  | `Prep_else_in_stmt_tail (v1, v2) -> R.Case ("Prep_else_in_stmt_tail",
      let v1 = map_pat_56631e5 env v1 in
      let v2 =
        (match v2 with
        | Some (v1, v2) -> R.Option (Some (
            let v1 = map_expression_element env v1 in
            let v2 = (* ";" *) token env v2 in
            R.Tuple [v1; v2]
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2]
    )
  | `Prep_elif_in_stmt_tail (v1, v2, v3, v4, v5) -> R.Case ("Prep_elif_in_stmt_tail",
      let v1 = map_pat_bfeb4bb env v1 in
      let v2 = map_preproc_expression env v2 in
      let v3 = map_pat_1d78758 env v3 in
      let v4 =
        (match v4 with
        | Some (v1, v2) -> R.Option (Some (
            let v1 = map_expression_element env v1 in
            let v2 = (* ";" *) token env v2 in
            R.Tuple [v1; v2]
          ))
        | None -> R.Option None)
      in
      let v5 =
        (match v5 with
        | Some x -> R.Option (Some (
            map_anon_choice_prep_else_in_stmt_tail_95f56b9 env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  )

and map_anon_choice_prep_else_in_top_level_0825411 (env : env) (x : CST.anon_choice_prep_else_in_top_level_0825411) =
  (match x with
  | `Prep_else_in_top_level (v1, v2) -> R.Case ("Prep_else_in_top_level",
      let v1 = map_pat_56631e5 env v1 in
      let v2 =
        R.List (List.map (map_anon_choice_choice_top_level_item_no_stmt_485bee8 env) v2)
      in
      R.Tuple [v1; v2]
    )
  | `Prep_elif_in_top_level (v1, v2, v3, v4, v5) -> R.Case ("Prep_elif_in_top_level",
      let v1 = map_pat_bfeb4bb env v1 in
      let v2 = map_preproc_expression env v2 in
      let v3 = map_pat_1d78758 env v3 in
      let v4 =
        R.List (List.map (map_anon_choice_choice_top_level_item_no_stmt_485bee8 env) v4)
      in
      let v5 =
        (match v5 with
        | Some x -> R.Option (Some (
            map_anon_choice_prep_else_in_top_level_0825411 env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  )

and map_anon_choice_top_level_item_no_stmt_65e2afd (env : env) (x : CST.anon_choice_top_level_item_no_stmt_65e2afd) =
  (match x with
  | `Choice_extern_alias_dire x -> R.Case ("Choice_extern_alias_dire",
      map_top_level_item_no_statement env x
    )
  | `Stmt x -> R.Case ("Stmt",
      map_global_statement env x
    )
  )

and map_anon_choice_type_param_list_94781cc (env : env) (x : CST.anon_choice_type_param_list_94781cc) =
  (match x with
  | `Type_param_list x -> R.Case ("Type_param_list",
      map_type_parameter_list env x
    )
  | `Param_list x -> R.Case ("Param_list",
      map_parameter_list env x
    )
  | `Base_list x -> R.Case ("Base_list",
      map_base_list env x
    )
  )

and map_anon_opt_rep_type_param_consts_clause_plain_func_body_888cfe6 (env : env) (opt : CST.anon_opt_rep_type_param_consts_clause_plain_func_body_888cfe6) =
  (match opt with
  | Some (v1, v2) -> R.Option (Some (
      let v1 =
        R.List (List.map (map_type_parameter_constraints_clause env) v1)
      in
      let v2 = map_plain_function_body env v2 in
      R.Tuple [v1; v2]
    ))
  | None -> R.Option None)

and map_anon_subp_rep_COMMA_subp_300d2c5 (env : env) ((v1, v2) : CST.anon_subp_rep_COMMA_subp_300d2c5) =
  let v1 = map_subpattern env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_subpattern env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  R.Tuple [v1; v2]

and map_anonymous_object_member_declarator (env : env) (x : CST.anonymous_object_member_declarator) =
  (match x with
  | `Id_EQ_exp x -> R.Case ("Id_EQ_exp",
      map_with_initializer env x
    )
  | `Exp x -> R.Case ("Exp",
      map_expression_element env x
    )
  )

and map_argument (env : env) (x : CST.argument) =
  (match x with
  | `Opt_id_COLON_opt_choice_ref_choice_exp (v1, v2, v3) -> R.Case ("Opt_id_COLON_opt_choice_ref_choice_exp",
      let v1 =
        (match v1 with
        | Some (v1, v2) -> R.Option (Some (
            let v1 = map_identifier env v1 in
            let v2 = (* ":" *) token env v2 in
            R.Tuple [v1; v2]
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
            map_expression_element env x
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
  | `Name x -> R.Case ("Name",
      map_name env x
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
              map_expression_element env x
            ))
          | None -> R.Option None)
        in
        let v2 =
          R.List (List.map (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 =
              (match v2 with
              | Some x -> R.Option (Some (
                  map_expression_element env x
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
  let v2 = map_expression_element env v2 in
  R.Tuple [v1; v2]

and map_attribute (env : env) ((v1, v2) : CST.attribute) =
  let v1 = map_name env v1 in
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
    | Some (v1, v2) -> R.Option (Some (
        let v1 = map_identifier env v1 in
        let v2 =
          (match v2 with
          | `COLON tok -> R.Case ("COLON",
              (* ":" *) token env tok
            )
          | `EQ tok -> R.Case ("EQ",
              (* "=" *) token env tok
            )
          )
        in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  let v2 = map_expression_element env v2 in
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

and map_attribute_list_ (env : env) (x : CST.attribute_list_) =
  (match x with
  | `Attr_list x -> R.Case ("Attr_list",
      map_attribute_list env x
    )
  | `Prep_if_in_attr_list (v1, v2, v3, v4, v5, v6) -> R.Case ("Prep_if_in_attr_list",
      let v1 = map_pat_3df6e71 env v1 in
      let v2 = map_preproc_expression env v2 in
      let v3 = map_pat_1d78758 env v3 in
      let v4 =
        (match v4 with
        | Some x -> R.Option (Some (
            map_attribute_list env x
          ))
        | None -> R.Option None)
      in
      let v5 =
        (match v5 with
        | Some x -> R.Option (Some (
            map_anon_choice_prep_else_in_attr_list_44dddc1 env x
          ))
        | None -> R.Option None)
      in
      let v6 = map_pat_c46d1b2 env v6 in
      R.Tuple [v1; v2; v3; v4; v5; v6]
    )
  )

and map_base_list (env : env) ((v1, v2, v3, v4) : CST.base_list) =
  let v1 = (* ":" *) token env v1 in
  let v2 = map_type_pattern env v2 in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_argument_list env x
      ))
    | None -> R.Option None)
  in
  let v4 =
    R.List (List.map (fun (v1, v2, v3) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_type_pattern env v2 in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_argument_list env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3]
    ) v4)
  in
  R.Tuple [v1; v2; v3; v4]

and map_binary_expression (env : env) (x : CST.binary_expression) =
  (match x with
  | `Exp_AMPAMP_exp (v1, v2, v3) -> R.Case ("Exp_AMPAMP_exp",
      let v1 = map_expression_element env v1 in
      let v2 = (* "&&" *) token env v2 in
      let v3 = map_expression_element env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_BARBAR_exp (v1, v2, v3) -> R.Case ("Exp_BARBAR_exp",
      let v1 = map_expression_element env v1 in
      let v2 = (* "||" *) token env v2 in
      let v3 = map_expression_element env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_GTGT_exp (v1, v2, v3) -> R.Case ("Exp_GTGT_exp",
      let v1 = map_expression_element env v1 in
      let v2 = (* ">>" *) token env v2 in
      let v3 = map_expression_element env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_GTGTGT_exp (v1, v2, v3) -> R.Case ("Exp_GTGTGT_exp",
      let v1 = map_expression_element env v1 in
      let v2 = (* ">>>" *) token env v2 in
      let v3 = map_expression_element env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_LTLT_exp (v1, v2, v3) -> R.Case ("Exp_LTLT_exp",
      let v1 = map_expression_element env v1 in
      let v2 = (* "<<" *) token env v2 in
      let v3 = map_expression_element env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_AMP_exp (v1, v2, v3) -> R.Case ("Exp_AMP_exp",
      let v1 = map_expression_element env v1 in
      let v2 = (* "&" *) token env v2 in
      let v3 = map_expression_element env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_HAT_exp (v1, v2, v3) -> R.Case ("Exp_HAT_exp",
      let v1 = map_expression_element env v1 in
      let v2 = (* "^" *) token env v2 in
      let v3 = map_expression_element env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_BAR_exp (v1, v2, v3) -> R.Case ("Exp_BAR_exp",
      let v1 = map_expression_element env v1 in
      let v2 = (* "|" *) token env v2 in
      let v3 = map_expression_element env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_PLUS_exp (v1, v2, v3) -> R.Case ("Exp_PLUS_exp",
      let v1 = map_expression_element env v1 in
      let v2 = (* "+" *) token env v2 in
      let v3 = map_expression_element env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_DASH_exp (v1, v2, v3) -> R.Case ("Exp_DASH_exp",
      let v1 = map_expression_element env v1 in
      let v2 = (* "-" *) token env v2 in
      let v3 = map_expression_element env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_STAR_exp (v1, v2, v3) -> R.Case ("Exp_STAR_exp",
      let v1 = map_expression_element env v1 in
      let v2 = (* "*" *) token env v2 in
      let v3 = map_expression_element env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_SLASH_exp (v1, v2, v3) -> R.Case ("Exp_SLASH_exp",
      let v1 = map_expression_element env v1 in
      let v2 = (* "/" *) token env v2 in
      let v3 = map_expression_element env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_PERC_exp (v1, v2, v3) -> R.Case ("Exp_PERC_exp",
      let v1 = map_expression_element env v1 in
      let v2 = (* "%" *) token env v2 in
      let v3 = map_expression_element env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_LT_exp (v1, v2, v3) -> R.Case ("Exp_LT_exp",
      let v1 = map_expression_element env v1 in
      let v2 = (* "<" *) token env v2 in
      let v3 = map_expression_element env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_LTEQ_exp (v1, v2, v3) -> R.Case ("Exp_LTEQ_exp",
      let v1 = map_expression_element env v1 in
      let v2 = (* "<=" *) token env v2 in
      let v3 = map_expression_element env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_EQEQ_exp (v1, v2, v3) -> R.Case ("Exp_EQEQ_exp",
      let v1 = map_expression_element env v1 in
      let v2 = (* "==" *) token env v2 in
      let v3 = map_expression_element env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_BANGEQ_exp (v1, v2, v3) -> R.Case ("Exp_BANGEQ_exp",
      let v1 = map_expression_element env v1 in
      let v2 = (* "!=" *) token env v2 in
      let v3 = map_expression_element env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_GTEQ_exp (v1, v2, v3) -> R.Case ("Exp_GTEQ_exp",
      let v1 = map_expression_element env v1 in
      let v2 = (* ">=" *) token env v2 in
      let v3 = map_expression_element env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_GT_exp (v1, v2, v3) -> R.Case ("Exp_GT_exp",
      let v1 = map_expression_element env v1 in
      let v2 = (* ">" *) token env v2 in
      let v3 = map_expression_element env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_QMARKQMARK_exp (v1, v2, v3) -> R.Case ("Exp_QMARKQMARK_exp",
      let v1 = map_expression_element env v1 in
      let v2 = (* "??" *) token env v2 in
      let v3 = map_expression_element env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_block (env : env) ((v1, v2, v3) : CST.block) =
  let v1 = (* "{" *) token env v1 in
  let v2 = R.List (List.map (map_global_statement env) v2) in
  let v3 = (* "}" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_bracketed_argument_list (env : env) ((v1, v2, v3, v4, v5) : CST.bracketed_argument_list) =
  let v1 = (* "[" *) token env v1 in
  let v2 = map_argument env v2 in
  let v3 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_argument env v2 in
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

and map_bracketed_parameter_list (env : env) ((v1, v2, v3) : CST.bracketed_parameter_list) =
  let v1 = (* "[" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_anon_choice_param_rep_COMMA_choice_param_6325c4d env x
      ))
    | None -> R.Option None)
  in
  let v3 = (* "]" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_cast_expression (env : env) ((v1, v2, v3, v4) : CST.cast_expression) =
  let v1 = (* "(" *) token env v1 in
  let v2 = map_type_pattern env v2 in
  let v3 = (* ")" *) token env v3 in
  let v4 = map_expression_element env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_catch_clause (env : env) ((v1, v2, v3) : CST.catch_clause) =
  let v1 = (* "catch" *) token env v1 in
  let v2 =
    R.List (List.map (fun x ->
      (match x with
      | `Catch_decl x -> R.Case ("Catch_decl",
          map_catch_declaration env x
        )
      | `Catch_filter_clause x -> R.Case ("Catch_filter_clause",
          map_catch_filter_clause env x
        )
      )
    ) v2)
  in
  let v3 = map_block env v3 in
  R.Tuple [v1; v2; v3]

and map_catch_declaration (env : env) ((v1, v2, v3, v4) : CST.catch_declaration) =
  let v1 = (* "(" *) token env v1 in
  let v2 = map_type_pattern env v2 in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_identifier env x
      ))
    | None -> R.Option None)
  in
  let v4 = (* ")" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_catch_filter_clause (env : env) ((v1, v2, v3, v4) : CST.catch_filter_clause) =
  let v1 = (* "when" *) token env v1 in
  let v2 = (* "(" *) token env v2 in
  let v3 = map_expression_element env v3 in
  let v4 = (* ")" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_checked_expression (env : env) ((v1, v2, v3, v4) : CST.checked_expression) =
  let v1 = map_anon_choice_chec_7987657 env v1 in
  let v2 = (* "(" *) token env v2 in
  let v3 = map_expression_element env v3 in
  let v4 = (* ")" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_class_declaration (env : env) ((v1, v2) : CST.class_declaration) =
  let v1 = map_class_declaration_initializer env v1 in
  let v2 = map_declaration_list_body env v2 in
  R.Tuple [v1; v2]

and map_class_declaration_initializer (env : env) ((v1, v2, v3, v4, v5, v6) : CST.class_declaration_initializer) =
  let v1 = R.List (List.map (map_attribute_list_ env) v1) in
  let v2 = R.List (List.map (map_modifier env) v2) in
  let v3 = (* "class" *) token env v3 in
  let v4 = map_identifier env v4 in
  let v5 =
    R.List (List.map (map_anon_choice_type_param_list_94781cc env) v5)
  in
  let v6 =
    R.List (List.map (map_type_parameter_constraints_clause env) v6)
  in
  R.Tuple [v1; v2; v3; v4; v5; v6]

and map_collection_element (env : env) (x : CST.collection_element) =
  (match x with
  | `Exp_elem x -> R.Case ("Exp_elem",
      map_expression_element env x
    )
  | `Spread_elem (v1, v2) -> R.Case ("Spread_elem",
      let v1 = (* ".." *) token env v1 in
      let v2 = map_expression_element env v2 in
      R.Tuple [v1; v2]
    )
  | `Semg_vari_meta tok -> R.Case ("Semg_vari_meta",
      (* pattern \$\.\.\.[A-Z_][A-Z_0-9]* *) token env tok
    )
  )

and map_collection_items (env : env) (x : CST.collection_items) =
  (match x with
  | `Rep1_choice_coll_elem_COMMA_opt_coll_elem (v1, v2) -> R.Case ("Rep1_choice_coll_elem_COMMA_opt_coll_elem",
      let v1 =
        R.List (List.map (fun x ->
          (match x with
          | `Coll_elem_COMMA (v1, v2) -> R.Case ("Coll_elem_COMMA",
              let v1 = map_collection_element env v1 in
              let v2 = (* "," *) token env v2 in
              R.Tuple [v1; v2]
            )
          | `Prep_if_in_coll x -> R.Case ("Prep_if_in_coll",
              map_preproc_if_in_collection env x
            )
          )
        ) v1)
      in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_collection_element env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2]
    )
  | `Coll_elem x -> R.Case ("Coll_elem",
      map_collection_element env x
    )
  )

and map_conditional_access_expression (env : env) ((v1, v2, v3) : CST.conditional_access_expression) =
  let v1 = map_expression_element env v1 in
  let v2 = (* "?" *) token env v2 in
  let v3 =
    (match v3 with
    | `Member_bind_exp x -> R.Case ("Member_bind_exp",
        map_member_binding_expression env x
      )
    | `Brac_arg_list x -> R.Case ("Brac_arg_list",
        map_bracketed_argument_list env x
      )
    )
  in
  R.Tuple [v1; v2; v3]

and map_conditional_ending_in_preproc (env : env) ((v1, v2, v3, v4, v5) : CST.conditional_ending_in_preproc) =
  let v1 = map_expression_element env v1 in
  let v2 = (* "?" *) token env v2 in
  let v3 = map_expression_element env v3 in
  let v4 = (* ":" *) token env v4 in
  let v5 = map_preproc_if_in_statement_tail env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

and map_constant_pattern (env : env) (x : CST.constant_pattern) =
  (match x with
  | `Bin_exp x -> R.Case ("Bin_exp",
      map_binary_expression env x
    )
  | `Chec_exp x -> R.Case ("Chec_exp",
      map_checked_expression env x
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
  | `Sizeof_exp x -> R.Case ("Sizeof_exp",
      map_sizeof_expression env x
    )
  | `Tuple_exp x -> R.Case ("Tuple_exp",
      map_tuple_expression env x
    )
  | `Typeof_exp x -> R.Case ("Typeof_exp",
      map_typeof_expression env x
    )
  | `Member_access_exp x -> R.Case ("Member_access_exp",
      map_member_access_expression env x
    )
  | `Name_invo_pat (v1, v2) -> R.Case ("Name_invo_pat",
      let v1 = map_name env v1 in
      let v2 = map_argument_list env v2 in
      R.Tuple [v1; v2]
    )
  | `Comp_invo_exp (v1, v2) -> R.Case ("Comp_invo_exp",
      let v1 =
        (match v1 with
        | `Member_access_exp x -> R.Case ("Member_access_exp",
            map_member_access_expression env x
          )
        | `Elem_access_exp x -> R.Case ("Elem_access_exp",
            map_element_access_expression env x
          )
        | `Invo_exp x -> R.Case ("Invo_exp",
            map_invocation_expression env x
          )
        | `Paren_exp x -> R.Case ("Paren_exp",
            map_parenthesized_expression env x
          )
        | `Cond_access_exp x -> R.Case ("Cond_access_exp",
            map_conditional_access_expression env x
          )
        | `Cast_exp x -> R.Case ("Cast_exp",
            map_cast_expression env x
          )
        )
      in
      let v2 = map_argument_list env v2 in
      R.Tuple [v1; v2]
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

and map_constructor_declaration_initializer (env : env) ((v1, v2, v3, v4, v5) : CST.constructor_declaration_initializer) =
  let v1 = R.List (List.map (map_attribute_list_ env) v1) in
  let v2 = R.List (List.map (map_modifier env) v2) in
  let v3 = map_identifier env v3 in
  let v4 = map_parameter_list env v4 in
  let v5 =
    (match v5 with
    | Some x -> R.Option (Some (
        map_constructor_initializer env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3; v4; v5]

and map_constructor_initializer (env : env) ((v1, v2, v3) : CST.constructor_initializer) =
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

and map_declaration (env : env) (x : CST.declaration) =
  (match x with
  | `Class_decl x -> R.Case ("Class_decl",
      map_class_declaration env x
    )
  | `Struct_decl x -> R.Case ("Struct_decl",
      map_struct_declaration env x
    )
  | `Enum_decl x -> R.Case ("Enum_decl",
      map_enum_declaration env x
    )
  | `Dele_decl x -> R.Case ("Dele_decl",
      map_delegate_declaration env x
    )
  | `Field_decl (v1, v2, v3, v4) -> R.Case ("Field_decl",
      let v1 = R.List (List.map (map_attribute_list_ env) v1) in
      let v2 = R.List (List.map (map_modifier env) v2) in
      let v3 = map_variable_declaration env v3 in
      let v4 = (* ";" *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Meth_decl x -> R.Case ("Meth_decl",
      map_method_declaration env x
    )
  | `Event_decl (v1, v2, v3, v4, v5, v6, v7) -> R.Case ("Event_decl",
      let v1 = R.List (List.map (map_attribute_list_ env) v1) in
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
      let v6 = map_identifier env v6 in
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
      let v1 = R.List (List.map (map_attribute_list_ env) v1) in
      let v2 = R.List (List.map (map_modifier env) v2) in
      let v3 = (* "event" *) token env v3 in
      let v4 = map_variable_declaration env v4 in
      let v5 = (* ";" *) token env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `Record_decl x -> R.Case ("Record_decl",
      map_record_declaration env x
    )
  | `Cons_decl (v1, v2) -> R.Case ("Cons_decl",
      let v1 = map_constructor_declaration_initializer env v1 in
      let v2 = map_function_body env v2 in
      R.Tuple [v1; v2]
    )
  | `Dest_decl (v1, v2, v3, v4, v5, v6) -> R.Case ("Dest_decl",
      let v1 = R.List (List.map (map_attribute_list_ env) v1) in
      let v2 =
        (match v2 with
        | Some tok -> R.Option (Some (
            (* "extern" *) token env tok
          ))
        | None -> R.Option None)
      in
      let v3 = (* "~" *) token env v3 in
      let v4 = map_identifier env v4 in
      let v5 = map_parameter_list env v5 in
      let v6 = map_function_body env v6 in
      R.Tuple [v1; v2; v3; v4; v5; v6]
    )
  | `Inde_decl (v1, v2, v3, v4, v5, v6, v7) -> R.Case ("Inde_decl",
      let v1 = R.List (List.map (map_attribute_list_ env) v1) in
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
  | `Name_decl x -> R.Case ("Name_decl",
      map_namespace_declaration env x
    )
  | `Op_decl x -> R.Case ("Op_decl",
      map_operator_declaration env x
    )
  | `Conv_op_decl (v1, v2, v3, v4, v5, v6, v7) -> R.Case ("Conv_op_decl",
      let v1 = R.List (List.map (map_attribute_list_ env) v1) in
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
        R.List (List.map (fun x ->
          (match x with
          | `Expl_inte_spec x -> R.Case ("Expl_inte_spec",
              map_explicit_interface_specifier env x
            )
          | `Op tok -> R.Case ("Op",
              (* "operator" *) token env tok
            )
          | `Chec tok -> R.Case ("Chec",
              (* "checked" *) token env tok
            )
          )
        ) v4)
      in
      let v5 = map_type_pattern env v5 in
      let v6 = map_parameter_list env v6 in
      let v7 = map_function_body env v7 in
      R.Tuple [v1; v2; v3; v4; v5; v6; v7]
    )
  | `Prop_decl x -> R.Case ("Prop_decl",
      map_property_declaration env x
    )
  | `Using_dire x -> R.Case ("Using_dire",
      map_using_directive env x
    )
  | `Prep_if (v1, v2, v3, v4, v5, v6) -> R.Case ("Prep_if",
      let v1 = map_pat_3df6e71 env v1 in
      let v2 = map_preproc_expression env v2 in
      let v3 = map_pat_1d78758 env v3 in
      let v4 = R.List (List.map (map_declaration env) v4) in
      let v5 =
        (match v5 with
        | Some x -> R.Option (Some (
            map_anon_choice_prep_else_8b52b0f env x
          ))
        | None -> R.Option None)
      in
      let v6 = map_pat_c46d1b2 env v6 in
      R.Tuple [v1; v2; v3; v4; v5; v6]
    )
  | `Exte_decl x -> R.Case ("Exte_decl",
      map_extension_declaration env x
    )
  | `Ellips tok -> R.Case ("Ellips",
      (* "..." *) token env tok
    )
  | `Igno_dire x -> R.Case ("Igno_dire",
      map_ignored_directive env x
    )
  )

and map_declaration_expression (env : env) ((v1, v2) : CST.declaration_expression) =
  let v1 = map_type_pattern env v1 in
  let v2 = map_identifier env v2 in
  R.Tuple [v1; v2]

and map_declaration_list (env : env) ((v1, v2, v3) : CST.declaration_list) =
  let v1 = (* "{" *) token env v1 in
  let v2 = R.List (List.map (map_declaration env) v2) in
  let v3 = (* "}" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_declaration_list_body (env : env) (x : CST.declaration_list_body) =
  (match x with
  | `Decl_list_opt_semi (v1, v2) -> R.Case ("Decl_list_opt_semi",
      let v1 = map_declaration_list env v1 in
      let v2 = (* optional_semi *) token env v2 in
      R.Tuple [v1; v2]
    )
  | `SEMI tok -> R.Case ("SEMI",
      (* ";" *) token env tok
    )
  )

and map_deep_ellipsis (env : env) ((v1, v2, v3) : CST.deep_ellipsis) =
  let v1 = (* "<..." *) token env v1 in
  let v2 = map_expression_element env v2 in
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

and map_delegate_declaration (env : env) ((v1, v2, v3) : CST.delegate_declaration) =
  let v1 = map_delegate_declaration_initializer env v1 in
  let v2 =
    R.List (List.map (map_type_parameter_constraints_clause env) v2)
  in
  let v3 = (* ";" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_delegate_declaration_initializer (env : env) ((v1, v2, v3, v4, v5, v6, v7) : CST.delegate_declaration_initializer) =
  let v1 = R.List (List.map (map_attribute_list_ env) v1) in
  let v2 = R.List (List.map (map_modifier env) v2) in
  let v3 = (* "delegate" *) token env v3 in
  let v4 = map_type_pattern env v4 in
  let v5 = map_identifier env v5 in
  let v6 =
    (match v6 with
    | Some x -> R.Option (Some (
        map_type_parameter_list env x
      ))
    | None -> R.Option None)
  in
  let v7 = map_parameter_list env v7 in
  R.Tuple [v1; v2; v3; v4; v5; v6; v7]

and map_element_access_expression (env : env) ((v1, v2) : CST.element_access_expression) =
  let v1 = map_expression_element env v1 in
  let v2 = map_bracketed_argument_list env v2 in
  R.Tuple [v1; v2]

and map_enum_declaration (env : env) ((v1, v2) : CST.enum_declaration) =
  let v1 = map_enum_declaration_initializer env v1 in
  let v2 =
    (match v2 with
    | `Enum_member_decl_list_opt_semi (v1, v2) -> R.Case ("Enum_member_decl_list_opt_semi",
        let v1 = map_enum_member_declaration_list env v1 in
        let v2 = (* optional_semi *) token env v2 in
        R.Tuple [v1; v2]
      )
    | `SEMI tok -> R.Case ("SEMI",
        (* ";" *) token env tok
      )
    )
  in
  R.Tuple [v1; v2]

and map_enum_declaration_initializer (env : env) ((v1, v2, v3, v4, v5) : CST.enum_declaration_initializer) =
  let v1 = R.List (List.map (map_attribute_list_ env) v1) in
  let v2 = R.List (List.map (map_modifier env) v2) in
  let v3 = (* "enum" *) token env v3 in
  let v4 = map_identifier env v4 in
  let v5 =
    (match v5 with
    | Some x -> R.Option (Some (
        map_base_list env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3; v4; v5]

and map_enum_member_declaration (env : env) (x : CST.enum_member_declaration) =
  (match x with
  | `Rep_attr_list__id_opt_EQ_exp (v1, v2, v3) -> R.Case ("Rep_attr_list__id_opt_EQ_exp",
      let v1 = R.List (List.map (map_attribute_list_ env) v1) in
      let v2 = map_identifier env v2 in
      let v3 =
        (match v3 with
        | Some (v1, v2) -> R.Option (Some (
            let v1 = (* "=" *) token env v1 in
            let v2 = map_expression_element env v2 in
            R.Tuple [v1; v2]
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3]
    )
  | `Ellips tok -> R.Case ("Ellips",
      (* "..." *) token env tok
    )
  )

and map_enum_member_declaration_list (env : env) ((v1, v2, v3) : CST.enum_member_declaration_list) =
  let v1 = (* "{" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_enum_member_items env x
      ))
    | None -> R.Option None)
  in
  let v3 = (* "}" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_enum_member_items (env : env) (xs : CST.enum_member_items) =
  R.List (List.map (fun x ->
    (match x with
    | `Enum_member_decl x -> R.Case ("Enum_member_decl",
        map_enum_member_declaration env x
      )
    | `COMMA tok -> R.Case ("COMMA",
        (* "," *) token env tok
      )
    | `Prep_if_in_enum_member_decl x -> R.Case ("Prep_if_in_enum_member_decl",
        map_preproc_if_in_enum_member_declaration env x
      )
    )
  ) xs)

and map_explicit_interface_specifier (env : env) ((v1, v2) : CST.explicit_interface_specifier) =
  let v1 = map_name env v1 in
  let v2 = (* "." *) token env v2 in
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

and map_expression_element (env : env) (x : CST.expression_element) =
  map_expression env x

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
      let v2 =
        (match v2 with
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
      in
      let v3 = map_expression_element env v3 in
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
      let v2 = map_expression_element env v2 in
      R.Tuple [v1; v2]
    )
  | `Obj_crea_exp (v1, v2, v3, v4) -> R.Case ("Obj_crea_exp",
      let v1 = (* "new" *) token env v1 in
      let v2 = map_type_pattern env v2 in
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

and map_extension_body (env : env) ((v1, v2, v3, v4) : CST.extension_body) =
  let v1 = (* "{" *) token env v1 in
  let v2 =
    R.List (List.map (fun x ->
      (match x with
      | `Exte_member_decl x -> R.Case ("Exte_member_decl",
          map_extension_member_declaration env x
        )
      | `Ellips tok -> R.Case ("Ellips",
          (* "..." *) token env tok
        )
      )
    ) v2)
  in
  let v3 = (* "}" *) token env v3 in
  let v4 =
    (match v4 with
    | Some tok -> R.Option (Some (
        (* ";" *) token env tok
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3; v4]

and map_extension_declaration (env : env) ((v1, v2, v3, v4, v5, v6, v7, v8) : CST.extension_declaration) =
  let v1 = R.List (List.map (map_attribute_list_ env) v1) in
  let v2 = (* "extension" *) token env v2 in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_type_parameter_list env x
      ))
    | None -> R.Option None)
  in
  let v4 = (* "(" *) token env v4 in
  let v5 = map_receiver_parameter env v5 in
  let v6 = (* ")" *) token env v6 in
  let v7 =
    R.List (List.map (map_type_parameter_constraints_clause env) v7)
  in
  let v8 = map_extension_body env v8 in
  R.Tuple [v1; v2; v3; v4; v5; v6; v7; v8]

and map_extension_member_declaration (env : env) (x : CST.extension_member_declaration) =
  (match x with
  | `Meth_decl x -> R.Case ("Meth_decl",
      map_method_declaration env x
    )
  | `Prop_decl x -> R.Case ("Prop_decl",
      map_property_declaration env x
    )
  | `Op_decl x -> R.Case ("Op_decl",
      map_operator_declaration env x
    )
  )

and map_finally_clause (env : env) ((v1, v2) : CST.finally_clause) =
  let v1 = (* "finally" *) token env v1 in
  let v2 = map_block env v2 in
  R.Tuple [v1; v2]

and map_for_statement_conditions (env : env) ((v1, v2, v3, v4, v5, v6, v7) : CST.for_statement_conditions) =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        (match x with
        | `Var_decl x -> R.Case ("Var_decl",
            map_variable_declaration env x
          )
        | `Exp_rep_COMMA_exp (v1, v2) -> R.Case ("Exp_rep_COMMA_exp",
            let v1 = map_expression_element env v1 in
            let v2 =
              R.List (List.map (map_interpolation_alignment_clause env) v2)
            in
            R.Tuple [v1; v2]
          )
        )
      ))
    | None -> R.Option None)
  in
  let v3 = (* ";" *) token env v3 in
  let v4 =
    (match v4 with
    | Some x -> R.Option (Some (
        map_expression_element env x
      ))
    | None -> R.Option None)
  in
  let v5 = (* ";" *) token env v5 in
  let v6 =
    (match v6 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = map_expression_element env v1 in
        let v2 =
          R.List (List.map (map_interpolation_alignment_clause env) v2)
        in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  let v7 = (* ")" *) token env v7 in
  R.Tuple [v1; v2; v3; v4; v5; v6; v7]

and map_foreach_statement_initializer (env : env) ((v1, v2, v3, v4, v5, v6, v7) : CST.foreach_statement_initializer) =
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
        let v2 = map_anon_choice_id_bf14316 env v2 in
        R.Tuple [v1; v2]
      )
    | `Exp x -> R.Case ("Exp",
        map_expression_element env x
      )
    )
  in
  let v5 = (* "in" *) token env v5 in
  let v6 = map_expression_element env v6 in
  let v7 = (* ")" *) token env v7 in
  R.Tuple [v1; v2; v3; v4; v5; v6; v7]

and map_from_clause (env : env) ((v1, v2, v3, v4, v5) : CST.from_clause) =
  let v1 = (* "from" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_type_pattern env x
      ))
    | None -> R.Option None)
  in
  let v3 = map_identifier env v3 in
  let v4 = (* "in" *) token env v4 in
  let v5 = map_expression_element env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

and map_function_body (env : env) (x : CST.function_body) =
  (match x with
  | `Plain_func_body x -> R.Case ("Plain_func_body",
      map_plain_function_body env x
    )
  | `Prep_if_in_func_body (v1, v2, v3, v4, v5, v6) -> R.Case ("Prep_if_in_func_body",
      let v1 = map_pat_3df6e71 env v1 in
      let v2 = map_preproc_expression env v2 in
      let v3 = map_pat_1d78758 env v3 in
      let v4 =
        map_anon_opt_rep_type_param_consts_clause_plain_func_body_888cfe6 env v4
      in
      let v5 =
        (match v5 with
        | Some x -> R.Option (Some (
            map_anon_choice_prep_else_in_func_body_66df6e1 env x
          ))
        | None -> R.Option None)
      in
      let v6 = map_pat_c46d1b2 env v6 in
      R.Tuple [v1; v2; v3; v4; v5; v6]
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

and map_function_pointer_type (env : env) ((v1, v2, v3, v4, v5, v6, v7) : CST.function_pointer_type) =
  let v1 = (* "delegate" *) token env v1 in
  let v2 = (* "*" *) token env v2 in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_calling_convention env x
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
  let v6 = map_type_pattern env v6 in
  let v7 = (* ">" *) token env v7 in
  R.Tuple [v1; v2; v3; v4; v5; v6; v7]

and map_global_statement (env : env) (x : CST.global_statement) =
  map_statement env x

and map_initializer_expression (env : env) ((v1, v2, v3) : CST.initializer_expression) =
  let v1 = (* "{" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_initializer_items env x
      ))
    | None -> R.Option None)
  in
  let v3 = (* "}" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_initializer_items (env : env) (x : CST.initializer_items) =
  (match x with
  | `Rep1_choice_exp_COMMA_opt_exp (v1, v2) -> R.Case ("Rep1_choice_exp_COMMA_opt_exp",
      let v1 =
        R.List (List.map (fun x ->
          (match x with
          | `Exp_COMMA (v1, v2) -> R.Case ("Exp_COMMA",
              let v1 = map_expression_element env v1 in
              let v2 = (* "," *) token env v2 in
              R.Tuple [v1; v2]
            )
          | `Prep_if_in_init x -> R.Case ("Prep_if_in_init",
              map_preproc_if_in_initializer env x
            )
          )
        ) v1)
      in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_expression_element env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2]
    )
  | `Exp x -> R.Case ("Exp",
      map_expression_element env x
    )
  )

and map_interface_declaration (env : env) ((v1, v2) : CST.interface_declaration) =
  let v1 = map_interface_declaration_initializer env v1 in
  let v2 = map_declaration_list_body env v2 in
  R.Tuple [v1; v2]

and map_interface_declaration_initializer (env : env) ((v1, v2, v3, v4, v5, v6, v7) : CST.interface_declaration_initializer) =
  let v1 = R.List (List.map (map_attribute_list_ env) v1) in
  let v2 = R.List (List.map (map_modifier env) v2) in
  let v3 = (* "interface" *) token env v3 in
  let v4 = map_identifier env v4 in
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
  R.Tuple [v1; v2; v3; v4; v5; v6; v7]

and map_interpolated_raw_string_content (env : env) (x : CST.interpolated_raw_string_content) =
  (match x with
  | `Interp_str_content tok -> R.Case ("Interp_str_content",
      (* interpolation_string_content *) token env tok
    )
  | `Interp x -> R.Case ("Interp",
      map_interpolation env x
    )
  )

and map_interpolated_string_content (env : env) (x : CST.interpolated_string_content) =
  (match x with
  | `Interp_str_content tok -> R.Case ("Interp_str_content",
      (* interpolation_string_content *) token env tok
    )
  | `Esc_seq x -> R.Case ("Esc_seq",
      map_escape_sequence env x
    )
  | `Interp x -> R.Case ("Interp",
      map_interpolation env x
    )
  )

and map_interpolated_string_expression (env : env) (x : CST.interpolated_string_expression) =
  (match x with
  | `Interp_regu_start_interp_start_quote_rep_inte_str_content_interp_end_quote (v1, v2, v3, v4) -> R.Case ("Interp_regu_start_interp_start_quote_rep_inte_str_content_interp_end_quote",
      let v1 = (* interpolation_regular_start *) token env v1 in
      let v2 = (* interpolation_start_quote *) token env v2 in
      let v3 =
        R.List (List.map (map_interpolated_string_content env) v3)
      in
      let v4 = (* interpolation_end_quote *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Interp_verb_start_interp_start_quote_rep_inte_verb_str_content_interp_end_quote (v1, v2, v3, v4) -> R.Case ("Interp_verb_start_interp_start_quote_rep_inte_verb_str_content_interp_end_quote",
      let v1 = (* interpolation_verbatim_start *) token env v1 in
      let v2 = (* interpolation_start_quote *) token env v2 in
      let v3 =
        R.List (List.map (map_interpolated_verbatim_string_content env) v3)
      in
      let v4 = (* interpolation_end_quote *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Interp_raw_start_interp_start_quote_rep_inte_raw_str_content_interp_end_quote (v1, v2, v3, v4) -> R.Case ("Interp_raw_start_interp_start_quote_rep_inte_raw_str_content_interp_end_quote",
      let v1 = (* interpolation_raw_start *) token env v1 in
      let v2 = (* interpolation_start_quote *) token env v2 in
      let v3 =
        R.List (List.map (map_interpolated_raw_string_content env) v3)
      in
      let v4 = (* interpolation_end_quote *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  )

and map_interpolated_verbatim_string_content (env : env) (x : CST.interpolated_verbatim_string_content) =
  (match x with
  | `Interp_str_content tok -> R.Case ("Interp_str_content",
      (* interpolation_string_content *) token env tok
    )
  | `Interp x -> R.Case ("Interp",
      map_interpolation env x
    )
  )

and map_interpolation (env : env) ((v1, v2, v3, v4, v5) : CST.interpolation) =
  let v1 = (* interpolation_open_brace *) token env v1 in
  let v2 = map_expression_element env v2 in
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
  let v5 = (* interpolation_close_brace *) token env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

and map_interpolation_alignment_clause (env : env) ((v1, v2) : CST.interpolation_alignment_clause) =
  let v1 = (* "," *) token env v1 in
  let v2 = map_expression_element env v2 in
  R.Tuple [v1; v2]

and map_invocation_expression (env : env) ((v1, v2) : CST.invocation_expression) =
  let v1 = map_expression_element env v1 in
  let v2 = map_argument_list env v2 in
  R.Tuple [v1; v2]

and map_join_body (env : env) ((v1, v2, v3, v4) : CST.join_body) =
  let v1 = (* "on" *) token env v1 in
  let v2 = map_expression_element env v2 in
  let v3 = (* "equals" *) token env v3 in
  let v4 = map_expression_element env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_join_header (env : env) ((v1, v2, v3, v4) : CST.join_header) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        map_type_pattern env x
      ))
    | None -> R.Option None)
  in
  let v2 = map_identifier env v2 in
  let v3 = (* "in" *) token env v3 in
  let v4 = map_expression_element env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_lambda_expression_init (env : env) ((v1, v2, v3, v4) : CST.lambda_expression_init) =
  let v1 = R.List (List.map (map_attribute_list_ env) v1) in
  let v2 =
    R.List (List.map (map_anon_choice_static_d4628a0 env) v2)
  in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_type_pattern env x
      ))
    | None -> R.Option None)
  in
  let v4 = map_lambda_parameters env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_lambda_parameters (env : env) (x : CST.lambda_parameters) =
  (match x with
  | `Param_list x -> R.Case ("Param_list",
      map_parameter_list env x
    )
  | `Id x -> R.Case ("Id",
      map_identifier env x
    )
  | `Impl_param_list_with_modifs (v1, v2, v3, v4) -> R.Case ("Impl_param_list_with_modifs",
      let v1 = (* lambda_paren_open *) token env v1 in
      let v2 =
        map_anon_choice_rep1_choice_scoped_id_5b70ee9 env v2
      in
      let v3 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = (* "," *) token env v1 in
          let v2 =
            map_anon_choice_rep1_choice_scoped_id_5b70ee9 env v2
          in
          R.Tuple [v1; v2]
        ) v3)
      in
      let v4 = (* ")" *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  )

and map_list_pattern (env : env) (x : CST.list_pattern) =
  (match x with
  | `List_pat_body x -> R.Case ("List_pat_body",
      map_list_pattern_body env x
    )
  | `List_pat_body_var_desi (v1, v2) -> R.Case ("List_pat_body_var_desi",
      let v1 = map_list_pattern_body env v1 in
      let v2 = map_variable_designation env v2 in
      R.Tuple [v1; v2]
    )
  )

and map_list_pattern_body (env : env) ((v1, v2, v3) : CST.list_pattern_body) =
  let v1 = (* "[" *) token env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2, v3) -> R.Option (Some (
        let v1 = map_anon_choice_pat_dee696d env v1 in
        let v2 =
          R.List (List.map (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_anon_choice_pat_dee696d env v2 in
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

and map_local_function_declaration (env : env) ((v1, v2, v3, v4, v5, v6) : CST.local_function_declaration) =
  let v1 = R.List (List.map (map_attribute_list_ env) v1) in
  let v2 = R.List (List.map (map_modifier env) v2) in
  let v3 = map_type_pattern env v3 in
  let v4 = map_identifier env v4 in
  let v5 =
    (match v5 with
    | Some x -> R.Option (Some (
        map_type_parameter_list env x
      ))
    | None -> R.Option None)
  in
  let v6 = map_parameter_list env v6 in
  R.Tuple [v1; v2; v3; v4; v5; v6]

and map_lvalue_expression (env : env) (x : CST.lvalue_expression) =
  (match x with
  | `This tok -> R.Case ("This",
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
  | `Elem_access_exp x -> R.Case ("Elem_access_exp",
      map_element_access_expression env x
    )
  | `Brac_arg_list x -> R.Case ("Brac_arg_list",
      map_bracketed_argument_list env x
    )
  | `Poin_indi_exp (v1, v2) -> R.Case ("Poin_indi_exp",
      let v1 = (* "*" *) token env v1 in
      let v2 =
        (match v2 with
        | `Lvalue_exp x -> R.Case ("Lvalue_exp",
            map_lvalue_expression env x
          )
        | `Paren_exp x -> R.Case ("Paren_exp",
            map_parenthesized_expression env x
          )
        | `Poin_cast_exp x -> R.Case ("Poin_cast_exp",
            map_pointer_cast_expression env x
          )
        )
      in
      R.Tuple [v1; v2]
    )
  | `Paren_lvalue_exp (v1, v2, v3) -> R.Case ("Paren_lvalue_exp",
      let v1 = (* "(" *) token env v1 in
      let v2 = map_lvalue_expression env v2 in
      let v3 = (* ")" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Cond_access_exp x -> R.Case ("Cond_access_exp",
      map_conditional_access_expression env x
    )
  )

and map_member_access_ellipsis_expression (env : env) ((v1, v2, v3) : CST.member_access_ellipsis_expression) =
  let v1 = map_anon_choice_exp_elem_3bb8381 env v1 in
  let v2 = map_anon_choice_DOT_2ad1dab env v2 in
  let v3 = (* "..." *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_member_access_expression (env : env) ((v1, v2, v3) : CST.member_access_expression) =
  let v1 = map_anon_choice_exp_elem_3bb8381 env v1 in
  let v2 = map_anon_choice_DOT_2ad1dab env v2 in
  let v3 = map_simple_name env v3 in
  R.Tuple [v1; v2; v3]

and map_member_binding_expression (env : env) ((v1, v2) : CST.member_binding_expression) =
  let v1 = (* "." *) token env v1 in
  let v2 = map_simple_name env v2 in
  R.Tuple [v1; v2]

and map_method_declaration (env : env) ((v1, v2, v3, v4, v5, v6, v7, v8, v9) : CST.method_declaration) =
  let v1 = R.List (List.map (map_attribute_list_ env) v1) in
  let v2 = R.List (List.map (map_modifier env) v2) in
  let v3 = map_type_pattern env v3 in
  let v4 =
    (match v4 with
    | Some x -> R.Option (Some (
        map_explicit_interface_specifier env x
      ))
    | None -> R.Option None)
  in
  let v5 = map_identifier env v5 in
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

and map_name (env : env) (x : CST.name) =
  (match x with
  | `Alias_qual_name (v1, v2, v3) -> R.Case ("Alias_qual_name",
      let v1 = map_identifier env v1 in
      let v2 = (* "::" *) token env v2 in
      let v3 = map_simple_name env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Qual_name (v1, v2, v3) -> R.Case ("Qual_name",
      let v1 = map_name env v1 in
      let v2 = (* "." *) token env v2 in
      let v3 = map_simple_name env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Simple_name x -> R.Case ("Simple_name",
      map_simple_name env x
    )
  )

and map_namespace_declaration (env : env) ((v1, v2, v3, v4) : CST.namespace_declaration) =
  let v1 = (* "namespace" *) token env v1 in
  let v2 = map_name env v2 in
  let v3 = map_declaration_list env v3 in
  let v4 = (* optional_semi *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_namespace_member_declaration (env : env) (x : CST.namespace_member_declaration) =
  (match x with
  | `Name_decl x -> R.Case ("Name_decl",
      map_namespace_declaration env x
    )
  | `Type_decl x -> R.Case ("Type_decl",
      map_type_declaration env x
    )
  )

and map_non_lvalue_expression (env : env) (x : CST.non_lvalue_expression) =
  (match x with
  | `Base tok -> R.Case ("Base",
      (* "base" *) token env tok
    )
  | `Bin_exp x -> R.Case ("Bin_exp",
      map_binary_expression env x
    )
  | `Inte_str_exp x -> R.Case ("Inte_str_exp",
      map_interpolated_string_expression env x
    )
  | `Cond_exp (v1, v2, v3, v4, v5) -> R.Case ("Cond_exp",
      let v1 = map_expression_element env v1 in
      let v2 = (* "?" *) token env v2 in
      let v3 = map_expression_element env v3 in
      let v4 = (* ":" *) token env v4 in
      let v5 = map_expression_element env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `Cond_access_exp x -> R.Case ("Cond_access_exp",
      map_conditional_access_expression env x
    )
  | `Lit x -> R.Case ("Lit",
      map_literal env x
    )
  | `Exp_stmt_exp x -> R.Case ("Exp_stmt_exp",
      map_expression_statement_expression env x
    )
  | `Is_exp (v1, v2, v3) -> R.Case ("Is_exp",
      let v1 = map_expression_element env v1 in
      let v2 = (* "is" *) token env v2 in
      let v3 = map_type_pattern env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Is_pat_exp (v1, v2, v3) -> R.Case ("Is_pat_exp",
      let v1 = map_expression_element env v1 in
      let v2 = (* "is" *) token env v2 in
      let v3 = map_pattern env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `As_exp (v1, v2, v3) -> R.Case ("As_exp",
      let v1 = map_expression_element env v1 in
      let v2 = (* "as" *) token env v2 in
      let v3 = map_type_pattern env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Cast_exp x -> R.Case ("Cast_exp",
      map_cast_expression env x
    )
  | `Chec_exp x -> R.Case ("Chec_exp",
      map_checked_expression env x
    )
  | `Coll_exp (v1, v2, v3) -> R.Case ("Coll_exp",
      let v1 = (* "[" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_collection_items env x
          ))
        | None -> R.Option None)
      in
      let v3 = (* "]" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Switch_exp (v1, v2, v3) -> R.Case ("Switch_exp",
      let v1 = map_expression_element env v1 in
      let v2 = (* "switch" *) token env v2 in
      let v3 = map_switch_expression_body env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Throw_exp (v1, v2) -> R.Case ("Throw_exp",
      let v1 = (* "throw" *) token env v1 in
      let v2 = map_expression_element env v2 in
      R.Tuple [v1; v2]
    )
  | `Defa_exp x -> R.Case ("Defa_exp",
      map_default_expression env x
    )
  | `Lambda_exp (v1, v2, v3) -> R.Case ("Lambda_exp",
      let v1 = map_lambda_expression_init env v1 in
      let v2 = (* "=>" *) token env v2 in
      let v3 =
        (match v3 with
        | `Blk x -> R.Case ("Blk",
            map_block env x
          )
        | `Exp x -> R.Case ("Exp",
            map_expression_element env x
          )
        )
      in
      R.Tuple [v1; v2; v3]
    )
  | `With_exp (v1, v2, v3) -> R.Case ("With_exp",
      let v1 = map_expression_element env v1 in
      let v2 = (* "with" *) token env v2 in
      let v3 = map_with_body env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Sizeof_exp x -> R.Case ("Sizeof_exp",
      map_sizeof_expression env x
    )
  | `Typeof_exp x -> R.Case ("Typeof_exp",
      map_typeof_expression env x
    )
  | `Make_exp (v1, v2, v3, v4) -> R.Case ("Make_exp",
      let v1 = (* "__makeref" *) token env v1 in
      let v2 = (* "(" *) token env v2 in
      let v3 = map_expression_element env v3 in
      let v4 = (* ")" *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Ref_exp (v1, v2) -> R.Case ("Ref_exp",
      let v1 = (* "ref" *) token env v1 in
      let v2 = map_expression_element env v2 in
      R.Tuple [v1; v2]
    )
  | `Addr_of_exp x -> R.Case ("Addr_of_exp",
      map_address_of_expression env x
    )
  | `Reft_exp (v1, v2, v3, v4) -> R.Case ("Reft_exp",
      let v1 = (* "__reftype" *) token env v1 in
      let v2 = (* "(" *) token env v2 in
      let v3 = map_expression_element env v3 in
      let v4 = (* ")" *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Refv_exp (v1, v2, v3, v4, v5, v6) -> R.Case ("Refv_exp",
      let v1 = (* "__refvalue" *) token env v1 in
      let v2 = (* "(" *) token env v2 in
      let v3 = map_expression_element env v3 in
      let v4 = (* "," *) token env v4 in
      let v5 = map_type_pattern env v5 in
      let v6 = (* ")" *) token env v6 in
      R.Tuple [v1; v2; v3; v4; v5; v6]
    )
  | `Stac_exp (v1, v2, v3) -> R.Case ("Stac_exp",
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
  | `Range_exp (v1, v2, v3) -> R.Case ("Range_exp",
      let v1 =
        (match v1 with
        | Some x -> R.Option (Some (
            map_expression_element env x
          ))
        | None -> R.Option None)
      in
      let v2 = (* ".." *) token env v2 in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_expression_element env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3]
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
  | `Anon_meth_exp (v1, v2, v3, v4) -> R.Case ("Anon_meth_exp",
      let v1 =
        R.List (List.map (map_anon_choice_static_d4628a0 env) v1)
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
  | `Impl_stac_exp (v1, v2, v3, v4) -> R.Case ("Impl_stac_exp",
      let v1 = (* "stackalloc" *) token env v1 in
      let v2 = (* "[" *) token env v2 in
      let v3 = (* "]" *) token env v3 in
      let v4 = map_initializer_expression env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Init_exp x -> R.Case ("Init_exp",
      map_initializer_expression env x
    )
  | `Query_exp (v1, v2) -> R.Case ("Query_exp",
      let v1 = map_from_clause env v1 in
      let v2 = map_query_body env v2 in
      R.Tuple [v1; v2]
    )
  | `Prep_if_in_exp (v1, v2, v3, v4, v5, v6) -> R.Case ("Prep_if_in_exp",
      let v1 = map_pat_3df6e71 env v1 in
      let v2 = map_preproc_expression env v2 in
      let v3 = map_pat_1d78758 env v3 in
      let v4 =
        (match v4 with
        | Some x -> R.Option (Some (
            map_expression_element env x
          ))
        | None -> R.Option None)
      in
      let v5 =
        (match v5 with
        | Some x -> R.Option (Some (
            map_anon_choice_prep_else_in_exp_678167b env x
          ))
        | None -> R.Option None)
      in
      let v6 = map_pat_c46d1b2 env v6 in
      R.Tuple [v1; v2; v3; v4; v5; v6]
    )
  )

and map_nullable_base_type (env : env) (x : CST.nullable_base_type) =
  (match x with
  | `Array_type x -> R.Case ("Array_type",
      map_array_type env x
    )
  | `Name x -> R.Case ("Name",
      map_name env x
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

and map_operator_declaration (env : env) ((v1, v2, v3, v4, v5, v6, v7, v8, v9) : CST.operator_declaration) =
  let v1 = R.List (List.map (map_attribute_list_ env) v1) in
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
  let v7 =
    (match v7 with
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
    | `HATEQ tok -> R.Case ("HATEQ",
        (* "^=" *) token env tok
      )
    | `BAREQ tok -> R.Case ("BAREQ",
        (* "|=" *) token env tok
      )
    | `AMPEQ tok -> R.Case ("AMPEQ",
        (* "&=" *) token env tok
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
    )
  in
  let v8 = map_parameter_list env v8 in
  let v9 = map_function_body env v9 in
  R.Tuple [v1; v2; v3; v4; v5; v6; v7; v8; v9]

and map_ordering (env : env) ((v1, v2) : CST.ordering) =
  let v1 = map_expression_element env v1 in
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
  | `Rep_attr_list__opt_rep_choice_choice_this_type_id_opt_EQ_exp (v1, v2, v3, v4) -> R.Case ("Rep_attr_list__opt_rep_choice_choice_this_type_id_opt_EQ_exp",
      let v1 = R.List (List.map (map_attribute_list_ env) v1) in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_parameter_type_with_modifiers env x
          ))
        | None -> R.Option None)
      in
      let v3 = map_identifier env v3 in
      let v4 =
        (match v4 with
        | Some (v1, v2) -> R.Option (Some (
            let v1 = (* "=" *) token env v1 in
            let v2 = map_expression_element env v2 in
            R.Tuple [v1; v2]
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
        map_anon_choice_param_rep_COMMA_choice_param_6325c4d env x
      ))
    | None -> R.Option None)
  in
  let v3 = (* ")" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_parameter_type_with_modifiers (env : env) ((v1, v2) : CST.parameter_type_with_modifiers) =
  let v1 =
    R.List (List.map (map_anon_choice_choice_this_6a06011 env) v1)
  in
  let v2 = map_type_pattern env v2 in
  R.Tuple [v1; v2]

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
  | `Recu_pat x -> R.Case ("Recu_pat",
      map_recursive_pattern env x
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
  | `Paren_pat_with_desi (v1, v2, v3, v4) -> R.Case ("Paren_pat_with_desi",
      let v1 = (* "(" *) token env v1 in
      let v2 = map_pattern env v2 in
      let v3 = (* ")" *) token env v3 in
      let v4 = map_variable_designation env v4 in
      R.Tuple [v1; v2; v3; v4]
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
  | `List_pat x -> R.Case ("List_pat",
      map_list_pattern env x
    )
  | `Type_pat x -> R.Case ("Type_pat",
      map_type_pattern env x
    )
  )

and map_plain_function_body (env : env) (x : CST.plain_function_body) =
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

and map_pointer_base_type (env : env) (x : CST.pointer_base_type) =
  (match x with
  | `Name x -> R.Case ("Name",
      map_name env x
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

and map_pointer_cast_expression (env : env) ((v1, v2, v3, v4) : CST.pointer_cast_expression) =
  let v1 = (* "(" *) token env v1 in
  let v2 = map_pointer_type env v2 in
  let v3 = (* ")" *) token env v3 in
  let v4 =
    (match v4 with
    | `Lvalue_exp x -> R.Case ("Lvalue_exp",
        map_lvalue_expression env x
      )
    | `Paren_exp x -> R.Case ("Paren_exp",
        map_parenthesized_expression env x
      )
    | `Addr_of_exp x -> R.Case ("Addr_of_exp",
        map_address_of_expression env x
      )
    )
  in
  R.Tuple [v1; v2; v3; v4]

and map_pointer_type (env : env) ((v1, v2) : CST.pointer_type) =
  let v1 = map_pointer_base_type env v1 in
  let v2 = (* "*" *) token env v2 in
  R.Tuple [v1; v2]

and map_positional_pattern_clause (env : env) ((v1, v2, v3) : CST.positional_pattern_clause) =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    (match v2 with
    | Some opt -> R.Option (Some (
        (match opt with
        | Some x -> R.Option (Some (
            map_anon_subp_rep_COMMA_subp_300d2c5 env x
          ))
        | None -> R.Option None)
      ))
    | None -> R.Option None)
  in
  let v3 = (* ")" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_postfix_unary_expression (env : env) ((v1, v2) : CST.postfix_unary_expression) =
  let v1 = map_expression_element env v1 in
  let v2 =
    (match v2 with
    | `PLUSPLUS tok -> R.Case ("PLUSPLUS",
        (* "++" *) token env tok
      )
    | `DASHDASH tok -> R.Case ("DASHDASH",
        (* "--" *) token env tok
      )
    | `BANG tok -> R.Case ("BANG",
        (* "!" *) token env tok
      )
    )
  in
  R.Tuple [v1; v2]

and map_prefix_unary_expression (env : env) ((v1, v2) : CST.prefix_unary_expression) =
  let v1 =
    (match v1 with
    | `PLUSPLUS tok -> R.Case ("PLUSPLUS",
        (* "++" *) token env tok
      )
    | `DASHDASH tok -> R.Case ("DASHDASH",
        (* "--" *) token env tok
      )
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
    | `HAT tok -> R.Case ("HAT",
        (* "^" *) token env tok
      )
    )
  in
  let v2 = map_expression_element env v2 in
  R.Tuple [v1; v2]

and map_preproc_if_in_collection (env : env) ((v1, v2, v3, v4, v5, v6) : CST.preproc_if_in_collection) =
  let v1 = map_pat_3df6e71 env v1 in
  let v2 = map_preproc_expression env v2 in
  let v3 = map_pat_1d78758 env v3 in
  let v4 =
    (match v4 with
    | Some x -> R.Option (Some (
        map_collection_items env x
      ))
    | None -> R.Option None)
  in
  let v5 =
    (match v5 with
    | Some x -> R.Option (Some (
        map_anon_choice_prep_else_in_coll_8cdb09f env x
      ))
    | None -> R.Option None)
  in
  let v6 = map_pat_c46d1b2 env v6 in
  R.Tuple [v1; v2; v3; v4; v5; v6]

and map_preproc_if_in_else (env : env) ((v1, v2, v3, v4, v5, v6) : CST.preproc_if_in_else) =
  let v1 = (* preproc_else_if_start *) token env v1 in
  let v2 = map_preproc_expression env v2 in
  let v3 = map_pat_1d78758 env v3 in
  let v4 =
    (match v4 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = (* "else" *) token env v1 in
        let v2 = map_global_statement env v2 in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  let v5 =
    (match v5 with
    | Some x -> R.Option (Some (
        map_anon_choice_prep_else_in_else_e2f3ee0 env x
      ))
    | None -> R.Option None)
  in
  let v6 = map_pat_c46d1b2 env v6 in
  R.Tuple [v1; v2; v3; v4; v5; v6]

and map_preproc_if_in_enum_member_declaration (env : env) ((v1, v2, v3, v4, v5, v6) : CST.preproc_if_in_enum_member_declaration) =
  let v1 = map_pat_3df6e71 env v1 in
  let v2 = map_preproc_expression env v2 in
  let v3 = map_pat_1d78758 env v3 in
  let v4 =
    (match v4 with
    | Some x -> R.Option (Some (
        map_enum_member_items env x
      ))
    | None -> R.Option None)
  in
  let v5 =
    (match v5 with
    | Some x -> R.Option (Some (
        map_anon_choice_prep_else_in_enum_member_decl_ef97f12 env x
      ))
    | None -> R.Option None)
  in
  let v6 = map_pat_c46d1b2 env v6 in
  R.Tuple [v1; v2; v3; v4; v5; v6]

and map_preproc_if_in_initializer (env : env) ((v1, v2, v3, v4, v5, v6) : CST.preproc_if_in_initializer) =
  let v1 = map_pat_3df6e71 env v1 in
  let v2 = map_preproc_expression env v2 in
  let v3 = map_pat_1d78758 env v3 in
  let v4 =
    (match v4 with
    | Some x -> R.Option (Some (
        map_initializer_items env x
      ))
    | None -> R.Option None)
  in
  let v5 =
    (match v5 with
    | Some x -> R.Option (Some (
        map_anon_choice_prep_else_in_init_085a8ad env x
      ))
    | None -> R.Option None)
  in
  let v6 = map_pat_c46d1b2 env v6 in
  R.Tuple [v1; v2; v3; v4; v5; v6]

and map_preproc_if_in_statement_tail (env : env) ((v1, v2, v3, v4, v5, v6) : CST.preproc_if_in_statement_tail) =
  let v1 = map_pat_3df6e71 env v1 in
  let v2 = map_preproc_expression env v2 in
  let v3 = map_pat_1d78758 env v3 in
  let v4 =
    (match v4 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = map_expression_element env v1 in
        let v2 = (* ";" *) token env v2 in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  let v5 =
    (match v5 with
    | Some x -> R.Option (Some (
        map_anon_choice_prep_else_in_stmt_tail_95f56b9 env x
      ))
    | None -> R.Option None)
  in
  let v6 = map_pat_c46d1b2 env v6 in
  R.Tuple [v1; v2; v3; v4; v5; v6]

and map_preproc_if_in_top_level (env : env) ((v1, v2, v3, v4, v5, v6) : CST.preproc_if_in_top_level) =
  let v1 = map_pat_3df6e71 env v1 in
  let v2 = map_preproc_expression env v2 in
  let v3 = map_pat_1d78758 env v3 in
  let v4 =
    R.List (List.map (map_anon_choice_choice_top_level_item_no_stmt_485bee8 env) v4)
  in
  let v5 =
    (match v5 with
    | Some x -> R.Option (Some (
        map_anon_choice_prep_else_in_top_level_0825411 env x
      ))
    | None -> R.Option None)
  in
  let v6 = map_pat_c46d1b2 env v6 in
  R.Tuple [v1; v2; v3; v4; v5; v6]

and map_primary_constructor_base_type (env : env) ((v1, v2) : CST.primary_constructor_base_type) =
  let v1 = map_name env v1 in
  let v2 = map_argument_list env v2 in
  R.Tuple [v1; v2]

and map_property_declaration (env : env) ((v1, v2, v3, v4, v5, v6) : CST.property_declaration) =
  let v1 = R.List (List.map (map_attribute_list_ env) v1) in
  let v2 = R.List (List.map (map_modifier env) v2) in
  let v3 = map_type_pattern env v3 in
  let v4 =
    (match v4 with
    | Some x -> R.Option (Some (
        map_explicit_interface_specifier env x
      ))
    | None -> R.Option None)
  in
  let v5 = map_identifier env v5 in
  let v6 =
    (match v6 with
    | `Acce_list_opt_EQ_exp_SEMI (v1, v2) -> R.Case ("Acce_list_opt_EQ_exp_SEMI",
        let v1 = map_accessor_list env v1 in
        let v2 =
          (match v2 with
          | Some (v1, v2, v3) -> R.Option (Some (
              let v1 = (* "=" *) token env v1 in
              let v2 = map_expression_element env v2 in
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

and map_property_pattern_clause (env : env) ((v1, v2, v3, v4) : CST.property_pattern_clause) =
  let v1 = (* "{" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_anon_subp_rep_COMMA_subp_300d2c5 env x
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

and map_query_body (env : env) ((v1, v2, v3) : CST.query_body) =
  let v1 = R.List (List.map (map_query_clause env) v1) in
  let v2 = map_select_or_group_clause env v2 in
  let v3 =
    R.List (List.map (fun (v1, v2, v3, v4) ->
      let v1 = (* "into" *) token env v1 in
      let v2 = map_identifier env v2 in
      let v3 = R.List (List.map (map_query_clause env) v3) in
      let v4 = map_select_or_group_clause env v4 in
      R.Tuple [v1; v2; v3; v4]
    ) v3)
  in
  R.Tuple [v1; v2; v3]

and map_query_clause (env : env) (x : CST.query_clause) =
  (match x with
  | `From_clause x -> R.Case ("From_clause",
      map_from_clause env x
    )
  | `Join_clause (v1, v2, v3, v4) -> R.Case ("Join_clause",
      let v1 = (* "join" *) token env v1 in
      let v2 = map_join_header env v2 in
      let v3 = map_join_body env v3 in
      let v4 =
        (match v4 with
        | Some x -> R.Option (Some (
            map_join_into_clause env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Let_clause (v1, v2, v3, v4) -> R.Case ("Let_clause",
      let v1 = (* "let" *) token env v1 in
      let v2 = map_identifier env v2 in
      let v3 = (* "=" *) token env v3 in
      let v4 = map_expression_element env v4 in
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
      let v2 = map_expression_element env v2 in
      R.Tuple [v1; v2]
    )
  )

and map_receiver_parameter (env : env) ((v1, v2, v3, v4) : CST.receiver_parameter) =
  let v1 = R.List (List.map (map_attribute_list_ env) v1) in
  let v2 =
    R.List (List.map (map_anon_choice_choice_this_6a06011 env) v2)
  in
  let v3 = map_type_pattern env v3 in
  let v4 =
    (match v4 with
    | Some x -> R.Option (Some (
        map_identifier env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3; v4]

and map_record_base (env : env) (x : CST.record_base) =
  (match x with
  | `COLON_name_rep_COMMA_name (v1, v2, v3) -> R.Case ("COLON_name_rep_COMMA_name",
      let v1 = (* ":" *) token env v1 in
      let v2 = map_name env v2 in
      let v3 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = (* "," *) token env v1 in
          let v2 = map_name env v2 in
          R.Tuple [v1; v2]
        ) v3)
      in
      R.Tuple [v1; v2; v3]
    )
  | `COLON_prim_cons_base_type_opt_COMMA_name_rep_COMMA_name (v1, v2, v3) -> R.Case ("COLON_prim_cons_base_type_opt_COMMA_name_rep_COMMA_name",
      let v1 = (* ":" *) token env v1 in
      let v2 = map_primary_constructor_base_type env v2 in
      let v3 =
        (match v3 with
        | Some (v1, v2, v3) -> R.Option (Some (
            let v1 = (* "," *) token env v1 in
            let v2 = map_name env v2 in
            let v3 =
              R.List (List.map (fun (v1, v2) ->
                let v1 = (* "," *) token env v1 in
                let v2 = map_name env v2 in
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

and map_record_declaration (env : env) ((v1, v2) : CST.record_declaration) =
  let v1 = map_record_declaration_initializer env v1 in
  let v2 = map_declaration_list_body env v2 in
  R.Tuple [v1; v2]

and map_record_declaration_initializer (env : env) ((v1, v2, v3, v4, v5, v6, v7, v8) : CST.record_declaration_initializer) =
  let v1 = R.List (List.map (map_attribute_list_ env) v1) in
  let v2 = R.List (List.map (map_modifier env) v2) in
  let v3 = (* "record" *) token env v3 in
  let v4 =
    (match v4 with
    | Some x -> R.Option (Some (
        (match x with
        | `Class tok -> R.Case ("Class",
            (* "class" *) token env tok
          )
        | `Struct tok -> R.Case ("Struct",
            (* "struct" *) token env tok
          )
        )
      ))
    | None -> R.Option None)
  in
  let v5 = map_identifier env v5 in
  let v6 =
    R.List (List.map (fun x ->
      (match x with
      | `Type_param_list x -> R.Case ("Type_param_list",
          map_type_parameter_list env x
        )
      | `Param_list x -> R.Case ("Param_list",
          map_parameter_list env x
        )
      )
    ) v6)
  in
  let v7 =
    (match v7 with
    | Some x -> R.Option (Some (
        map_record_base env x
      ))
    | None -> R.Option None)
  in
  let v8 =
    R.List (List.map (map_type_parameter_constraints_clause env) v8)
  in
  R.Tuple [v1; v2; v3; v4; v5; v6; v7; v8]

and map_recursive_pattern (env : env) (x : CST.recursive_pattern) =
  (match x with
  | `Name_posi_pat_clause_opt_prop_pat_clause_var_desi (v1, v2, v3, v4) -> R.Case ("Name_posi_pat_clause_opt_prop_pat_clause_var_desi",
      let v1 = map_name env v1 in
      let v2 = map_positional_pattern_clause env v2 in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_property_pattern_clause env x
          ))
        | None -> R.Option None)
      in
      let v4 = map_variable_designation env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Name_posi_pat_clause_opt_prop_pat_clause (v1, v2, v3) -> R.Case ("Name_posi_pat_clause_opt_prop_pat_clause",
      let v1 = map_name env v1 in
      let v2 = map_positional_pattern_clause env v2 in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_property_pattern_clause env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3]
    )
  | `Posi_pat_clause_var_desi (v1, v2) -> R.Case ("Posi_pat_clause_var_desi",
      let v1 = map_positional_pattern_clause env v1 in
      let v2 = map_variable_designation env v2 in
      R.Tuple [v1; v2]
    )
  | `Posi_pat_clause x -> R.Case ("Posi_pat_clause",
      map_positional_pattern_clause env x
    )
  | `Type_choice_posi_pat_clause_opt_prop_pat_clause_opt_var_desi (v1, v2, v3) -> R.Case ("Type_choice_posi_pat_clause_opt_prop_pat_clause_opt_var_desi",
      let v1 = map_type_pattern env v1 in
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
  | `Choice_posi_pat_clause_prop_pat_clause_opt_var_desi (v1, v2) -> R.Case ("Choice_posi_pat_clause_prop_pat_clause_opt_var_desi",
      let v1 =
        (match v1 with
        | `Posi_pat_clause_prop_pat_clause (v1, v2) -> R.Case ("Posi_pat_clause_prop_pat_clause",
            let v1 = map_positional_pattern_clause env v1 in
            let v2 = map_property_pattern_clause env v2 in
            R.Tuple [v1; v2]
          )
        | `Prop_pat_clause x -> R.Case ("Prop_pat_clause",
            map_property_pattern_clause env x
          )
        )
      in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_variable_designation env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2]
    )
  )

and map_ref_base_type (env : env) (x : CST.ref_base_type) =
  (match x with
  | `Impl_type tok -> R.Case ("Impl_type",
      (* "var" *) token env tok
    )
  | `Name x -> R.Case ("Name",
      map_name env x
    )
  | `Null_type x -> R.Case ("Null_type",
      map_nullable_type env x
    )
  | `Array_type x -> R.Case ("Array_type",
      map_array_type env x
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
  let v3 = map_type_pattern env v3 in
  R.Tuple [v1; v2; v3]

and map_relational_pattern (env : env) (x : CST.relational_pattern) =
  (match x with
  | `LT_exp (v1, v2) -> R.Case ("LT_exp",
      let v1 = (* "<" *) token env v1 in
      let v2 = map_expression_element env v2 in
      R.Tuple [v1; v2]
    )
  | `LTEQ_exp (v1, v2) -> R.Case ("LTEQ_exp",
      let v1 = (* "<=" *) token env v1 in
      let v2 = map_expression_element env v2 in
      R.Tuple [v1; v2]
    )
  | `GT_exp (v1, v2) -> R.Case ("GT_exp",
      let v1 = (* ">" *) token env v1 in
      let v2 = map_expression_element env v2 in
      R.Tuple [v1; v2]
    )
  | `GTEQ_exp (v1, v2) -> R.Case ("GTEQ_exp",
      let v1 = (* ">=" *) token env v1 in
      let v2 = map_expression_element env v2 in
      R.Tuple [v1; v2]
    )
  )

and map_scoped_base_type (env : env) (x : CST.scoped_base_type) =
  (match x with
  | `Name x -> R.Case ("Name",
      map_name env x
    )
  | `Ref_type x -> R.Case ("Ref_type",
      map_ref_type env x
    )
  )

and map_select_or_group_clause (env : env) (x : CST.select_or_group_clause) =
  (match x with
  | `Group_clause (v1, v2, v3, v4) -> R.Case ("Group_clause",
      let v1 = (* "group" *) token env v1 in
      let v2 = map_expression_element env v2 in
      let v3 = (* "by" *) token env v3 in
      let v4 = map_expression_element env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Select_clause (v1, v2) -> R.Case ("Select_clause",
      let v1 = (* "select" *) token env v1 in
      let v2 = map_expression_element env v2 in
      R.Tuple [v1; v2]
    )
  )

and map_simple_name (env : env) (x : CST.simple_name) =
  (match x with
  | `Id x -> R.Case ("Id",
      map_identifier env x
    )
  | `Gene_name (v1, v2) -> R.Case ("Gene_name",
      let v1 = map_identifier env v1 in
      let v2 = map_type_argument_list env v2 in
      R.Tuple [v1; v2]
    )
  )

and map_sizeof_expression (env : env) ((v1, v2, v3, v4) : CST.sizeof_expression) =
  let v1 = (* "sizeof" *) token env v1 in
  let v2 = (* "(" *) token env v2 in
  let v3 = map_type_pattern env v3 in
  let v4 = (* ")" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_slice_pattern (env : env) (x : CST.slice_pattern) =
  (match x with
  | `DOTDOT tok -> R.Case ("DOTDOT",
      (* ".." *) token env tok
    )
  | `DOTDOT_pat (v1, v2) -> R.Case ("DOTDOT_pat",
      let v1 = (* ".." *) token env v1 in
      let v2 = map_pattern env v2 in
      R.Tuple [v1; v2]
    )
  )

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
      let v1 = map_anon_choice_chec_7987657 env v1 in
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
      let v5 = map_expression_element env v5 in
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
  | `For_stmt (v1, v2, v3) -> R.Case ("For_stmt",
      let v1 = (* "for" *) token env v1 in
      let v2 = map_for_statement_conditions env v2 in
      let v3 = map_global_statement env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Ret_stmt (v1, v2) -> R.Case ("Ret_stmt",
      let v1 = (* "return" *) token env v1 in
      let v2 =
        (match v2 with
        | `Opt_exp_SEMI (v1, v2) -> R.Case ("Opt_exp_SEMI",
            let v1 =
              (match v1 with
              | Some x -> R.Option (Some (
                  map_expression_element env x
                ))
              | None -> R.Option None)
            in
            let v2 = (* ";" *) token env v2 in
            R.Tuple [v1; v2]
          )
        | `Cond_ending_in_prep x -> R.Case ("Cond_ending_in_prep",
            map_conditional_ending_in_preproc env x
          )
        )
      in
      R.Tuple [v1; v2]
    )
  | `Lock_stmt (v1, v2, v3, v4, v5) -> R.Case ("Lock_stmt",
      let v1 = (* "lock" *) token env v1 in
      let v2 = (* "(" *) token env v2 in
      let v3 = map_expression_element env v3 in
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
            let v2 = map_expression_element env v2 in
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
  | `Switch_stmt (v1, v2, v3) -> R.Case ("Switch_stmt",
      let v1 = (* "switch" *) token env v1 in
      let v2 =
        (match v2 with
        | `LPAR_exp_RPAR (v1, v2, v3) -> R.Case ("LPAR_exp_RPAR",
            let v1 = (* "(" *) token env v1 in
            let v2 = map_expression_element env v2 in
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
            map_expression_element env x
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
        | `Using_var_decl x -> R.Case ("Using_var_decl",
            map_using_variable_declaration env x
          )
        | `Exp x -> R.Case ("Exp",
            map_expression_element env x
          )
        )
      in
      let v5 = (* ")" *) token env v5 in
      let v6 = map_global_statement env v6 in
      R.Tuple [v1; v2; v3; v4; v5; v6]
    )
  | `Fore_stmt (v1, v2) -> R.Case ("Fore_stmt",
      let v1 = map_foreach_statement_initializer env v1 in
      let v2 = map_global_statement env v2 in
      R.Tuple [v1; v2]
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
            map_expression_element env x
          ))
        | None -> R.Option None)
      in
      let v4 = (* ";" *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Labe_stmt (v1, v2, v3) -> R.Case ("Labe_stmt",
      let v1 = map_identifier env v1 in
      let v2 = (* ":" *) token env v2 in
      let v3 = map_global_statement env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `If_stmt (v1, v2, v3, v4, v5, v6) -> R.Case ("If_stmt",
      let v1 = (* "if" *) token env v1 in
      let v2 = (* "(" *) token env v2 in
      let v3 = map_expression_element env v3 in
      let v4 = (* ")" *) token env v4 in
      let v5 = map_global_statement env v5 in
      let v6 =
        (match v6 with
        | Some x -> R.Option (Some (
            (match x with
            | `Else_stmt (v1, v2) -> R.Case ("Else_stmt",
                let v1 = (* "else" *) token env v1 in
                let v2 = map_global_statement env v2 in
                R.Tuple [v1; v2]
              )
            | `Prep_if_in_else_opt_else_stmt (v1, v2) -> R.Case ("Prep_if_in_else_opt_else_stmt",
                let v1 = map_preproc_if_in_else env v1 in
                let v2 =
                  (match v2 with
                  | Some (v1, v2) -> R.Option (Some (
                      let v1 = (* "else" *) token env v1 in
                      let v2 = map_global_statement env v2 in
                      R.Tuple [v1; v2]
                    ))
                  | None -> R.Option None)
                in
                R.Tuple [v1; v2]
              )
            )
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3; v4; v5; v6]
    )
  | `While_stmt (v1, v2, v3, v4, v5) -> R.Case ("While_stmt",
      let v1 = (* "while" *) token env v1 in
      let v2 = (* "(" *) token env v2 in
      let v3 = map_expression_element env v3 in
      let v4 = (* ")" *) token env v4 in
      let v5 = map_global_statement env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
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
  | `Local_func_stmt (v1, v2, v3) -> R.Case ("Local_func_stmt",
      let v1 = map_local_function_declaration env v1 in
      let v2 =
        R.List (List.map (map_type_parameter_constraints_clause env) v2)
      in
      let v3 = map_function_body env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Prep_if_in_top_level x -> R.Case ("Prep_if_in_top_level",
      map_preproc_if_in_top_level env x
    )
  )

and map_struct_declaration (env : env) ((v1, v2) : CST.struct_declaration) =
  let v1 = map_struct_declaration_initializer env v1 in
  let v2 = map_declaration_list_body env v2 in
  R.Tuple [v1; v2]

and map_struct_declaration_initializer (env : env) ((v1, v2, v3, v4, v5, v6, v7) : CST.struct_declaration_initializer) =
  let v1 = R.List (List.map (map_attribute_list_ env) v1) in
  let v2 = R.List (List.map (map_modifier env) v2) in
  let v3 =
    (match v3 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = (* "ref" *) token env v1 in
        let v2 =
          (match v2 with
          | Some tok -> R.Option (Some (
              (* "partial" *) token env tok
            ))
          | None -> R.Option None)
        in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  let v4 = (* "struct" *) token env v4 in
  let v5 = map_identifier env v5 in
  let v6 =
    R.List (List.map (map_anon_choice_type_param_list_94781cc env) v6)
  in
  let v7 =
    R.List (List.map (map_type_parameter_constraints_clause env) v7)
  in
  R.Tuple [v1; v2; v3; v4; v5; v6; v7]

and map_subpattern (env : env) ((v1, v2) : CST.subpattern) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        (match x with
        | `Exp_COLON (v1, v2) -> R.Case ("Exp_COLON",
            let v1 = map_expression_element env v1 in
            let v2 = (* ":" *) token env v2 in
            R.Tuple [v1; v2]
          )
        | `Id_COLON (v1, v2) -> R.Case ("Id_COLON",
            let v1 = map_identifier env v1 in
            let v2 = (* ":" *) token env v2 in
            R.Tuple [v1; v2]
          )
        )
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
  let v4 = map_expression_element env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_switch_expression_body (env : env) ((v1, v2, v3, v4) : CST.switch_expression_body) =
  let v1 = (* "{" *) token env v1 in
  let v2 =
    (match v2 with
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
  let v3 =
    (match v3 with
    | Some tok -> R.Option (Some (
        (* "," *) token env tok
      ))
    | None -> R.Option None)
  in
  let v4 = (* "}" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_switch_section (env : env) ((v1, v2, v3) : CST.switch_section) =
  let v1 =
    (match v1 with
    | `Case_choice_exp (v1, v2) -> R.Case ("Case_choice_exp",
        let v1 = (* "case" *) token env v1 in
        let v2 =
          (match v2 with
          | `Exp x -> R.Case ("Exp",
              map_expression_element env x
            )
          | `Pat_opt_when_clause (v1, v2) -> R.Case ("Pat_opt_when_clause",
              let v1 = map_pattern env v1 in
              let v2 =
                (match v2 with
                | Some x -> R.Option (Some (
                    map_when_clause env x
                  ))
                | None -> R.Option None)
              in
              R.Tuple [v1; v2]
            )
          )
        in
        R.Tuple [v1; v2]
      )
    | `Defa tok -> R.Case ("Defa",
        (* "default" *) token env tok
      )
    )
  in
  let v2 = (* ":" *) token env v2 in
  let v3 = R.List (List.map (map_global_statement env) v3) in
  R.Tuple [v1; v2; v3]

and map_top_level_item_no_statement (env : env) (x : CST.top_level_item_no_statement) =
  (match x with
  | `Extern_alias_dire (v1, v2, v3, v4) -> R.Case ("Extern_alias_dire",
      let v1 = (* "extern" *) token env v1 in
      let v2 = (* "alias" *) token env v2 in
      let v3 = map_identifier env v3 in
      let v4 = (* ";" *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Using_dire x -> R.Case ("Using_dire",
      map_using_directive env x
    )
  | `Global_attr (v1, v2, v3, v4, v5, v6, v7) -> R.Case ("Global_attr",
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
      let v4 = map_attribute env v4 in
      let v5 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = (* "," *) token env v1 in
          let v2 = map_attribute env v2 in
          R.Tuple [v1; v2]
        ) v5)
      in
      let v6 =
        (match v6 with
        | Some tok -> R.Option (Some (
            (* "," *) token env tok
          ))
        | None -> R.Option None)
      in
      let v7 = (* "]" *) token env v7 in
      R.Tuple [v1; v2; v3; v4; v5; v6; v7]
    )
  | `Prep_if_in_top_level x -> R.Case ("Prep_if_in_top_level",
      map_preproc_if_in_top_level env x
    )
  | `Choice_name_decl x -> R.Case ("Choice_name_decl",
      map_namespace_member_declaration env x
    )
  | `File_scoped_name_decl (v1, v2, v3) -> R.Case ("File_scoped_name_decl",
      let v1 = (* "namespace" *) token env v1 in
      let v2 = map_name env v2 in
      let v3 = (* ";" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_tuple_element (env : env) ((v1, v2) : CST.tuple_element) =
  let v1 = map_type_pattern env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_identifier env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2]

and map_tuple_expression (env : env) ((v1, v2, v3, v4) : CST.tuple_expression) =
  let v1 = (* "(" *) token env v1 in
  let v2 = map_argument env v2 in
  let v3 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_argument env v2 in
      R.Tuple [v1; v2]
    ) v3)
  in
  let v4 = (* ")" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_tuple_type (env : env) ((v1, v2, v3, v4) : CST.tuple_type) =
  let v1 = (* "(" *) token env v1 in
  let v2 = map_tuple_element env v2 in
  let v3 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_tuple_element env v2 in
      R.Tuple [v1; v2]
    ) v3)
  in
  let v4 = (* ")" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_type_ (env : env) (x : CST.type_) =
  (match x with
  | `Impl_type tok -> R.Case ("Impl_type",
      (* "var" *) token env tok
    )
  | `Array_type x -> R.Case ("Array_type",
      map_array_type env x
    )
  | `Name x -> R.Case ("Name",
      map_name env x
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

and map_type_declaration (env : env) (x : CST.type_declaration) =
  (match x with
  | `Class_decl x -> R.Case ("Class_decl",
      map_class_declaration env x
    )
  | `Struct_decl x -> R.Case ("Struct_decl",
      map_struct_declaration env x
    )
  | `Enum_decl x -> R.Case ("Enum_decl",
      map_enum_declaration env x
    )
  | `Inte_decl x -> R.Case ("Inte_decl",
      map_interface_declaration env x
    )
  | `Dele_decl x -> R.Case ("Dele_decl",
      map_delegate_declaration env x
    )
  | `Record_decl x -> R.Case ("Record_decl",
      map_record_declaration env x
    )
  | `Ellips tok -> R.Case ("Ellips",
      (* "..." *) token env tok
    )
  )

and map_type_parameter (env : env) ((v1, v2, v3) : CST.type_parameter) =
  let v1 = R.List (List.map (map_attribute_list_ env) v1) in
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
  let v3 = map_identifier env v3 in
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
  | `Type x -> R.Case ("Type",
      map_type_pattern env x
    )
  )

and map_type_parameter_constraints_clause (env : env) ((v1, v2, v3, v4, v5) : CST.type_parameter_constraints_clause) =
  let v1 = (* "where" *) token env v1 in
  let v2 = map_identifier env v2 in
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

and map_typeof_expression (env : env) ((v1, v2, v3, v4) : CST.typeof_expression) =
  let v1 = (* "typeof" *) token env v1 in
  let v2 = (* "(" *) token env v2 in
  let v3 = map_type_pattern env v3 in
  let v4 = (* ")" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_using_directive (env : env) ((v1, v2, v3, v4) : CST.using_directive) =
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
    | `Opt_unsafe_id_EQ_type (v1, v2, v3, v4) -> R.Case ("Opt_unsafe_id_EQ_type",
        let v1 =
          (match v1 with
          | Some tok -> R.Option (Some (
              (* "unsafe" *) token env tok
            ))
          | None -> R.Option None)
        in
        let v2 = map_identifier env v2 in
        let v3 = (* "=" *) token env v3 in
        let v4 = map_type_pattern env v4 in
        R.Tuple [v1; v2; v3; v4]
      )
    | `Rep_choice_static_name (v1, v2) -> R.Case ("Rep_choice_static_name",
        let v1 =
          R.List (List.map (fun x ->
            (match x with
            | `Static tok -> R.Case ("Static",
                (* "static" *) token env tok
              )
            | `Unsafe tok -> R.Case ("Unsafe",
                (* "unsafe" *) token env tok
              )
            )
          ) v1)
        in
        let v2 = map_name env v2 in
        R.Tuple [v1; v2]
      )
    )
  in
  let v4 = (* ";" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_using_variable_declaration (env : env) ((v1, v2, v3) : CST.using_variable_declaration) =
  let v1 = map_type_pattern env v1 in
  let v2 = map_using_variable_declarator env v2 in
  let v3 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_using_variable_declarator env v2 in
      R.Tuple [v1; v2]
    ) v3)
  in
  R.Tuple [v1; v2; v3]

and map_using_variable_declarator (env : env) ((v1, v2) : CST.using_variable_declarator) =
  let v1 = map_identifier env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = (* "=" *) token env v1 in
        let v2 = map_expression_element env v2 in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2]

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
  let v1 = map_anon_choice_id_bf14316 env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_bracketed_argument_list env x
      ))
    | None -> R.Option None)
  in
  let v3 =
    (match v3 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = (* "=" *) token env v1 in
        let v2 = map_expression_element env v2 in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3]

and map_when_clause (env : env) ((v1, v2) : CST.when_clause) =
  let v1 = (* "when" *) token env v1 in
  let v2 = map_expression_element env v2 in
  R.Tuple [v1; v2]

and map_with_body (env : env) ((v1, v2, v3, v4) : CST.with_body) =
  let v1 = (* "{" *) token env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = map_with_initializer env v1 in
        let v2 =
          R.List (List.map (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_with_initializer env v2 in
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

and map_with_initializer (env : env) ((v1, v2, v3) : CST.with_initializer) =
  let v1 = map_identifier env v1 in
  let v2 = (* "=" *) token env v2 in
  let v3 = map_expression_element env v3 in
  R.Tuple [v1; v2; v3]

let map_top_level_item (env : env) (x : CST.top_level_item) =
  (match x with
  | `Choice_choice_extern_alias_dire x -> R.Case ("Choice_choice_extern_alias_dire",
      (match x with
      | `Choice_extern_alias_dire x -> R.Case ("Choice_extern_alias_dire",
          map_top_level_item_no_statement env x
        )
      | `Global_stmt x -> R.Case ("Global_stmt",
          map_global_statement env x
        )
      )
    )
  | `Igno_dire x -> R.Case ("Igno_dire",
      map_ignored_directive env x
    )
  | `Exte_decl x -> R.Case ("Exte_decl",
      map_extension_declaration env x
    )
  )

let map_compilation_unit (env : env) (x : CST.compilation_unit) =
  (match x with
  | `Opt_sheb_dire_rep_top_level_item (v1, v2) -> R.Case ("Opt_sheb_dire_rep_top_level_item",
      let v1 =
        (match v1 with
        | Some tok -> R.Option (Some (
            (* shebang_directive *) token env tok
          ))
        | None -> R.Option None)
      in
      let v2 = R.List (List.map (map_top_level_item env) v2) in
      R.Tuple [v1; v2]
    )
  | `Semg_exp (v1, v2) -> R.Case ("Semg_exp",
      let v1 = (* "__SEMGREP_EXPRESSION" *) token env v1 in
      let v2 = map_expression_element env v2 in
      R.Tuple [v1; v2]
    )
  | `Switch_sect x -> R.Case ("Switch_sect",
      map_switch_section env x
    )
  )

let map_comment (env : env) (tok : CST.comment) =
  (* comment *) token env tok

let map_preproc_define (env : env) ((v1, v2, v3) : CST.preproc_define) =
  let v1 = map_pat_c3ea183 env v1 in
  let v2 = (* preproc_arg *) token env v2 in
  let v3 = map_pat_1d78758 env v3 in
  R.Tuple [v1; v2; v3]

let map_preproc_region (env : env) ((v1, v2, v3) : CST.preproc_region) =
  let v1 = map_pat_b56baf5 env v1 in
  let v2 =
    (match v2 with
    | Some tok -> R.Option (Some (
        (* preproc_arg *) token env tok
      ))
    | None -> R.Option None)
  in
  let v3 = map_pat_1d78758 env v3 in
  R.Tuple [v1; v2; v3]

let map_preproc_undef (env : env) ((v1, v2, v3) : CST.preproc_undef) =
  let v1 = map_pat_4946ad9 env v1 in
  let v2 = (* preproc_arg *) token env v2 in
  let v3 = map_pat_1d78758 env v3 in
  R.Tuple [v1; v2; v3]

let map_preproc_nullable (env : env) ((v1, v2, v3, v4) : CST.preproc_nullable) =
  let v1 = map_pat_fdf8f81 env v1 in
  let v2 =
    (match v2 with
    | `Enable tok -> R.Case ("Enable",
        (* "enable" *) token env tok
      )
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
  let v4 = map_pat_1d78758 env v4 in
  R.Tuple [v1; v2; v3; v4]

let map_preproc_endregion (env : env) ((v1, v2, v3) : CST.preproc_endregion) =
  let v1 = map_pat_721a115 env v1 in
  let v2 =
    (match v2 with
    | Some tok -> R.Option (Some (
        (* preproc_arg *) token env tok
      ))
    | None -> R.Option None)
  in
  let v3 = map_pat_1d78758 env v3 in
  R.Tuple [v1; v2; v3]

let map_preproc_warning (env : env) ((v1, v2, v3) : CST.preproc_warning) =
  let v1 = map_pat_ca788a8 env v1 in
  let v2 = (* preproc_arg *) token env v2 in
  let v3 = map_pat_1d78758 env v3 in
  R.Tuple [v1; v2; v3]

let map_preproc_error (env : env) ((v1, v2, v3) : CST.preproc_error) =
  let v1 = map_pat_8a5c9c8 env v1 in
  let v2 = (* preproc_arg *) token env v2 in
  let v3 = map_pat_1d78758 env v3 in
  R.Tuple [v1; v2; v3]

let map_preproc_line (env : env) ((v1, v2, v3) : CST.preproc_line) =
  let v1 = map_pat_d299690 env v1 in
  let v2 =
    (match v2 with
    | `Defa tok -> R.Case ("Defa",
        (* "default" *) token env tok
      )
    | `Hidden tok -> R.Case ("Hidden",
        (* "hidden" *) token env tok
      )
    | `Int_lit_opt_str_lit (v1, v2) -> R.Case ("Int_lit_opt_str_lit",
        let v1 = (* integer_literal *) token env v1 in
        let v2 =
          (match v2 with
          | Some x -> R.Option (Some (
              map_string_literal env x
            ))
          | None -> R.Option None)
        in
        R.Tuple [v1; v2]
      )
    | `LPAR_int_lit_COMMA_int_lit_RPAR_DASH_LPAR_int_lit_COMMA_int_lit_RPAR_opt_int_lit_str_lit (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13) -> R.Case ("LPAR_int_lit_COMMA_int_lit_RPAR_DASH_LPAR_int_lit_COMMA_int_lit_RPAR_opt_int_lit_str_lit",
        let v1 = (* "(" *) token env v1 in
        let v2 = (* integer_literal *) token env v2 in
        let v3 = (* "," *) token env v3 in
        let v4 = (* integer_literal *) token env v4 in
        let v5 = (* ")" *) token env v5 in
        let v6 = (* "-" *) token env v6 in
        let v7 = (* "(" *) token env v7 in
        let v8 = (* integer_literal *) token env v8 in
        let v9 = (* "," *) token env v9 in
        let v10 = (* integer_literal *) token env v10 in
        let v11 = (* ")" *) token env v11 in
        let v12 =
          (match v12 with
          | Some tok -> R.Option (Some (
              (* integer_literal *) token env tok
            ))
          | None -> R.Option None)
        in
        let v13 = map_string_literal env v13 in
        R.Tuple [v1; v2; v3; v4; v5; v6; v7; v8; v9; v10; v11; v12; v13]
      )
    )
  in
  let v3 = map_pat_1d78758 env v3 in
  R.Tuple [v1; v2; v3]

let map_preproc_pragma (env : env) ((v1, v2, v3) : CST.preproc_pragma) =
  let v1 = map_pat_eb92600 env v1 in
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
              let v1 = map_anon_choice_id_f2cdd14 env v1 in
              let v2 =
                R.List (List.map (fun (v1, v2) ->
                  let v1 = (* "," *) token env v1 in
                  let v2 = map_anon_choice_id_f2cdd14 env v2 in
                  R.Tuple [v1; v2]
                ) v2)
              in
              R.Tuple [v1; v2]
            ))
          | None -> R.Option None)
        in
        R.Tuple [v1; v2; v3]
      )
    | `Chec_str_lit_str_lit_str_lit (v1, v2, v3, v4) -> R.Case ("Chec_str_lit_str_lit_str_lit",
        let v1 = (* "checksum" *) token env v1 in
        let v2 = map_string_literal env v2 in
        let v3 = map_string_literal env v3 in
        let v4 = map_string_literal env v4 in
        R.Tuple [v1; v2; v3; v4]
      )
    )
  in
  let v3 = map_pat_1d78758 env v3 in
  R.Tuple [v1; v2; v3]

let dump_tree root =
  map_compilation_unit () root
  |> Tree_sitter_run.Raw_tree.to_channel stdout

let map_extra (env : env) (x : CST.extra) =
  match x with
  | `Comment (_loc, x) -> ("comment", "comment", map_comment env x)
  | `Preproc_region (_loc, x) -> ("preproc_region", "preproc_region", map_preproc_region env x)
  | `Preproc_endregion (_loc, x) -> ("preproc_endregion", "preproc_endregion", map_preproc_endregion env x)
  | `Preproc_line (_loc, x) -> ("preproc_line", "preproc_line", map_preproc_line env x)
  | `Preproc_pragma (_loc, x) -> ("preproc_pragma", "preproc_pragma", map_preproc_pragma env x)
  | `Preproc_nullable (_loc, x) -> ("preproc_nullable", "preproc_nullable", map_preproc_nullable env x)
  | `Preproc_error (_loc, x) -> ("preproc_error", "preproc_error", map_preproc_error env x)
  | `Preproc_warning (_loc, x) -> ("preproc_warning", "preproc_warning", map_preproc_warning env x)
  | `Preproc_define (_loc, x) -> ("preproc_define", "preproc_define", map_preproc_define env x)
  | `Preproc_undef (_loc, x) -> ("preproc_undef", "preproc_undef", map_preproc_undef env x)

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
