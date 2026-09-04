(**
   Boilerplate to be used as a template when mapping the ocaml CST
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

let map_pat_d43393f (env : env) (tok : CST.pat_d43393f) =
  (* pattern "[^\\\\']" *) token env tok

let map_concat_operator (env : env) (tok : CST.concat_operator) =
  (* concat_operator *) token env tok

let map_let_operator (env : env) (tok : CST.let_operator) =
  (* let_operator *) token env tok

let map_conversion_specification (env : env) (tok : CST.conversion_specification) =
  (* conversion_specification *) token env tok

let map_or_operator (env : env) (tok : CST.or_operator) =
  (* or_operator *) token env tok

let map_tok_choice_plus_rep1_pat_2ed1ddf (env : env) (tok : CST.tok_choice_plus_rep1_pat_2ed1ddf) =
  (* tok_choice_plus_rep1_pat_2ed1ddf *) token env tok

let map_boolean (env : env) (x : CST.boolean) =
  (match x with
  | `True tok -> R.Case ("True",
      (* "true" *) token env tok
    )
  | `False tok -> R.Case ("False",
      (* "false" *) token env tok
    )
  )

let map_pat_a1391b9 (env : env) (tok : CST.pat_a1391b9) =
  (* pattern \. *) token env tok

let map_tok_choice_pat_4349e4b (env : env) (tok : CST.tok_choice_pat_4349e4b) =
  (* tok_choice_pat_4349e4b *) token env tok

let map_pat_3bf11d1 (env : env) (tok : CST.pat_3bf11d1) =
  (* pattern \\x[0-9A-Fa-f][0-9A-Fa-f] *) token env tok

let map_pat_9465c8b (env : env) (tok : CST.pat_9465c8b) =
  (* pattern \\\n[\t ]* *) token env tok

let map_pat_dece469 (env : env) (tok : CST.pat_dece469) =
  (* pattern [+-] *) token env tok

let map_pat_86b875b (env : env) (tok : CST.pat_86b875b) =
  (* pattern \\[0-9][0-9][0-9] *) token env tok

let map_pat_3d340f6 (env : env) (tok : CST.pat_3d340f6) =
  (* pattern \s+ *) token env tok

let map_and_operator (env : env) (tok : CST.and_operator) =
  (* and_operator *) token env tok

let map_capitalized_identifier (env : env) (tok : CST.capitalized_identifier) =
  (* pattern "[A-Z][a-zA-Z0-9_']*" *) token env tok

let map_shebang (env : env) (tok : CST.shebang) =
  (* pattern #!.* *) token env tok

let map_unit_ (env : env) (x : CST.unit_) =
  (match x with
  | `LPAR_RPAR (v1, v2) -> R.Case ("LPAR_RPAR",
      let v1 = (* "(" *) token env v1 in
      let v2 = (* ")" *) token env v2 in
      R.Tuple [v1; v2]
    )
  | `Begin_end (v1, v2) -> R.Case ("Begin_end",
      let v1 = (* "begin" *) token env v1 in
      let v2 = (* "end" *) token env v2 in
      R.Tuple [v1; v2]
    )
  )

let map_hash_operator (env : env) (tok : CST.hash_operator) =
  (* hash_operator *) token env tok

let map_pat_833344d (env : env) (tok : CST.pat_833344d) =
  (* pattern ` *) token env tok

let map_mult_operator (env : env) (tok : CST.mult_operator) =
  (* mult_operator *) token env tok

let map_pat_3590cb8 (env : env) (tok : CST.pat_3590cb8) =
  (* pattern "'" *) token env tok

let map_pretty_printing_indication (env : env) (tok : CST.pretty_printing_indication) =
  (* pattern @([\[\], ;.{}?]|\\n|<[0-9]+>) *) token env tok

let map_assign_operator (env : env) (tok : CST.assign_operator) =
  (* pattern := *) token env tok

let map_anon_choice_muta_d43fe41 (env : env) (x : CST.anon_choice_muta_d43fe41) =
  (match x with
  | `Muta tok -> R.Case ("Muta",
      (* "mutable" *) token env tok
    )
  | `Virt tok -> R.Case ("Virt",
      (* "virtual" *) token env tok
    )
  )

let map_match_operator (env : env) (tok : CST.match_operator) =
  (* match_operator *) token env tok

let map_imm_tok_pat_dcdac4f (env : env) (tok : CST.imm_tok_pat_dcdac4f) =
  (* pattern \s *) token env tok

let map_pat_21333c0 (env : env) (tok : CST.pat_21333c0) =
  (* pattern \\o[0-3][0-7][0-7] *) token env tok

let map_number (env : env) (tok : CST.number) =
  (* number *) token env tok

let map_null (env : env) (tok : CST.null) =
  (* null *) token env tok

let map_anon_choice_TILDE_72781e5 (env : env) (x : CST.anon_choice_TILDE_72781e5) =
  (match x with
  | `TILDE tok -> R.Case ("TILDE",
      (* "~" *) token env tok
    )
  | `QMARK tok -> R.Case ("QMARK",
      (* "?" *) token env tok
    )
  )

let map_imm_tok_colon (env : env) (tok : CST.imm_tok_colon) =
  (* ":" *) token env tok

let map_ocamlyacc_value (env : env) (tok : CST.ocamlyacc_value) =
  (* pattern \$[0-9]+ *) token env tok

let map_prefix_operator (env : env) (tok : CST.prefix_operator) =
  (* prefix_operator *) token env tok

let map_pat_714c625 (env : env) (tok : CST.pat_714c625) =
  (* pattern [^%@|]+|%|@|\| *) token env tok

let map_pat_79c7248 (env : env) (tok : CST.pat_79c7248) =
  (* pattern [+-]\. *) token env tok

let map_let_and_operator (env : env) (tok : CST.let_and_operator) =
  (* let_and_operator *) token env tok

let map_pat_60fc52b (env : env) (tok : CST.pat_60fc52b) =
  (* pattern "\\\\[\\\\\"'ntbr ]" *) token env tok

let map_pat_01abfc7 (env : env) (tok : CST.pat_01abfc7) =
  (* pattern # *) token env tok

let map_left_quoted_string_delimiter (env : env) (tok : CST.left_quoted_string_delimiter) =
  (* left_quoted_string_delimiter *) token env tok

let map_pat_6cdf4be (env : env) (tok : CST.pat_6cdf4be) =
  (* pattern \\u\{[0-9A-Fa-f]+\} *) token env tok

let map_anon_choice_EQ_4ccabd6 (env : env) (x : CST.anon_choice_EQ_4ccabd6) =
  (match x with
  | `EQ tok -> R.Case ("EQ",
      (* "=" *) token env tok
    )
  | `COLONEQ tok -> R.Case ("COLONEQ",
      (* ":=" *) token env tok
    )
  )

let map_pat_6c51254 (env : env) (tok : CST.pat_6c51254) =
  (* pattern \[@ *) token env tok

let map_identifier (env : env) (tok : CST.identifier) =
  (* pattern "[a-z_][a-zA-Z0-9_']*" *) token env tok

let map_right_quoted_string_delimiter (env : env) (tok : CST.right_quoted_string_delimiter) =
  (* right_quoted_string_delimiter *) token env tok

let map_indexing_operator (env : env) (tok : CST.indexing_operator) =
  (* indexing_operator *) token env tok

let map_rel_operator (env : env) (tok : CST.rel_operator) =
  (* rel_operator *) token env tok

let map_anon_choice_priv_c7cc539 (env : env) (x : CST.anon_choice_priv_c7cc539) =
  (match x with
  | `Priv tok -> R.Case ("Priv",
      (* "private" *) token env tok
    )
  | `Virt tok -> R.Case ("Virt",
      (* "virtual" *) token env tok
    )
  )

let map_imm_tok_pat_6c51254 (env : env) (tok : CST.imm_tok_pat_6c51254) =
  (* pattern \[@ *) token env tok

let map_pat_19aaf34 (env : env) (tok : CST.pat_19aaf34) =
  (* pattern "[^\\\\\"%@]+|%|@" *) token env tok

let map_pow_operator (env : env) (tok : CST.pow_operator) =
  (* pow_operator *) token env tok

let map_add_operator (env : env) (x : CST.add_operator) =
  (match x with
  | `Pat_dece469 x -> R.Case ("Pat_dece469",
      map_pat_dece469 env x
    )
  | `Pat_79c7248 x -> R.Case ("Pat_79c7248",
      map_pat_79c7248 env x
    )
  | `Tok_choice_plus_rep1_pat_2ed1ddf x -> R.Case ("Tok_choice_plus_rep1_pat_2ed1ddf",
      map_tok_choice_plus_rep1_pat_2ed1ddf env x
    )
  )

let map_sign_operator (env : env) (x : CST.sign_operator) =
  (match x with
  | `Pat_dece469 x -> R.Case ("Pat_dece469",
      map_pat_dece469 env x
    )
  | `Pat_79c7248 x -> R.Case ("Pat_79c7248",
      map_pat_79c7248 env x
    )
  )

let map_escape_sequence (env : env) (x : CST.escape_sequence) =
  (match x with
  | `Pat_60fc52b x -> R.Case ("Pat_60fc52b",
      map_pat_60fc52b env x
    )
  | `Pat_86b875b x -> R.Case ("Pat_86b875b",
      map_pat_86b875b env x
    )
  | `Pat_3bf11d1 x -> R.Case ("Pat_3bf11d1",
      map_pat_3bf11d1 env x
    )
  | `Pat_21333c0 x -> R.Case ("Pat_21333c0",
      map_pat_21333c0 env x
    )
  )

let map_quoted_string_content (env : env) (xs : CST.quoted_string_content) =
  R.List (List.map (fun x ->
    (match x with
    | `Imm_tok_pat_dcdac4f x -> R.Case ("Imm_tok_pat_dcdac4f",
        map_imm_tok_pat_dcdac4f env x
      )
    | `Imm_tok_pat_6c51254 x -> R.Case ("Imm_tok_pat_6c51254",
        map_imm_tok_pat_6c51254 env x
      )
    | `Pat_714c625 x -> R.Case ("Pat_714c625",
        map_pat_714c625 env x
      )
    | `Null tok -> R.Case ("Null",
        (* null *) token env tok
      )
    | `Conv_spec tok -> R.Case ("Conv_spec",
        (* conversion_specification *) token env tok
      )
    | `Pretty_prin_indi tok -> R.Case ("Pretty_prin_indi",
        (* pattern @([\[\], ;.{}?]|\\n|<[0-9]+>) *) token env tok
      )
    )
  ) xs)

let map_anon_choice_module_name_7ad5569 (env : env) (x : CST.anon_choice_module_name_7ad5569) =
  (match x with
  | `Capi_id tok -> R.Case ("Capi_id",
      (* pattern "[A-Z][a-zA-Z0-9_']*" *) token env tok
    )
  | `X__ tok -> R.Case ("X__",
      (* "_" *) token env tok
    )
  )

let rec map_module_path (env : env) (x : CST.module_path) =
  (match x with
  | `Capi_id tok -> R.Case ("Capi_id",
      (* pattern "[A-Z][a-zA-Z0-9_']*" *) token env tok
    )
  | `Module_path_DOT_capi_id (v1, v2, v3) -> R.Case ("Module_path_DOT_capi_id",
      let v1 = map_module_path env v1 in
      let v2 = (* "." *) token env v2 in
      let v3 = (* pattern "[A-Z][a-zA-Z0-9_']*" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

let map_constructor_name (env : env) (x : CST.constructor_name) =
  (match x with
  | `Capi_id tok -> R.Case ("Capi_id",
      (* pattern "[A-Z][a-zA-Z0-9_']*" *) token env tok
    )
  | `LPAR_COLONCOLON_RPAR (v1, v2, v3) -> R.Case ("LPAR_COLONCOLON_RPAR",
      let v1 = (* "(" *) token env v1 in
      let v2 = (* "::" *) token env v2 in
      let v3 = (* ")" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

let rec map_extended_module_path (env : env) (x : CST.extended_module_path) =
  (match x with
  | `Choice_capi_id x -> R.Case ("Choice_capi_id",
      (match x with
      | `Capi_id tok -> R.Case ("Capi_id",
          (* pattern "[A-Z][a-zA-Z0-9_']*" *) token env tok
        )
      | `Exte_module_path_DOT_capi_id (v1, v2, v3) -> R.Case ("Exte_module_path_DOT_capi_id",
          let v1 = map_extended_module_path env v1 in
          let v2 = (* "." *) token env v2 in
          let v3 = (* pattern "[A-Z][a-zA-Z0-9_']*" *) token env v3 in
          R.Tuple [v1; v2; v3]
        )
      )
    )
  | `Exte_module_path_LPAR_exte_module_path_RPAR (v1, v2, v3, v4) -> R.Case ("Exte_module_path_LPAR_exte_module_path_RPAR",
      let v1 = map_extended_module_path env v1 in
      let v2 = (* "(" *) token env v2 in
      let v3 = map_extended_module_path env v3 in
      let v4 = (* ")" *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  )

let map_infix_operator (env : env) (x : CST.infix_operator) =
  (match x with
  | `Pow_op tok -> R.Case ("Pow_op",
      (* pow_operator *) token env tok
    )
  | `Mult_op tok -> R.Case ("Mult_op",
      (* mult_operator *) token env tok
    )
  | `Add_op x -> R.Case ("Add_op",
      map_add_operator env x
    )
  | `Concat_op tok -> R.Case ("Concat_op",
      (* concat_operator *) token env tok
    )
  | `Rel_op tok -> R.Case ("Rel_op",
      (* rel_operator *) token env tok
    )
  | `And_op tok -> R.Case ("And_op",
      (* and_operator *) token env tok
    )
  | `Or_op tok -> R.Case ("Or_op",
      (* or_operator *) token env tok
    )
  | `Assign_op tok -> R.Case ("Assign_op",
      (* pattern := *) token env tok
    )
  )

let map_character_content (env : env) (x : CST.character_content) =
  (match x with
  | `Pat_d43393f x -> R.Case ("Pat_d43393f",
      map_pat_d43393f env x
    )
  | `Null tok -> R.Case ("Null",
      (* null *) token env tok
    )
  | `Esc_seq x -> R.Case ("Esc_seq",
      map_escape_sequence env x
    )
  )

let map_string_content (env : env) (xs : CST.string_content) =
  R.List (List.map (fun x ->
    (match x with
    | `Imm_tok_pat_dcdac4f x -> R.Case ("Imm_tok_pat_dcdac4f",
        map_imm_tok_pat_dcdac4f env x
      )
    | `Imm_tok_pat_6c51254 x -> R.Case ("Imm_tok_pat_6c51254",
        map_imm_tok_pat_6c51254 env x
      )
    | `Pat_19aaf34 x -> R.Case ("Pat_19aaf34",
        map_pat_19aaf34 env x
      )
    | `Null tok -> R.Case ("Null",
        (* null *) token env tok
      )
    | `Esc_seq x -> R.Case ("Esc_seq",
        map_escape_sequence env x
      )
    | `Pat_6cdf4be x -> R.Case ("Pat_6cdf4be",
        map_pat_6cdf4be env x
      )
    | `Pat_9465c8b x -> R.Case ("Pat_9465c8b",
        map_pat_9465c8b env x
      )
    | `Conv_spec tok -> R.Case ("Conv_spec",
        (* conversion_specification *) token env tok
      )
    | `Pretty_prin_indi tok -> R.Case ("Pretty_prin_indi",
        (* pattern @([\[\], ;.{}?]|\\n|<[0-9]+>) *) token env tok
      )
    )
  ) xs)

let map_label (env : env) ((v1, v2) : CST.label) =
  let v1 = map_anon_choice_TILDE_72781e5 env v1 in
  let v2 =
    (* pattern "[a-z_][a-zA-Z0-9_']*" *) token env v2
  in
  R.Tuple [v1; v2]

let map_module_type_name (env : env) (x : CST.module_type_name) =
  (match x with
  | `Capi_id tok -> R.Case ("Capi_id",
      (* pattern "[A-Z][a-zA-Z0-9_']*" *) token env tok
    )
  | `Id tok -> R.Case ("Id",
      (* pattern "[a-z_][a-zA-Z0-9_']*" *) token env tok
    )
  )

let map_abstract_type (env : env) ((v1, v2) : CST.abstract_type) =
  let v1 = (* "type" *) token env v1 in
  let v2 =
    R.List (List.map (token env (* pattern "[a-z_][a-zA-Z0-9_']*" *)) v2)
  in
  R.Tuple [v1; v2]

let map_anon_choice_meth_name_cbd841f (env : env) (x : CST.anon_choice_meth_name_cbd841f) =
  (match x with
  | `Id tok -> R.Case ("Id",
      (* pattern "[a-z_][a-zA-Z0-9_']*" *) token env tok
    )
  | `Capi_id tok -> R.Case ("Capi_id",
      (* pattern "[A-Z][a-zA-Z0-9_']*" *) token env tok
    )
  )

let map_indexing_operator_path (env : env) (x : CST.indexing_operator_path) =
  (match x with
  | `Inde_op tok -> R.Case ("Inde_op",
      (* indexing_operator *) token env tok
    )
  | `Module_path_DOT_inde_op (v1, v2, v3) -> R.Case ("Module_path_DOT_inde_op",
      let v1 = map_module_path env v1 in
      let v2 = (* "." *) token env v2 in
      let v3 = (* indexing_operator *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

let map_field_path (env : env) (x : CST.field_path) =
  (match x with
  | `Id tok -> R.Case ("Id",
      (* pattern "[a-z_][a-zA-Z0-9_']*" *) token env tok
    )
  | `Module_path_DOT_id (v1, v2, v3) -> R.Case ("Module_path_DOT_id",
      let v1 = map_module_path env v1 in
      let v2 = (* "." *) token env v2 in
      let v3 =
        (* pattern "[a-z_][a-zA-Z0-9_']*" *) token env v3
      in
      R.Tuple [v1; v2; v3]
    )
  )

let map_class_path (env : env) (x : CST.class_path) =
  (match x with
  | `Id tok -> R.Case ("Id",
      (* pattern "[a-z_][a-zA-Z0-9_']*" *) token env tok
    )
  | `Module_path_DOT_id (v1, v2, v3) -> R.Case ("Module_path_DOT_id",
      let v1 = map_module_path env v1 in
      let v2 = (* "." *) token env v2 in
      let v3 =
        (* pattern "[a-z_][a-zA-Z0-9_']*" *) token env v3
      in
      R.Tuple [v1; v2; v3]
    )
  )

let map_constructor_path (env : env) (x : CST.constructor_path) =
  (match x with
  | `Choice_capi_id x -> R.Case ("Choice_capi_id",
      map_constructor_name env x
    )
  | `Module_path_DOT_choice_capi_id (v1, v2, v3) -> R.Case ("Module_path_DOT_choice_capi_id",
      let v1 = map_module_path env v1 in
      let v2 = (* "." *) token env v2 in
      let v3 = map_constructor_name env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

let map_type_constructor_path (env : env) (x : CST.type_constructor_path) =
  (match x with
  | `Id tok -> R.Case ("Id",
      (* pattern "[a-z_][a-zA-Z0-9_']*" *) token env tok
    )
  | `Exte_module_path_DOT_id (v1, v2, v3) -> R.Case ("Exte_module_path_DOT_id",
      let v1 = map_extended_module_path env v1 in
      let v2 = (* "." *) token env v2 in
      let v3 =
        (* pattern "[a-z_][a-zA-Z0-9_']*" *) token env v3
      in
      R.Tuple [v1; v2; v3]
    )
  )

let map_class_type_path (env : env) (x : CST.class_type_path) =
  (match x with
  | `Id tok -> R.Case ("Id",
      (* pattern "[a-z_][a-zA-Z0-9_']*" *) token env tok
    )
  | `Exte_module_path_DOT_id (v1, v2, v3) -> R.Case ("Exte_module_path_DOT_id",
      let v1 = map_extended_module_path env v1 in
      let v2 = (* "." *) token env v2 in
      let v3 =
        (* pattern "[a-z_][a-zA-Z0-9_']*" *) token env v3
      in
      R.Tuple [v1; v2; v3]
    )
  )

let map_parenthesized_operator (env : env) ((v1, v2, v3) : CST.parenthesized_operator) =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    (match v2 with
    | `Prefix_op tok -> R.Case ("Prefix_op",
        (* prefix_operator *) token env tok
      )
    | `Infix_op x -> R.Case ("Infix_op",
        map_infix_operator env x
      )
    | `Hash_op tok -> R.Case ("Hash_op",
        (* hash_operator *) token env tok
      )
    | `DOT_inde_op_choice_LPAR_opt_SEMI_DOTDOT_RPAR_opt_LTDASH (v1, v2, v3, v4) -> R.Case ("DOT_inde_op_choice_LPAR_opt_SEMI_DOTDOT_RPAR_opt_LTDASH",
        let v1 = (* "." *) token env v1 in
        let v2 = (* indexing_operator *) token env v2 in
        let v3 =
          (match v3 with
          | `LPAR_opt_SEMI_DOTDOT_RPAR (v1, v2, v3) -> R.Case ("LPAR_opt_SEMI_DOTDOT_RPAR",
              let v1 = (* "(" *) token env v1 in
              let v2 =
                (match v2 with
                | Some (v1, v2) -> R.Option (Some (
                    let v1 = (* ";" *) token env v1 in
                    let v2 = (* ".." *) token env v2 in
                    R.Tuple [v1; v2]
                  ))
                | None -> R.Option None)
              in
              let v3 = (* ")" *) token env v3 in
              R.Tuple [v1; v2; v3]
            )
          | `LBRACK_opt_SEMI_DOTDOT_RBRACK (v1, v2, v3) -> R.Case ("LBRACK_opt_SEMI_DOTDOT_RBRACK",
              let v1 = (* "[" *) token env v1 in
              let v2 =
                (match v2 with
                | Some (v1, v2) -> R.Option (Some (
                    let v1 = (* ";" *) token env v1 in
                    let v2 = (* ".." *) token env v2 in
                    R.Tuple [v1; v2]
                  ))
                | None -> R.Option None)
              in
              let v3 = (* "]" *) token env v3 in
              R.Tuple [v1; v2; v3]
            )
          | `LCURL_opt_SEMI_DOTDOT_RCURL (v1, v2, v3) -> R.Case ("LCURL_opt_SEMI_DOTDOT_RCURL",
              let v1 = (* "{" *) token env v1 in
              let v2 =
                (match v2 with
                | Some (v1, v2) -> R.Option (Some (
                    let v1 = (* ";" *) token env v1 in
                    let v2 = (* ".." *) token env v2 in
                    R.Tuple [v1; v2]
                  ))
                | None -> R.Option None)
              in
              let v3 = (* "}" *) token env v3 in
              R.Tuple [v1; v2; v3]
            )
          )
        in
        let v4 =
          (match v4 with
          | Some tok -> R.Option (Some (
              (* "<-" *) token env tok
            ))
          | None -> R.Option None)
        in
        R.Tuple [v1; v2; v3; v4]
      )
    | `Let_op tok -> R.Case ("Let_op",
        (* let_operator *) token env tok
      )
    | `Let_and_op tok -> R.Case ("Let_and_op",
        (* let_and_operator *) token env tok
      )
    | `Match_op tok -> R.Case ("Match_op",
        (* match_operator *) token env tok
      )
    )
  in
  let v3 = (* ")" *) token env v3 in
  R.Tuple [v1; v2; v3]

let map_string_ (env : env) ((v1, v2, v3) : CST.string_) =
  let v1 = (* "\"" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_string_content env x
      ))
    | None -> R.Option None)
  in
  let v3 = (* "\"" *) token env v3 in
  R.Tuple [v1; v2; v3]

let map_module_type_path (env : env) (x : CST.module_type_path) =
  (match x with
  | `Choice_capi_id x -> R.Case ("Choice_capi_id",
      map_module_type_name env x
    )
  | `Exte_module_path_DOT_choice_capi_id (v1, v2, v3) -> R.Case ("Exte_module_path_DOT_choice_capi_id",
      let v1 = map_extended_module_path env v1 in
      let v2 = (* "." *) token env v2 in
      let v3 = map_module_type_name env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

let map_parenthesized_abstract_type (env : env) ((v1, v2, v3) : CST.parenthesized_abstract_type) =
  let v1 = (* "(" *) token env v1 in
  let v2 = map_abstract_type env v2 in
  let v3 = (* ")" *) token env v3 in
  R.Tuple [v1; v2; v3]

let map_type_variable (env : env) ((v1, v2) : CST.type_variable) =
  let v1 = map_pat_3590cb8 env v1 in
  let v2 = map_anon_choice_meth_name_cbd841f env v2 in
  R.Tuple [v1; v2]

let map_attribute_id (env : env) ((v1, v2) : CST.attribute_id) =
  let v1 = map_anon_choice_meth_name_cbd841f env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = map_pat_a1391b9 env v1 in
      let v2 = map_anon_choice_meth_name_cbd841f env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  R.Tuple [v1; v2]

let map_directive (env : env) ((v1, v2) : CST.directive) =
  let v1 = map_pat_01abfc7 env v1 in
  let v2 = map_anon_choice_meth_name_cbd841f env v2 in
  R.Tuple [v1; v2]

let map_tag (env : env) ((v1, v2) : CST.tag) =
  let v1 = map_pat_833344d env v1 in
  let v2 = map_anon_choice_meth_name_cbd841f env v2 in
  R.Tuple [v1; v2]

let map_polymorphic_variant_pattern (env : env) ((v1, v2) : CST.polymorphic_variant_pattern) =
  let v1 = (* "#" *) token env v1 in
  let v2 = map_type_constructor_path env v2 in
  R.Tuple [v1; v2]

let map_value_name (env : env) (x : CST.value_name) =
  (match x with
  | `Id tok -> R.Case ("Id",
      (* pattern "[a-z_][a-zA-Z0-9_']*" *) token env tok
    )
  | `Paren_op x -> R.Case ("Paren_op",
      map_parenthesized_operator env x
    )
  )

let map_value_pattern (env : env) (x : CST.value_pattern) =
  (match x with
  | `Id tok -> R.Case ("Id",
      (* pattern "[a-z_][a-zA-Z0-9_']*" *) token env tok
    )
  | `Paren_op x -> R.Case ("Paren_op",
      map_parenthesized_operator env x
    )
  )

let map_constant (env : env) (x : CST.constant) =
  (match x with
  | `Num tok -> R.Case ("Num",
      (* number *) token env tok
    )
  | `Char (v1, v2, v3) -> R.Case ("Char",
      let v1 = (* "'" *) token env v1 in
      let v2 = map_character_content env v2 in
      let v3 = (* "'" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Str x -> R.Case ("Str",
      map_string_ env x
    )
  | `Quoted_str (v1, v2, v3, v4, v5) -> R.Case ("Quoted_str",
      let v1 = (* "{" *) token env v1 in
      let v2 = (* left_quoted_string_delimiter *) token env v2 in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_quoted_string_content env x
          ))
        | None -> R.Option None)
      in
      let v4 = (* right_quoted_string_delimiter *) token env v4 in
      let v5 = (* "}" *) token env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `Bool x -> R.Case ("Bool",
      map_boolean env x
    )
  | `Unit x -> R.Case ("Unit",
      map_unit_ env x
    )
  )

let map_type_param (env : env) ((v1, v2) : CST.type_param) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        (match x with
        | `PLUS_opt_BANG (v1, v2) -> R.Case ("PLUS_opt_BANG",
            let v1 = (* "+" *) token env v1 in
            let v2 =
              (match v2 with
              | Some tok -> R.Option (Some (
                  (* "!" *) token env tok
                ))
              | None -> R.Option None)
            in
            R.Tuple [v1; v2]
          )
        | `DASH_opt_BANG (v1, v2) -> R.Case ("DASH_opt_BANG",
            let v1 = (* "-" *) token env v1 in
            let v2 =
              (match v2 with
              | Some tok -> R.Option (Some (
                  (* "!" *) token env tok
                ))
              | None -> R.Option None)
            in
            R.Tuple [v1; v2]
          )
        | `BANG_opt_choice_PLUS (v1, v2) -> R.Case ("BANG_opt_choice_PLUS",
            let v1 = (* "!" *) token env v1 in
            let v2 =
              (match v2 with
              | Some x -> R.Option (Some (
                  (match x with
                  | `PLUS tok -> R.Case ("PLUS",
                      (* "+" *) token env tok
                    )
                  | `DASH tok -> R.Case ("DASH",
                      (* "-" *) token env tok
                    )
                  )
                ))
              | None -> R.Option None)
            in
            R.Tuple [v1; v2]
          )
        )
      ))
    | None -> R.Option None)
  in
  let v2 =
    (match v2 with
    | `Type_var x -> R.Case ("Type_var",
        map_type_variable env x
      )
    | `X__ tok -> R.Case ("X__",
        (* "_" *) token env tok
      )
    )
  in
  R.Tuple [v1; v2]

let map_attribute (env : env) ((v1, v2) : CST.attribute) =
  let v1 = (* "%" *) token env v1 in
  let v2 = map_attribute_id env v2 in
  R.Tuple [v1; v2]

let map_value_path (env : env) (x : CST.value_path) =
  (match x with
  | `Value_name x -> R.Case ("Value_name",
      map_value_name env x
    )
  | `Module_path_DOT_value_name (v1, v2, v3) -> R.Case ("Module_path_DOT_value_name",
      let v1 = map_module_path env v1 in
      let v2 = (* "." *) token env v2 in
      let v3 = map_value_name env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

let map_signed_constant (env : env) (x : CST.signed_constant) =
  (match x with
  | `Cst x -> R.Case ("Cst",
      map_constant env x
    )
  | `Signed_num (v1, v2) -> R.Case ("Signed_num",
      let v1 = map_pat_dece469 env v1 in
      let v2 = map_tok_choice_pat_4349e4b env v2 in
      R.Tuple [v1; v2]
    )
  )

let map_anon_LBRACK_type_param_rep_COMMA_type_param_RBRACK_cea5434 (env : env) ((v1, v2, v3, v4) : CST.anon_LBRACK_type_param_rep_COMMA_type_param_RBRACK_cea5434) =
  let v1 = (* "[" *) token env v1 in
  let v2 = map_type_param env v2 in
  let v3 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_type_param env v2 in
      R.Tuple [v1; v2]
    ) v3)
  in
  let v4 = (* "]" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

let map_type_params (env : env) (x : CST.type_params) =
  (match x with
  | `Type_param x -> R.Case ("Type_param",
      map_type_param env x
    )
  | `LPAR_type_param_rep_COMMA_type_param_RPAR (v1, v2, v3, v4) -> R.Case ("LPAR_type_param_rep_COMMA_type_param_RPAR",
      let v1 = (* "(" *) token env v1 in
      let v2 = map_type_param env v2 in
      let v3 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = (* "," *) token env v1 in
          let v2 = map_type_param env v2 in
          R.Tuple [v1; v2]
        ) v3)
      in
      let v4 = (* ")" *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  )

let map_toplevel_directive (env : env) ((v1, v2) : CST.toplevel_directive) =
  let v1 = map_directive env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        (match x with
        | `Cst x -> R.Case ("Cst",
            map_constant env x
          )
        | `Value_path x -> R.Case ("Value_path",
            map_value_path env x
          )
        | `Module_path x -> R.Case ("Module_path",
            map_module_path env x
          )
        )
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2]

let map_range_pattern (env : env) ((v1, v2, v3) : CST.range_pattern) =
  let v1 = map_signed_constant env v1 in
  let v2 = (* ".." *) token env v2 in
  let v3 = map_signed_constant env v3 in
  R.Tuple [v1; v2; v3]

let rec map_anon_bind_pat_ext_rep_SEMI_bind_pat_ext_opt_SEMI_38caf30 (env : env) ((v1, v2, v3) : CST.anon_bind_pat_ext_rep_SEMI_bind_pat_ext_opt_SEMI_38caf30) =
  let v1 = map_binding_pattern_ext env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* ";" *) token env v1 in
      let v2 = map_binding_pattern_ext env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  let v3 =
    (match v3 with
    | Some tok -> R.Option (Some (
        (* ";" *) token env tok
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3]

and map_anon_choice_cons_type_771aabb (env : env) (x : CST.anon_choice_cons_type_771aabb) =
  (match x with
  | `Cons_type (v1, v2, v3, v4, v5) -> R.Case ("Cons_type",
      let v1 = (* "type" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_type_params env x
          ))
        | None -> R.Option None)
      in
      let v3 = map_type_constructor_path env v3 in
      let v4 = map_type_equation env v4 in
      let v5 = R.List (List.map (map_type_constraint env) v5) in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `Cons_module (v1, v2, v3, v4) -> R.Case ("Cons_module",
      let v1 = (* "module" *) token env v1 in
      let v2 = map_module_path env v2 in
      let v3 = map_anon_choice_EQ_4ccabd6 env v3 in
      let v4 = map_extended_module_path env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Cons_module_type (v1, v2, v3, v4, v5) -> R.Case ("Cons_module_type",
      let v1 = (* "module" *) token env v1 in
      let v2 = (* "type" *) token env v2 in
      let v3 = map_module_type_path env v3 in
      let v4 = map_anon_choice_EQ_4ccabd6 env v4 in
      let v5 = map_module_type_ext env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  )

and map_anon_choice_meth_type_345b567 (env : env) (x : CST.anon_choice_meth_type_345b567) =
  (match x with
  | `Meth_type (v1, v2) -> R.Case ("Meth_type",
      let v1 =
        (* pattern "[a-z_][a-zA-Z0-9_']*" *) token env v1
      in
      let v2 = map_polymorphic_typed env v2 in
      R.Tuple [v1; v2]
    )
  | `Choice_simple_type x -> R.Case ("Choice_simple_type",
      map_simple_type_ext env x
    )
  )

and map_anon_choice_simple_type_ext_30dd028 (env : env) (x : CST.anon_choice_simple_type_ext_30dd028) =
  (match x with
  | `Choice_simple_type x -> R.Case ("Choice_simple_type",
      map_simple_type_ext env x
    )
  | `LPAR_type_ext_rep_COMMA_type_ext_RPAR (v1, v2, v3, v4) -> R.Case ("LPAR_type_ext_rep_COMMA_type_ext_RPAR",
      let v1 = (* "(" *) token env v1 in
      let v2 = map_type_ext env v2 in
      let v3 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = (* "," *) token env v1 in
          let v2 = map_type_ext env v2 in
          R.Tuple [v1; v2]
        ) v3)
      in
      let v4 = (* ")" *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  )

and map_anon_cons_decl_rep_BAR_cons_decl_fc0ccc5 (env : env) ((v1, v2) : CST.anon_cons_decl_rep_BAR_cons_decl_fc0ccc5) =
  let v1 = map_constructor_declaration env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "|" *) token env v1 in
      let v2 = map_constructor_declaration env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  R.Tuple [v1; v2]

and map_anon_exp_ext_rep_SEMI_exp_ext_opt_SEMI_f0de170 (env : env) ((v1, v2, v3) : CST.anon_exp_ext_rep_SEMI_exp_ext_opt_SEMI_f0de170) =
  let v1 = map_expression_ext env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* ";" *) token env v1 in
      let v2 = map_expression_ext env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  let v3 =
    (match v3 with
    | Some tok -> R.Option (Some (
        (* ";" *) token env tok
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3]

and map_anon_pat_ext_rep_SEMI_pat_ext_opt_SEMI_3830e8c (env : env) ((v1, v2, v3) : CST.anon_pat_ext_rep_SEMI_pat_ext_opt_SEMI_3830e8c) =
  let v1 = map_pattern_ext env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* ";" *) token env v1 in
      let v2 = map_pattern_ext env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  let v3 =
    (match v3 with
    | Some tok -> R.Option (Some (
        (* ";" *) token env tok
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3]

and map_argument (env : env) (x : CST.argument) =
  (match x with
  | `Choice_simple_exp x -> R.Case ("Choice_simple_exp",
      map_simple_expression_ext env x
    )
  | `Labe_arg x -> R.Case ("Labe_arg",
      map_labeled_argument env x
    )
  )

and map_array_binding_pattern (env : env) ((v1, v2, v3) : CST.array_binding_pattern) =
  let v1 = (* "[|" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_anon_bind_pat_ext_rep_SEMI_bind_pat_ext_opt_SEMI_38caf30 env x
      ))
    | None -> R.Option None)
  in
  let v3 = (* "|]" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_array_expression (env : env) ((v1, v2, v3) : CST.array_expression) =
  let v1 = (* "[|" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_anon_exp_ext_rep_SEMI_exp_ext_opt_SEMI_f0de170 env x
      ))
    | None -> R.Option None)
  in
  let v3 = (* "|]" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_array_get_expression (env : env) ((v1, v2, v3, v4, v5, v6) : CST.array_get_expression) =
  let v1 = map_simple_expression_ext env v1 in
  let v2 = (* "." *) token env v2 in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_indexing_operator_path env x
      ))
    | None -> R.Option None)
  in
  let v4 = (* "(" *) token env v4 in
  let v5 = map_sequence_expression_ext env v5 in
  let v6 = (* ")" *) token env v6 in
  R.Tuple [v1; v2; v3; v4; v5; v6]

and map_array_pattern (env : env) ((v1, v2, v3) : CST.array_pattern) =
  let v1 = (* "[|" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_anon_pat_ext_rep_SEMI_pat_ext_opt_SEMI_3830e8c env x
      ))
    | None -> R.Option None)
  in
  let v3 = (* "|]" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_attribute_payload (env : env) (x : CST.attribute_payload) =
  (match x with
  | `Stru x -> R.Case ("Stru",
      map_structure env x
    )
  | `COLON_opt_choice_type_ext (v1, v2) -> R.Case ("COLON_opt_choice_type_ext",
      let v1 = (* ":" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            (match x with
            | `Type_ext x -> R.Case ("Type_ext",
                map_type_ext env x
              )
            | `Sign x -> R.Case ("Sign",
                map_signature env x
              )
            )
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2]
    )
  | `QMARK_pat_ext_opt_guard (v1, v2, v3) -> R.Case ("QMARK_pat_ext_opt_guard",
      let v1 = (* "?" *) token env v1 in
      let v2 = map_pattern_ext env v2 in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_guard env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3]
    )
  )

and map_bigarray_get_expression (env : env) ((v1, v2, v3, v4, v5, v6) : CST.bigarray_get_expression) =
  let v1 = map_simple_expression_ext env v1 in
  let v2 = (* "." *) token env v2 in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_indexing_operator_path env x
      ))
    | None -> R.Option None)
  in
  let v4 = (* "{" *) token env v4 in
  let v5 = map_sequence_expression_ext env v5 in
  let v6 = (* "}" *) token env v6 in
  R.Tuple [v1; v2; v3; v4; v5; v6]

and map_binding_pattern (env : env) (x : CST.binding_pattern) =
  (match x with
  | `Value_name x -> R.Case ("Value_name",
      map_value_name env x
    )
  | `Signed_cst x -> R.Case ("Signed_cst",
      map_signed_constant env x
    )
  | `Typed_bind_pat (v1, v2, v3, v4) -> R.Case ("Typed_bind_pat",
      let v1 = (* "(" *) token env v1 in
      let v2 = map_binding_pattern_ext env v2 in
      let v3 = map_typed env v3 in
      let v4 = (* ")" *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Cons_path x -> R.Case ("Cons_path",
      map_constructor_path env x
    )
  | `Tag x -> R.Case ("Tag",
      map_tag env x
    )
  | `Poly_vari_pat x -> R.Case ("Poly_vari_pat",
      map_polymorphic_variant_pattern env x
    )
  | `Record_bind_pat x -> R.Case ("Record_bind_pat",
      map_record_binding_pattern env x
    )
  | `List_bind_pat x -> R.Case ("List_bind_pat",
      map_list_binding_pattern env x
    )
  | `Array_bind_pat x -> R.Case ("Array_bind_pat",
      map_array_binding_pattern env x
    )
  | `Local_open_bind_pat (v1, v2, v3) -> R.Case ("Local_open_bind_pat",
      let v1 = map_module_path env v1 in
      let v2 = (* "." *) token env v2 in
      let v3 =
        (match v3 with
        | `LPAR_opt_bind_pat_ext_RPAR (v1, v2, v3) -> R.Case ("LPAR_opt_bind_pat_ext_RPAR",
            let v1 = (* "(" *) token env v1 in
            let v2 =
              (match v2 with
              | Some x -> R.Option (Some (
                  map_binding_pattern_ext env x
                ))
              | None -> R.Option None)
            in
            let v3 = (* ")" *) token env v3 in
            R.Tuple [v1; v2; v3]
          )
        | `List_bind_pat x -> R.Case ("List_bind_pat",
            map_list_binding_pattern env x
          )
        | `Array_bind_pat x -> R.Case ("Array_bind_pat",
            map_array_binding_pattern env x
          )
        | `Record_bind_pat x -> R.Case ("Record_bind_pat",
            map_record_binding_pattern env x
          )
        )
      in
      R.Tuple [v1; v2; v3]
    )
  | `Pack_pat x -> R.Case ("Pack_pat",
      map_package_pattern env x
    )
  | `Paren_bind_pat (v1, v2, v3) -> R.Case ("Paren_bind_pat",
      let v1 = (* "(" *) token env v1 in
      let v2 = map_binding_pattern_ext env v2 in
      let v3 = (* ")" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Alias_bind_pat (v1, v2, v3) -> R.Case ("Alias_bind_pat",
      let v1 = map_binding_pattern_ext env v1 in
      let v2 = (* "as" *) token env v2 in
      let v3 = map_value_name env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Or_bind_pat (v1, v2, v3) -> R.Case ("Or_bind_pat",
      let v1 = map_binding_pattern_ext env v1 in
      let v2 = (* "|" *) token env v2 in
      let v3 = map_binding_pattern_ext env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Cons_bind_pat_1ca6430 (v1, v2) -> R.Case ("Cons_bind_pat_1ca6430",
      let v1 = map_constructor_path env v1 in
      let v2 = map_binding_pattern_ext env v2 in
      R.Tuple [v1; v2]
    )
  | `Tag_bind_pat (v1, v2) -> R.Case ("Tag_bind_pat",
      let v1 = map_tag env v1 in
      let v2 = map_binding_pattern_ext env v2 in
      R.Tuple [v1; v2]
    )
  | `Tuple_bind_pat (v1, v2, v3) -> R.Case ("Tuple_bind_pat",
      let v1 = map_binding_pattern_ext env v1 in
      let v2 = (* "," *) token env v2 in
      let v3 = map_binding_pattern_ext env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Cons_bind_pat_f2d0ae9 (v1, v2, v3) -> R.Case ("Cons_bind_pat_f2d0ae9",
      let v1 = map_binding_pattern_ext env v1 in
      let v2 = (* "::" *) token env v2 in
      let v3 = map_binding_pattern_ext env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Range_pat x -> R.Case ("Range_pat",
      map_range_pattern env x
    )
  | `Lazy_bind_pat (v1, v2, v3) -> R.Case ("Lazy_bind_pat",
      let v1 = (* "lazy" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_attribute env x
          ))
        | None -> R.Option None)
      in
      let v3 = map_binding_pattern_ext env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_binding_pattern_ext (env : env) (x : CST.binding_pattern_ext) =
  (match x with
  | `Bind_pat x -> R.Case ("Bind_pat",
      map_binding_pattern env x
    )
  | `Exte x -> R.Case ("Exte",
      map_extension env x
    )
  )

and map_class_binding (env : env) ((v1, v2, v3, v4, v5, v6, v7) : CST.class_binding) =
  let v1 =
    (match v1 with
    | Some tok -> R.Option (Some (
        (* "virtual" *) token env tok
      ))
    | None -> R.Option None)
  in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_anon_LBRACK_type_param_rep_COMMA_type_param_RBRACK_cea5434 env x
      ))
    | None -> R.Option None)
  in
  let v3 =
    (* pattern "[a-z_][a-zA-Z0-9_']*" *) token env v3
  in
  let v4 = R.List (List.map (map_parameter env) v4) in
  let v5 =
    (match v5 with
    | Some x -> R.Option (Some (
        map_class_typed env x
      ))
    | None -> R.Option None)
  in
  let v6 =
    (match v6 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = (* "=" *) token env v1 in
        let v2 = map_class_expression_ext env v2 in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  let v7 = R.List (List.map (map_item_attribute env) v7) in
  R.Tuple [v1; v2; v3; v4; v5; v6; v7]

and map_class_definition (env : env) ((v1, v2, v3, v4) : CST.class_definition) =
  let v1 = (* "class" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_attribute env x
      ))
    | None -> R.Option None)
  in
  let v3 = map_class_binding env v3 in
  let v4 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "and" *) token env v1 in
      let v2 = map_class_binding env v2 in
      R.Tuple [v1; v2]
    ) v4)
  in
  R.Tuple [v1; v2; v3; v4]

and map_class_expression (env : env) (x : CST.class_expression) =
  (match x with
  | `Simple_class_exp x -> R.Case ("Simple_class_exp",
      map_simple_class_expression env x
    )
  | `Class_func (v1, v2, v3, v4) -> R.Case ("Class_func",
      let v1 = (* "fun" *) token env v1 in
      let v2 = R.List (List.map (map_parameter env) v2) in
      let v3 = (* "->" *) token env v3 in
      let v4 = map_class_expression_ext env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Class_app (v1, v2) -> R.Case ("Class_app",
      let v1 = map_simple_class_expression env v1 in
      let v2 = R.List (List.map (map_argument env) v2) in
      R.Tuple [v1; v2]
    )
  | `Let_class_exp (v1, v2, v3) -> R.Case ("Let_class_exp",
      let v1 = map_value_definition env v1 in
      let v2 = (* "in" *) token env v2 in
      let v3 = map_class_expression_ext env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Let_open_class_exp (v1, v2, v3, v4) -> R.Case ("Let_open_class_exp",
      let v1 = (* "let" *) token env v1 in
      let v2 = map_open_module env v2 in
      let v3 = (* "in" *) token env v3 in
      let v4 = map_class_expression_ext env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  )

and map_class_expression_ext (env : env) (x : CST.class_expression_ext) =
  (match x with
  | `Class_exp x -> R.Case ("Class_exp",
      map_class_expression env x
    )
  | `Exte x -> R.Case ("Exte",
      map_extension env x
    )
  )

and map_class_field (env : env) (x : CST.class_field) =
  (match x with
  | `Inhe_defi (v1, v2, v3, v4, v5) -> R.Case ("Inhe_defi",
      let v1 = (* "inherit" *) token env v1 in
      let v2 =
        (match v2 with
        | Some tok -> R.Option (Some (
            (* "!" *) token env tok
          ))
        | None -> R.Option None)
      in
      let v3 = map_class_expression_ext env v3 in
      let v4 =
        (match v4 with
        | Some (v1, v2) -> R.Option (Some (
            let v1 = (* "as" *) token env v1 in
            let v2 = map_value_pattern env v2 in
            R.Tuple [v1; v2]
          ))
        | None -> R.Option None)
      in
      let v5 = R.List (List.map (map_item_attribute env) v5) in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `Inst_var_defi (v1, v2, v3, v4, v5, v6, v7, v8) -> R.Case ("Inst_var_defi",
      let v1 = (* "val" *) token env v1 in
      let v2 =
        (match v2 with
        | Some tok -> R.Option (Some (
            (* "!" *) token env tok
          ))
        | None -> R.Option None)
      in
      let v3 =
        R.List (List.map (map_anon_choice_muta_d43fe41 env) v3)
      in
      let v4 =
        (* pattern "[a-z_][a-zA-Z0-9_']*" *) token env v4
      in
      let v5 =
        (match v5 with
        | Some x -> R.Option (Some (
            map_typed env x
          ))
        | None -> R.Option None)
      in
      let v6 =
        (match v6 with
        | Some (v1, v2) -> R.Option (Some (
            let v1 = (* ":>" *) token env v1 in
            let v2 = map_type_ext env v2 in
            R.Tuple [v1; v2]
          ))
        | None -> R.Option None)
      in
      let v7 =
        (match v7 with
        | Some (v1, v2) -> R.Option (Some (
            let v1 = (* "=" *) token env v1 in
            let v2 = map_sequence_expression_ext env v2 in
            R.Tuple [v1; v2]
          ))
        | None -> R.Option None)
      in
      let v8 = R.List (List.map (map_item_attribute env) v8) in
      R.Tuple [v1; v2; v3; v4; v5; v6; v7; v8]
    )
  | `Meth_defi (v1, v2, v3, v4, v5, v6, v7, v8) -> R.Case ("Meth_defi",
      let v1 = (* "method" *) token env v1 in
      let v2 =
        (match v2 with
        | Some tok -> R.Option (Some (
            (* "!" *) token env tok
          ))
        | None -> R.Option None)
      in
      let v3 =
        R.List (List.map (map_anon_choice_priv_c7cc539 env) v3)
      in
      let v4 =
        (* pattern "[a-z_][a-zA-Z0-9_']*" *) token env v4
      in
      let v5 = R.List (List.map (map_parameter env) v5) in
      let v6 =
        (match v6 with
        | Some x -> R.Option (Some (
            map_polymorphic_typed env x
          ))
        | None -> R.Option None)
      in
      let v7 =
        (match v7 with
        | Some (v1, v2) -> R.Option (Some (
            let v1 = (* "=" *) token env v1 in
            let v2 = map_sequence_expression_ext env v2 in
            R.Tuple [v1; v2]
          ))
        | None -> R.Option None)
      in
      let v8 = R.List (List.map (map_item_attribute env) v8) in
      R.Tuple [v1; v2; v3; v4; v5; v6; v7; v8]
    )
  | `Type_param_cons x -> R.Case ("Type_param_cons",
      map_type_parameter_constraint env x
    )
  | `Class_init (v1, v2, v3) -> R.Case ("Class_init",
      let v1 = (* "initializer" *) token env v1 in
      let v2 = map_sequence_expression_ext env v2 in
      let v3 = R.List (List.map (map_item_attribute env) v3) in
      R.Tuple [v1; v2; v3]
    )
  )

and map_class_field_ext (env : env) (x : CST.class_field_ext) =
  (match x with
  | `Class_field x -> R.Case ("Class_field",
      map_class_field env x
    )
  | `Item_exte x -> R.Case ("Item_exte",
      map_item_extension env x
    )
  )

and map_class_field_specification (env : env) (x : CST.class_field_specification) =
  (match x with
  | `Inhe_spec (v1, v2, v3) -> R.Case ("Inhe_spec",
      let v1 = (* "inherit" *) token env v1 in
      let v2 = map_simple_class_type_ext env v2 in
      let v3 = R.List (List.map (map_item_attribute env) v3) in
      R.Tuple [v1; v2; v3]
    )
  | `Inst_var_spec (v1, v2, v3, v4, v5) -> R.Case ("Inst_var_spec",
      let v1 = (* "val" *) token env v1 in
      let v2 =
        R.List (List.map (map_anon_choice_muta_d43fe41 env) v2)
      in
      let v3 =
        (* pattern "[a-z_][a-zA-Z0-9_']*" *) token env v3
      in
      let v4 = map_typed env v4 in
      let v5 = R.List (List.map (map_item_attribute env) v5) in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `Meth_spec (v1, v2, v3, v4, v5) -> R.Case ("Meth_spec",
      let v1 = (* "method" *) token env v1 in
      let v2 =
        R.List (List.map (map_anon_choice_priv_c7cc539 env) v2)
      in
      let v3 =
        (* pattern "[a-z_][a-zA-Z0-9_']*" *) token env v3
      in
      let v4 = map_polymorphic_typed env v4 in
      let v5 = R.List (List.map (map_item_attribute env) v5) in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `Type_param_cons x -> R.Case ("Type_param_cons",
      map_type_parameter_constraint env x
    )
  )

and map_class_field_specification_ext (env : env) (x : CST.class_field_specification_ext) =
  (match x with
  | `Class_field_spec x -> R.Case ("Class_field_spec",
      map_class_field_specification env x
    )
  | `Item_exte x -> R.Case ("Item_exte",
      map_item_extension env x
    )
  )

and map_class_type (env : env) (x : CST.class_type) =
  (match x with
  | `Simple_class_type x -> R.Case ("Simple_class_type",
      map_simple_class_type env x
    )
  | `Class_func_type (v1, v2, v3, v4) -> R.Case ("Class_func_type",
      let v1 =
        (match v1 with
        | Some (v1, v2, v3) -> R.Option (Some (
            let v1 =
              (match v1 with
              | Some tok -> R.Option (Some (
                  (* "?" *) token env tok
                ))
              | None -> R.Option None)
            in
            let v2 =
              (* pattern "[a-z_][a-zA-Z0-9_']*" *) token env v2
            in
            let v3 = (* ":" *) token env v3 in
            R.Tuple [v1; v2; v3]
          ))
        | None -> R.Option None)
      in
      let v2 = map_tuple_type_ext env v2 in
      let v3 = (* "->" *) token env v3 in
      let v4 = map_class_type_ext env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  )

and map_class_type_binding (env : env) ((v1, v2, v3, v4, v5, v6) : CST.class_type_binding) =
  let v1 =
    (match v1 with
    | Some tok -> R.Option (Some (
        (* "virtual" *) token env tok
      ))
    | None -> R.Option None)
  in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_anon_LBRACK_type_param_rep_COMMA_type_param_RBRACK_cea5434 env x
      ))
    | None -> R.Option None)
  in
  let v3 =
    (* pattern "[a-z_][a-zA-Z0-9_']*" *) token env v3
  in
  let v4 = (* "=" *) token env v4 in
  let v5 = map_simple_class_type_ext env v5 in
  let v6 = R.List (List.map (map_item_attribute env) v6) in
  R.Tuple [v1; v2; v3; v4; v5; v6]

and map_class_type_definition (env : env) ((v1, v2, v3, v4, v5) : CST.class_type_definition) =
  let v1 = (* "class" *) token env v1 in
  let v2 = (* "type" *) token env v2 in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_attribute env x
      ))
    | None -> R.Option None)
  in
  let v4 = map_class_type_binding env v4 in
  let v5 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "and" *) token env v1 in
      let v2 = map_class_type_binding env v2 in
      R.Tuple [v1; v2]
    ) v5)
  in
  R.Tuple [v1; v2; v3; v4; v5]

and map_class_type_ext (env : env) (x : CST.class_type_ext) =
  (match x with
  | `Class_type x -> R.Case ("Class_type",
      map_class_type env x
    )
  | `Exte x -> R.Case ("Exte",
      map_extension env x
    )
  )

and map_class_typed (env : env) ((v1, v2) : CST.class_typed) =
  let v1 = (* ":" *) token env v1 in
  let v2 = map_class_type_ext env v2 in
  R.Tuple [v1; v2]

and map_constructor_argument (env : env) (x : CST.constructor_argument) =
  (match x with
  | `Choice_simple_type_rep_STAR_choice_simple_type (v1, v2) -> R.Case ("Choice_simple_type_rep_STAR_choice_simple_type",
      let v1 = map_simple_type_ext env v1 in
      let v2 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = (* "*" *) token env v1 in
          let v2 = map_simple_type_ext env v2 in
          R.Tuple [v1; v2]
        ) v2)
      in
      R.Tuple [v1; v2]
    )
  | `Record_decl x -> R.Case ("Record_decl",
      map_record_declaration env x
    )
  )

and map_constructor_declaration (env : env) ((v1, v2) : CST.constructor_declaration) =
  let v1 =
    (match v1 with
    | `Choice_capi_id x -> R.Case ("Choice_capi_id",
        map_constructor_name env x
      )
    | `Choice_LBRACK_RBRACK x -> R.Case ("Choice_LBRACK_RBRACK",
        (match x with
        | `LBRACK_RBRACK (v1, v2) -> R.Case ("LBRACK_RBRACK",
            let v1 = (* "[" *) token env v1 in
            let v2 = (* "]" *) token env v2 in
            R.Tuple [v1; v2]
          )
        | `LPAR_RPAR (v1, v2) -> R.Case ("LPAR_RPAR",
            let v1 = (* "(" *) token env v1 in
            let v2 = (* ")" *) token env v2 in
            R.Tuple [v1; v2]
          )
        | `True tok -> R.Case ("True",
            (* "true" *) token env tok
          )
        | `False tok -> R.Case ("False",
            (* "false" *) token env tok
          )
        )
      )
    )
  in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        (match x with
        | `Of_cons_arg (v1, v2) -> R.Case ("Of_cons_arg",
            let v1 = (* "of" *) token env v1 in
            let v2 = map_constructor_argument env v2 in
            R.Tuple [v1; v2]
          )
        | `COLON_opt_rep1_type_var_DOT_opt_cons_arg_DASHGT_choice_simple_type (v1, v2, v3, v4) -> R.Case ("COLON_opt_rep1_type_var_DOT_opt_cons_arg_DASHGT_choice_simple_type",
            let v1 = (* ":" *) token env v1 in
            let v2 =
              (match v2 with
              | Some (v1, v2) -> R.Option (Some (
                  let v1 = R.List (List.map (map_type_variable env) v1) in
                  let v2 = (* "." *) token env v2 in
                  R.Tuple [v1; v2]
                ))
              | None -> R.Option None)
            in
            let v3 =
              (match v3 with
              | Some (v1, v2) -> R.Option (Some (
                  let v1 = map_constructor_argument env v1 in
                  let v2 = (* "->" *) token env v2 in
                  R.Tuple [v1; v2]
                ))
              | None -> R.Option None)
            in
            let v4 = map_simple_type_ext env v4 in
            R.Tuple [v1; v2; v3; v4]
          )
        | `EQ_cons_path (v1, v2) -> R.Case ("EQ_cons_path",
            let v1 = (* "=" *) token env v1 in
            let v2 = map_constructor_path env v2 in
            R.Tuple [v1; v2]
          )
        )
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2]

and map_do_clause (env : env) ((v1, v2, v3) : CST.do_clause) =
  let v1 = (* "do" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_sequence_expression_ext env x
      ))
    | None -> R.Option None)
  in
  let v3 = (* "done" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_else_clause (env : env) ((v1, v2) : CST.else_clause) =
  let v1 = (* "else" *) token env v1 in
  let v2 = map_expression_ext env v2 in
  R.Tuple [v1; v2]

and map_exception_definition (env : env) ((v1, v2, v3, v4) : CST.exception_definition) =
  let v1 = (* "exception" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_attribute env x
      ))
    | None -> R.Option None)
  in
  let v3 = map_constructor_declaration env v3 in
  let v4 = R.List (List.map (map_item_attribute env) v4) in
  R.Tuple [v1; v2; v3; v4]

and map_expression (env : env) (x : CST.expression) =
  (match x with
  | `Simple_exp x -> R.Case ("Simple_exp",
      map_simple_expression env x
    )
  | `Prod_exp (v1, v2, v3) -> R.Case ("Prod_exp",
      let v1 = map_expression_ext env v1 in
      let v2 = (* "," *) token env v2 in
      let v3 = map_expression_ext env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Cons_exp (v1, v2, v3) -> R.Case ("Cons_exp",
      let v1 = map_expression_ext env v1 in
      let v2 = (* "::" *) token env v2 in
      let v3 = map_expression_ext env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `App_exp (v1, v2) -> R.Case ("App_exp",
      let v1 = map_simple_expression_ext env v1 in
      let v2 = R.List (List.map (map_argument env) v2) in
      R.Tuple [v1; v2]
    )
  | `Infix_exp x -> R.Case ("Infix_exp",
      map_infix_expression env x
    )
  | `Sign_exp (v1, v2) -> R.Case ("Sign_exp",
      let v1 = map_sign_operator env v1 in
      let v2 = map_expression_ext env v2 in
      R.Tuple [v1; v2]
    )
  | `Set_exp (v1, v2, v3) -> R.Case ("Set_exp",
      let v1 =
        (match v1 with
        | `Field_get_exp x -> R.Case ("Field_get_exp",
            map_field_get_expression env x
          )
        | `Array_get_exp x -> R.Case ("Array_get_exp",
            map_array_get_expression env x
          )
        | `Str_get_exp x -> R.Case ("Str_get_exp",
            map_string_get_expression env x
          )
        | `Biga_get_exp x -> R.Case ("Biga_get_exp",
            map_bigarray_get_expression env x
          )
        | `Id tok -> R.Case ("Id",
            (* pattern "[a-z_][a-zA-Z0-9_']*" *) token env tok
          )
        )
      in
      let v2 = (* "<-" *) token env v2 in
      let v3 = map_expression_ext env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `If_exp (v1, v2, v3, v4, v5) -> R.Case ("If_exp",
      let v1 = (* "if" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_attribute env x
          ))
        | None -> R.Option None)
      in
      let v3 = map_sequence_expression_ext env v3 in
      let v4 = map_then_clause env v4 in
      let v5 =
        (match v5 with
        | Some x -> R.Option (Some (
            map_else_clause env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `While_exp (v1, v2, v3, v4) -> R.Case ("While_exp",
      let v1 = (* "while" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_attribute env x
          ))
        | None -> R.Option None)
      in
      let v3 = map_sequence_expression_ext env v3 in
      let v4 = map_do_clause env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `For_exp (v1, v2, v3, v4, v5, v6, v7, v8) -> R.Case ("For_exp",
      let v1 = (* "for" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_attribute env x
          ))
        | None -> R.Option None)
      in
      let v3 = map_value_pattern env v3 in
      let v4 = (* "=" *) token env v4 in
      let v5 = map_sequence_expression_ext env v5 in
      let v6 =
        (match v6 with
        | `To tok -> R.Case ("To",
            (* "to" *) token env tok
          )
        | `Downto tok -> R.Case ("Downto",
            (* "downto" *) token env tok
          )
        )
      in
      let v7 = map_sequence_expression_ext env v7 in
      let v8 = map_do_clause env v8 in
      R.Tuple [v1; v2; v3; v4; v5; v6; v7; v8]
    )
  | `Match_exp (v1, v2, v3, v4) -> R.Case ("Match_exp",
      let v1 =
        (match v1 with
        | `Match_opt_attr (v1, v2) -> R.Case ("Match_opt_attr",
            let v1 = (* "match" *) token env v1 in
            let v2 =
              (match v2 with
              | Some x -> R.Option (Some (
                  map_attribute env x
                ))
              | None -> R.Option None)
            in
            R.Tuple [v1; v2]
          )
        | `Match_op tok -> R.Case ("Match_op",
            (* match_operator *) token env tok
          )
        )
      in
      let v2 = map_sequence_expression_ext env v2 in
      let v3 = (* "with" *) token env v3 in
      let v4 = map_match_cases env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Func_exp (v1, v2, v3) -> R.Case ("Func_exp",
      let v1 = (* "function" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_attribute env x
          ))
        | None -> R.Option None)
      in
      let v3 = map_match_cases env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Fun_exp (v1, v2, v3, v4, v5, v6) -> R.Case ("Fun_exp",
      let v1 = (* "fun" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_attribute env x
          ))
        | None -> R.Option None)
      in
      let v3 = R.List (List.map (map_parameter env) v3) in
      let v4 =
        (match v4 with
        | Some x -> R.Option (Some (
            map_simple_typed env x
          ))
        | None -> R.Option None)
      in
      let v5 = (* "->" *) token env v5 in
      let v6 = map_sequence_expression_ext env v6 in
      R.Tuple [v1; v2; v3; v4; v5; v6]
    )
  | `Try_exp (v1, v2, v3, v4, v5) -> R.Case ("Try_exp",
      let v1 = (* "try" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_attribute env x
          ))
        | None -> R.Option None)
      in
      let v3 = map_sequence_expression_ext env v3 in
      let v4 = (* "with" *) token env v4 in
      let v5 = map_match_cases env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `Let_exp (v1, v2, v3) -> R.Case ("Let_exp",
      let v1 = map_value_definition env v1 in
      let v2 = (* "in" *) token env v2 in
      let v3 = map_sequence_expression_ext env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Assert_exp (v1, v2, v3) -> R.Case ("Assert_exp",
      let v1 = (* "assert" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_attribute env x
          ))
        | None -> R.Option None)
      in
      let v3 = map_simple_expression_ext env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Lazy_exp (v1, v2, v3) -> R.Case ("Lazy_exp",
      let v1 = (* "lazy" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_attribute env x
          ))
        | None -> R.Option None)
      in
      let v3 = map_simple_expression_ext env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Let_module_exp (v1, v2, v3, v4) -> R.Case ("Let_module_exp",
      let v1 = (* "let" *) token env v1 in
      let v2 = map_module_definition env v2 in
      let v3 = (* "in" *) token env v3 in
      let v4 = map_sequence_expression_ext env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Let_open_exp (v1, v2, v3, v4) -> R.Case ("Let_open_exp",
      let v1 = (* "let" *) token env v1 in
      let v2 = map_open_module env v2 in
      let v3 = (* "in" *) token env v3 in
      let v4 = map_sequence_expression_ext env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Let_exc_exp (v1, v2, v3, v4) -> R.Case ("Let_exc_exp",
      let v1 = (* "let" *) token env v1 in
      let v2 = map_exception_definition env v2 in
      let v3 = (* "in" *) token env v3 in
      let v4 = map_sequence_expression_ext env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  )

and map_expression_ext (env : env) (x : CST.expression_ext) =
  (match x with
  | `Exp x -> R.Case ("Exp",
      map_expression env x
    )
  | `Exte x -> R.Case ("Exte",
      map_extension env x
    )
  )

and map_expression_item (env : env) ((v1, v2) : CST.expression_item) =
  let v1 = map_sequence_expression_ext env v1 in
  let v2 = R.List (List.map (map_item_attribute env) v2) in
  R.Tuple [v1; v2]

and map_extension (env : env) (x : CST.extension) =
  (match x with
  | `Exte_ (v1, v2, v3, v4) -> R.Case ("Exte_",
      let v1 = (* "[%" *) token env v1 in
      let v2 = map_attribute_id env v2 in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_attribute_payload env x
          ))
        | None -> R.Option None)
      in
      let v4 = (* "]" *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Quoted_exte (v1, v2, v3, v4, v5, v6, v7) -> R.Case ("Quoted_exte",
      let v1 = (* "{%" *) token env v1 in
      let v2 = map_attribute_id env v2 in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_pat_3d340f6 env x
          ))
        | None -> R.Option None)
      in
      let v4 = (* left_quoted_string_delimiter *) token env v4 in
      let v5 =
        (match v5 with
        | Some x -> R.Option (Some (
            map_quoted_string_content env x
          ))
        | None -> R.Option None)
      in
      let v6 = (* right_quoted_string_delimiter *) token env v6 in
      let v7 = (* "}" *) token env v7 in
      R.Tuple [v1; v2; v3; v4; v5; v6; v7]
    )
  )

and map_external_ (env : env) ((v1, v2, v3, v4, v5, v6, v7) : CST.external_) =
  let v1 = (* "external" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_attribute env x
      ))
    | None -> R.Option None)
  in
  let v3 = map_value_name env v3 in
  let v4 = map_polymorphic_typed env v4 in
  let v5 = (* "=" *) token env v5 in
  let v6 = R.List (List.map (map_string_ env) v6) in
  let v7 = R.List (List.map (map_item_attribute env) v7) in
  R.Tuple [v1; v2; v3; v4; v5; v6; v7]

and map_field_binding_pattern (env : env) ((v1, v2, v3) : CST.field_binding_pattern) =
  let v1 = map_field_path env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_typed env x
      ))
    | None -> R.Option None)
  in
  let v3 =
    (match v3 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = (* "=" *) token env v1 in
        let v2 = map_binding_pattern_ext env v2 in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3]

and map_field_declaration (env : env) ((v1, v2, v3) : CST.field_declaration) =
  let v1 =
    (match v1 with
    | Some tok -> R.Option (Some (
        (* "mutable" *) token env tok
      ))
    | None -> R.Option None)
  in
  let v2 =
    (* pattern "[a-z_][a-zA-Z0-9_']*" *) token env v2
  in
  let v3 = map_polymorphic_typed env v3 in
  R.Tuple [v1; v2; v3]

and map_field_expression (env : env) ((v1, v2, v3) : CST.field_expression) =
  let v1 = map_field_path env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_typed env x
      ))
    | None -> R.Option None)
  in
  let v3 =
    (match v3 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = (* "=" *) token env v1 in
        let v2 = map_expression_ext env v2 in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3]

and map_field_get_expression (env : env) ((v1, v2, v3) : CST.field_get_expression) =
  let v1 = map_simple_expression_ext env v1 in
  let v2 = (* "." *) token env v2 in
  let v3 = map_field_path env v3 in
  R.Tuple [v1; v2; v3]

and map_field_pattern (env : env) ((v1, v2, v3) : CST.field_pattern) =
  let v1 = map_field_path env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_typed env x
      ))
    | None -> R.Option None)
  in
  let v3 =
    (match v3 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = (* "=" *) token env v1 in
        let v2 = map_pattern_ext env v2 in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3]

and map_floating_attribute (env : env) ((v1, v2, v3, v4) : CST.floating_attribute) =
  let v1 = (* "[@@@" *) token env v1 in
  let v2 = map_attribute_id env v2 in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_attribute_payload env x
      ))
    | None -> R.Option None)
  in
  let v4 = (* "]" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_guard (env : env) ((v1, v2) : CST.guard) =
  let v1 = (* "when" *) token env v1 in
  let v2 = map_sequence_expression_ext env v2 in
  R.Tuple [v1; v2]

and map_infix_expression (env : env) (x : CST.infix_expression) =
  (match x with
  | `Choice_exp_pow_op_choice_exp (v1, v2, v3) -> R.Case ("Choice_exp_pow_op_choice_exp",
      let v1 = map_expression_ext env v1 in
      let v2 = (* pow_operator *) token env v2 in
      let v3 = map_expression_ext env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Choice_exp_mult_op_choice_exp (v1, v2, v3) -> R.Case ("Choice_exp_mult_op_choice_exp",
      let v1 = map_expression_ext env v1 in
      let v2 = (* mult_operator *) token env v2 in
      let v3 = map_expression_ext env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Choice_exp_add_op_choice_exp (v1, v2, v3) -> R.Case ("Choice_exp_add_op_choice_exp",
      let v1 = map_expression_ext env v1 in
      let v2 = map_add_operator env v2 in
      let v3 = map_expression_ext env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Choice_exp_concat_op_choice_exp (v1, v2, v3) -> R.Case ("Choice_exp_concat_op_choice_exp",
      let v1 = map_expression_ext env v1 in
      let v2 = (* concat_operator *) token env v2 in
      let v3 = map_expression_ext env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Choice_exp_rel_op_choice_exp (v1, v2, v3) -> R.Case ("Choice_exp_rel_op_choice_exp",
      let v1 = map_expression_ext env v1 in
      let v2 = (* rel_operator *) token env v2 in
      let v3 = map_expression_ext env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Choice_exp_and_op_choice_exp (v1, v2, v3) -> R.Case ("Choice_exp_and_op_choice_exp",
      let v1 = map_expression_ext env v1 in
      let v2 = (* and_operator *) token env v2 in
      let v3 = map_expression_ext env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Choice_exp_or_op_choice_exp (v1, v2, v3) -> R.Case ("Choice_exp_or_op_choice_exp",
      let v1 = map_expression_ext env v1 in
      let v2 = (* or_operator *) token env v2 in
      let v3 = map_expression_ext env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Choice_exp_assign_op_choice_exp (v1, v2, v3) -> R.Case ("Choice_exp_assign_op_choice_exp",
      let v1 = map_expression_ext env v1 in
      let v2 = (* pattern := *) token env v2 in
      let v3 = map_expression_ext env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_instance_variable_expression (env : env) ((v1, v2) : CST.instance_variable_expression) =
  let v1 =
    (* pattern "[a-z_][a-zA-Z0-9_']*" *) token env v1
  in
  let v2 =
    (match v2 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = (* "=" *) token env v1 in
        let v2 = map_expression_ext env v2 in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2]

and map_item_attribute (env : env) ((v1, v2, v3, v4) : CST.item_attribute) =
  let v1 = (* "[@@" *) token env v1 in
  let v2 = map_attribute_id env v2 in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_attribute_payload env x
      ))
    | None -> R.Option None)
  in
  let v4 = (* "]" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_item_extension (env : env) (x : CST.item_extension) =
  (match x with
  | `Item_exte_ (v1, v2, v3, v4, v5) -> R.Case ("Item_exte_",
      let v1 = (* "[%%" *) token env v1 in
      let v2 = map_attribute_id env v2 in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_attribute_payload env x
          ))
        | None -> R.Option None)
      in
      let v4 = (* "]" *) token env v4 in
      let v5 = R.List (List.map (map_item_attribute env) v5) in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `Quoted_item_exte (v1, v2, v3, v4, v5, v6, v7, v8) -> R.Case ("Quoted_item_exte",
      let v1 = (* "{%%" *) token env v1 in
      let v2 = map_attribute_id env v2 in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_pat_3d340f6 env x
          ))
        | None -> R.Option None)
      in
      let v4 = (* left_quoted_string_delimiter *) token env v4 in
      let v5 =
        (match v5 with
        | Some x -> R.Option (Some (
            map_quoted_string_content env x
          ))
        | None -> R.Option None)
      in
      let v6 = (* right_quoted_string_delimiter *) token env v6 in
      let v7 = (* "}" *) token env v7 in
      let v8 = R.List (List.map (map_item_attribute env) v8) in
      R.Tuple [v1; v2; v3; v4; v5; v6; v7; v8]
    )
  )

and map_labeled_argument (env : env) (x : CST.labeled_argument) =
  (match x with
  | `Label x -> R.Case ("Label",
      map_label env x
    )
  | `Label_imm_tok_colon_choice_simple_exp (v1, v2, v3) -> R.Case ("Label_imm_tok_colon_choice_simple_exp",
      let v1 = map_label env v1 in
      let v2 = map_imm_tok_colon env v2 in
      let v3 = map_simple_expression_ext env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Choice_TILDE_LPAR_id_typed_RPAR (v1, v2, v3, v4, v5) -> R.Case ("Choice_TILDE_LPAR_id_typed_RPAR",
      let v1 = map_anon_choice_TILDE_72781e5 env v1 in
      let v2 = (* "(" *) token env v2 in
      let v3 =
        (* pattern "[a-z_][a-zA-Z0-9_']*" *) token env v3
      in
      let v4 = map_typed env v4 in
      let v5 = (* ")" *) token env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  )

and map_let_binding (env : env) ((v1, v2, v3) : CST.let_binding) =
  let v1 = map_binding_pattern_ext env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2, v3, v4, v5) -> R.Option (Some (
        let v1 = R.List (List.map (map_parameter env) v1) in
        let v2 =
          (match v2 with
          | Some x -> R.Option (Some (
              map_polymorphic_typed env x
            ))
          | None -> R.Option None)
        in
        let v3 =
          (match v3 with
          | Some (v1, v2) -> R.Option (Some (
              let v1 = (* ":>" *) token env v1 in
              let v2 = map_type_ext env v2 in
              R.Tuple [v1; v2]
            ))
          | None -> R.Option None)
        in
        let v4 = (* "=" *) token env v4 in
        let v5 = map_sequence_expression_ext env v5 in
        R.Tuple [v1; v2; v3; v4; v5]
      ))
    | None -> R.Option None)
  in
  let v3 = R.List (List.map (map_item_attribute env) v3) in
  R.Tuple [v1; v2; v3]

and map_list_binding_pattern (env : env) ((v1, v2, v3) : CST.list_binding_pattern) =
  let v1 = (* "[" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_anon_bind_pat_ext_rep_SEMI_bind_pat_ext_opt_SEMI_38caf30 env x
      ))
    | None -> R.Option None)
  in
  let v3 = (* "]" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_list_expression (env : env) ((v1, v2, v3) : CST.list_expression) =
  let v1 = (* "[" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_anon_exp_ext_rep_SEMI_exp_ext_opt_SEMI_f0de170 env x
      ))
    | None -> R.Option None)
  in
  let v3 = (* "]" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_list_pattern (env : env) ((v1, v2, v3) : CST.list_pattern) =
  let v1 = (* "[" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_anon_pat_ext_rep_SEMI_pat_ext_opt_SEMI_3830e8c env x
      ))
    | None -> R.Option None)
  in
  let v3 = (* "]" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_match_case (env : env) ((v1, v2, v3, v4) : CST.match_case) =
  let v1 = map_pattern_ext env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_guard env x
      ))
    | None -> R.Option None)
  in
  let v3 = (* "->" *) token env v3 in
  let v4 =
    (match v4 with
    | `Seq_exp_ext x -> R.Case ("Seq_exp_ext",
        map_sequence_expression_ext env x
      )
    | `Refu_case tok -> R.Case ("Refu_case",
        (* "." *) token env tok
      )
    )
  in
  R.Tuple [v1; v2; v3; v4]

and map_match_cases (env : env) ((v1, v2, v3) : CST.match_cases) =
  let v1 =
    (match v1 with
    | Some tok -> R.Option (Some (
        (* "|" *) token env tok
      ))
    | None -> R.Option None)
  in
  let v2 = map_match_case env v2 in
  let v3 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "|" *) token env v1 in
      let v2 = map_match_case env v2 in
      R.Tuple [v1; v2]
    ) v3)
  in
  R.Tuple [v1; v2; v3]

and map_module_binding (env : env) ((v1, v2, v3, v4, v5) : CST.module_binding) =
  let v1 = map_anon_choice_module_name_7ad5569 env v1 in
  let v2 = R.List (List.map (map_module_parameter env) v2) in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_module_typed env x
      ))
    | None -> R.Option None)
  in
  let v4 =
    (match v4 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = map_anon_choice_EQ_4ccabd6 env v1 in
        let v2 = map_module_expression_ext env v2 in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  let v5 = R.List (List.map (map_item_attribute env) v5) in
  R.Tuple [v1; v2; v3; v4; v5]

and map_module_definition (env : env) ((v1, v2, v3, v4, v5) : CST.module_definition) =
  let v1 = (* "module" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_attribute env x
      ))
    | None -> R.Option None)
  in
  let v3 =
    (match v3 with
    | Some tok -> R.Option (Some (
        (* "rec" *) token env tok
      ))
    | None -> R.Option None)
  in
  let v4 = map_module_binding env v4 in
  let v5 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "and" *) token env v1 in
      let v2 = map_module_binding env v2 in
      R.Tuple [v1; v2]
    ) v5)
  in
  R.Tuple [v1; v2; v3; v4; v5]

and map_module_expression (env : env) (x : CST.module_expression) =
  (match x with
  | `Simple_module_exp x -> R.Case ("Simple_module_exp",
      map_simple_module_expression env x
    )
  | `Module_path x -> R.Case ("Module_path",
      map_module_path env x
    )
  | `Stru_ (v1, v2, v3) -> R.Case ("Stru_",
      let v1 = (* "struct" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_structure env x
          ))
        | None -> R.Option None)
      in
      let v3 = (* "end" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Func (v1, v2, v3, v4) -> R.Case ("Func",
      let v1 = (* "functor" *) token env v1 in
      let v2 = R.List (List.map (map_module_parameter env) v2) in
      let v3 = (* "->" *) token env v3 in
      let v4 = map_module_expression_ext env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Module_app (v1, v2) -> R.Case ("Module_app",
      let v1 = map_module_expression_ext env v1 in
      let v2 =
        (match v2 with
        | `Simple_module_exp_ext x -> R.Case ("Simple_module_exp_ext",
            map_simple_module_expression_ext env x
          )
        | `LPAR_RPAR (v1, v2) -> R.Case ("LPAR_RPAR",
            let v1 = (* "(" *) token env v1 in
            let v2 = (* ")" *) token env v2 in
            R.Tuple [v1; v2]
          )
        )
      in
      R.Tuple [v1; v2]
    )
  )

and map_module_expression_ext (env : env) (x : CST.module_expression_ext) =
  (match x with
  | `Module_exp x -> R.Case ("Module_exp",
      map_module_expression env x
    )
  | `Exte x -> R.Case ("Exte",
      map_extension env x
    )
  )

and map_module_parameter (env : env) ((v1, v2, v3) : CST.module_parameter) =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = map_anon_choice_module_name_7ad5569 env v1 in
        let v2 = map_module_typed env v2 in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  let v3 = (* ")" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_module_type (env : env) (x : CST.module_type) =
  (match x with
  | `Module_type_path x -> R.Case ("Module_type_path",
      map_module_type_path env x
    )
  | `Sign_ (v1, v2, v3) -> R.Case ("Sign_",
      let v1 = (* "sig" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_signature env x
          ))
        | None -> R.Option None)
      in
      let v3 = (* "end" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Module_type_cons (v1, v2, v3, v4) -> R.Case ("Module_type_cons",
      let v1 = map_module_type_ext env v1 in
      let v2 = (* "with" *) token env v2 in
      let v3 = map_anon_choice_cons_type_771aabb env v3 in
      let v4 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = (* "and" *) token env v1 in
          let v2 = map_anon_choice_cons_type_771aabb env v2 in
          R.Tuple [v1; v2]
        ) v4)
      in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Module_type_of (v1, v2, v3, v4) -> R.Case ("Module_type_of",
      let v1 = (* "module" *) token env v1 in
      let v2 = (* "type" *) token env v2 in
      let v3 = (* "of" *) token env v3 in
      let v4 = map_module_expression_ext env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Func_type (v1, v2, v3) -> R.Case ("Func_type",
      let v1 =
        (match v1 with
        | `Func_rep_module_param (v1, v2) -> R.Case ("Func_rep_module_param",
            let v1 = (* "functor" *) token env v1 in
            let v2 = R.List (List.map (map_module_parameter env) v2) in
            R.Tuple [v1; v2]
          )
        | `Choice_module_type x -> R.Case ("Choice_module_type",
            map_module_type_ext env x
          )
        | `LPAR_RPAR (v1, v2) -> R.Case ("LPAR_RPAR",
            let v1 = (* "(" *) token env v1 in
            let v2 = (* ")" *) token env v2 in
            R.Tuple [v1; v2]
          )
        )
      in
      let v2 = (* "->" *) token env v2 in
      let v3 = map_module_type_ext env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Paren_module_type (v1, v2, v3) -> R.Case ("Paren_module_type",
      let v1 = (* "(" *) token env v1 in
      let v2 = map_module_type_ext env v2 in
      let v3 = (* ")" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_module_type_definition (env : env) ((v1, v2, v3, v4, v5, v6) : CST.module_type_definition) =
  let v1 = (* "module" *) token env v1 in
  let v2 = (* "type" *) token env v2 in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_attribute env x
      ))
    | None -> R.Option None)
  in
  let v4 = map_module_type_name env v4 in
  let v5 =
    (match v5 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = map_anon_choice_EQ_4ccabd6 env v1 in
        let v2 = map_module_type_ext env v2 in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  let v6 = R.List (List.map (map_item_attribute env) v6) in
  R.Tuple [v1; v2; v3; v4; v5; v6]

and map_module_type_ext (env : env) (x : CST.module_type_ext) =
  (match x with
  | `Module_type x -> R.Case ("Module_type",
      map_module_type env x
    )
  | `Exte x -> R.Case ("Exte",
      map_extension env x
    )
  )

and map_module_typed (env : env) ((v1, v2) : CST.module_typed) =
  let v1 = (* ":" *) token env v1 in
  let v2 = map_module_type_ext env v2 in
  R.Tuple [v1; v2]

and map_object_copy_expression (env : env) ((v1, v2, v3, v4) : CST.object_copy_expression) =
  let v1 = (* "{<" *) token env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = map_instance_variable_expression env v1 in
        let v2 =
          R.List (List.map (fun (v1, v2) ->
            let v1 = (* ";" *) token env v1 in
            let v2 = map_instance_variable_expression env v2 in
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
        (* ";" *) token env tok
      ))
    | None -> R.Option None)
  in
  let v4 = (* ">}" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_object_expression (env : env) ((v1, v2, v3, v4, v5) : CST.object_expression) =
  let v1 = (* "object" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_attribute env x
      ))
    | None -> R.Option None)
  in
  let v3 =
    (match v3 with
    | Some (v1, v2, v3, v4) -> R.Option (Some (
        let v1 = (* "(" *) token env v1 in
        let v2 = map_pattern_ext env v2 in
        let v3 =
          (match v3 with
          | Some x -> R.Option (Some (
              map_typed env x
            ))
          | None -> R.Option None)
        in
        let v4 = (* ")" *) token env v4 in
        R.Tuple [v1; v2; v3; v4]
      ))
    | None -> R.Option None)
  in
  let v4 =
    R.List (List.map (fun x ->
      (match x with
      | `Class_field_ext x -> R.Case ("Class_field_ext",
          map_class_field_ext env x
        )
      | `Floa_attr x -> R.Case ("Floa_attr",
          map_floating_attribute env x
        )
      )
    ) v4)
  in
  let v5 = (* "end" *) token env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

and map_open_module (env : env) ((v1, v2, v3, v4, v5) : CST.open_module) =
  let v1 = (* "open" *) token env v1 in
  let v2 =
    (match v2 with
    | Some tok -> R.Option (Some (
        (* "!" *) token env tok
      ))
    | None -> R.Option None)
  in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_attribute env x
      ))
    | None -> R.Option None)
  in
  let v4 = map_module_expression_ext env v4 in
  let v5 = R.List (List.map (map_item_attribute env) v5) in
  R.Tuple [v1; v2; v3; v4; v5]

and map_package_expression (env : env) ((v1, v2, v3, v4, v5, v6) : CST.package_expression) =
  let v1 = (* "(" *) token env v1 in
  let v2 = (* "module" *) token env v2 in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_attribute env x
      ))
    | None -> R.Option None)
  in
  let v4 = map_module_expression_ext env v4 in
  let v5 =
    (match v5 with
    | Some x -> R.Option (Some (
        map_module_typed env x
      ))
    | None -> R.Option None)
  in
  let v6 = (* ")" *) token env v6 in
  R.Tuple [v1; v2; v3; v4; v5; v6]

and map_package_pattern (env : env) ((v1, v2, v3, v4, v5, v6) : CST.package_pattern) =
  let v1 = (* "(" *) token env v1 in
  let v2 = (* "module" *) token env v2 in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_attribute env x
      ))
    | None -> R.Option None)
  in
  let v4 = map_anon_choice_module_name_7ad5569 env v4 in
  let v5 =
    (match v5 with
    | Some x -> R.Option (Some (
        map_module_typed env x
      ))
    | None -> R.Option None)
  in
  let v6 = (* ")" *) token env v6 in
  R.Tuple [v1; v2; v3; v4; v5; v6]

and map_parameter (env : env) (x : CST.parameter) =
  (match x with
  | `Param_ x -> R.Case ("Param_",
      map_parameter_ env x
    )
  | `Paren_abst_type x -> R.Case ("Paren_abst_type",
      map_parenthesized_abstract_type env x
    )
  )

and map_parameter_ (env : env) (x : CST.parameter_) =
  (match x with
  | `Simple_pat_ext x -> R.Case ("Simple_pat_ext",
      map_simple_pattern_ext env x
    )
  | `Choice_TILDE_id x -> R.Case ("Choice_TILDE_id",
      map_label env x
    )
  | `Label_imm_tok_colon_simple_pat_ext (v1, v2, v3) -> R.Case ("Label_imm_tok_colon_simple_pat_ext",
      let v1 = map_label env v1 in
      let v2 = map_imm_tok_colon env v2 in
      let v3 = map_simple_pattern_ext env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Choice_TILDE_LPAR_id_opt_typed_opt_EQ_seq_exp_ext_RPAR (v1, v2, v3, v4, v5, v6) -> R.Case ("Choice_TILDE_LPAR_id_opt_typed_opt_EQ_seq_exp_ext_RPAR",
      let v1 = map_anon_choice_TILDE_72781e5 env v1 in
      let v2 = (* "(" *) token env v2 in
      let v3 =
        (* pattern "[a-z_][a-zA-Z0-9_']*" *) token env v3
      in
      let v4 =
        (match v4 with
        | Some x -> R.Option (Some (
            map_typed env x
          ))
        | None -> R.Option None)
      in
      let v5 =
        (match v5 with
        | Some (v1, v2) -> R.Option (Some (
            let v1 = (* "=" *) token env v1 in
            let v2 = map_sequence_expression_ext env v2 in
            R.Tuple [v1; v2]
          ))
        | None -> R.Option None)
      in
      let v6 = (* ")" *) token env v6 in
      R.Tuple [v1; v2; v3; v4; v5; v6]
    )
  | `Label_imm_tok_colon_LPAR_pat_ext_opt_typed_EQ_seq_exp_ext_RPAR (v1, v2, v3, v4, v5, v6, v7, v8) -> R.Case ("Label_imm_tok_colon_LPAR_pat_ext_opt_typed_EQ_seq_exp_ext_RPAR",
      let v1 = map_label env v1 in
      let v2 = map_imm_tok_colon env v2 in
      let v3 = (* "(" *) token env v3 in
      let v4 = map_pattern_ext env v4 in
      let v5 =
        (match v5 with
        | Some x -> R.Option (Some (
            map_typed env x
          ))
        | None -> R.Option None)
      in
      let v6 = (* "=" *) token env v6 in
      let v7 = map_sequence_expression_ext env v7 in
      let v8 = (* ")" *) token env v8 in
      R.Tuple [v1; v2; v3; v4; v5; v6; v7; v8]
    )
  )

and map_parenthesized_expression (env : env) (x : CST.parenthesized_expression) =
  (match x with
  | `Begin_opt_attr_seq_exp_ext_end (v1, v2, v3, v4) -> R.Case ("Begin_opt_attr_seq_exp_ext_end",
      let v1 = (* "begin" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_attribute env x
          ))
        | None -> R.Option None)
      in
      let v3 = map_sequence_expression_ext env v3 in
      let v4 = (* "end" *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `LPAR_seq_exp_ext_RPAR (v1, v2, v3) -> R.Case ("LPAR_seq_exp_ext_RPAR",
      let v1 = (* "(" *) token env v1 in
      let v2 = map_sequence_expression_ext env v2 in
      let v3 = (* ")" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_parenthesized_type (env : env) ((v1, v2, v3) : CST.parenthesized_type) =
  let v1 = (* "(" *) token env v1 in
  let v2 = map_type_ext env v2 in
  let v3 = (* ")" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_pattern (env : env) (x : CST.pattern) =
  (match x with
  | `Simple_pat x -> R.Case ("Simple_pat",
      map_simple_pattern env x
    )
  | `Alias_pat (v1, v2, v3) -> R.Case ("Alias_pat",
      let v1 = map_pattern_ext env v1 in
      let v2 = (* "as" *) token env v2 in
      let v3 = map_value_pattern env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Or_pat (v1, v2, v3) -> R.Case ("Or_pat",
      let v1 = map_pattern_ext env v1 in
      let v2 = (* "|" *) token env v2 in
      let v3 = map_pattern_ext env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Cons_pat_4ec55c1 (v1, v2, v3) -> R.Case ("Cons_pat_4ec55c1",
      let v1 = map_constructor_path env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_parenthesized_abstract_type env x
          ))
        | None -> R.Option None)
      in
      let v3 = map_pattern_ext env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Tag_pat (v1, v2) -> R.Case ("Tag_pat",
      let v1 = map_tag env v1 in
      let v2 = map_pattern_ext env v2 in
      R.Tuple [v1; v2]
    )
  | `Tuple_pat (v1, v2, v3) -> R.Case ("Tuple_pat",
      let v1 = map_pattern_ext env v1 in
      let v2 = (* "," *) token env v2 in
      let v3 = map_pattern_ext env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Cons_pat_9b4e481 (v1, v2, v3) -> R.Case ("Cons_pat_9b4e481",
      let v1 = map_pattern_ext env v1 in
      let v2 = (* "::" *) token env v2 in
      let v3 = map_pattern_ext env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Range_pat x -> R.Case ("Range_pat",
      map_range_pattern env x
    )
  | `Lazy_pat (v1, v2, v3) -> R.Case ("Lazy_pat",
      let v1 = (* "lazy" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_attribute env x
          ))
        | None -> R.Option None)
      in
      let v3 = map_pattern_ext env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exc_pat (v1, v2, v3) -> R.Case ("Exc_pat",
      let v1 = (* "exception" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_attribute env x
          ))
        | None -> R.Option None)
      in
      let v3 = map_pattern_ext env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_pattern_ext (env : env) (x : CST.pattern_ext) =
  (match x with
  | `Pat x -> R.Case ("Pat",
      map_pattern env x
    )
  | `Exte x -> R.Case ("Exte",
      map_extension env x
    )
  )

and map_polymorphic_type (env : env) (x : CST.polymorphic_type) =
  (match x with
  | `Poly_type_ (v1, v2, v3) -> R.Case ("Poly_type_",
      let v1 =
        (match v1 with
        | `Rep1_type_var xs -> R.Case ("Rep1_type_var",
            R.List (List.map (map_type_variable env) xs)
          )
        | `Abst_type x -> R.Case ("Abst_type",
            map_abstract_type env x
          )
        )
      in
      let v2 = (* "." *) token env v2 in
      let v3 = map_type_ext env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Type_ext x -> R.Case ("Type_ext",
      map_type_ext env x
    )
  )

and map_polymorphic_typed (env : env) ((v1, v2) : CST.polymorphic_typed) =
  let v1 = (* ":" *) token env v1 in
  let v2 = map_polymorphic_type env v2 in
  R.Tuple [v1; v2]

and map_record_binding_pattern (env : env) ((v1, v2, v3, v4, v5, v6) : CST.record_binding_pattern) =
  let v1 = (* "{" *) token env v1 in
  let v2 = map_field_binding_pattern env v2 in
  let v3 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* ";" *) token env v1 in
      let v2 = map_field_binding_pattern env v2 in
      R.Tuple [v1; v2]
    ) v3)
  in
  let v4 =
    (match v4 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = (* ";" *) token env v1 in
        let v2 = (* "_" *) token env v2 in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  let v5 =
    (match v5 with
    | Some tok -> R.Option (Some (
        (* ";" *) token env tok
      ))
    | None -> R.Option None)
  in
  let v6 = (* "}" *) token env v6 in
  R.Tuple [v1; v2; v3; v4; v5; v6]

and map_record_declaration (env : env) ((v1, v2, v3, v4, v5) : CST.record_declaration) =
  let v1 = (* "{" *) token env v1 in
  let v2 = map_field_declaration env v2 in
  let v3 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* ";" *) token env v1 in
      let v2 = map_field_declaration env v2 in
      R.Tuple [v1; v2]
    ) v3)
  in
  let v4 =
    (match v4 with
    | Some tok -> R.Option (Some (
        (* ";" *) token env tok
      ))
    | None -> R.Option None)
  in
  let v5 = (* "}" *) token env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

and map_record_expression (env : env) ((v1, v2, v3, v4, v5, v6) : CST.record_expression) =
  let v1 = (* "{" *) token env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = map_simple_expression_ext env v1 in
        let v2 = (* "with" *) token env v2 in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  let v3 = map_field_expression env v3 in
  let v4 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* ";" *) token env v1 in
      let v2 = map_field_expression env v2 in
      R.Tuple [v1; v2]
    ) v4)
  in
  let v5 =
    (match v5 with
    | Some tok -> R.Option (Some (
        (* ";" *) token env tok
      ))
    | None -> R.Option None)
  in
  let v6 = (* "}" *) token env v6 in
  R.Tuple [v1; v2; v3; v4; v5; v6]

and map_record_pattern (env : env) ((v1, v2, v3, v4, v5, v6) : CST.record_pattern) =
  let v1 = (* "{" *) token env v1 in
  let v2 = map_field_pattern env v2 in
  let v3 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* ";" *) token env v1 in
      let v2 = map_field_pattern env v2 in
      R.Tuple [v1; v2]
    ) v3)
  in
  let v4 =
    (match v4 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = (* ";" *) token env v1 in
        let v2 = (* "_" *) token env v2 in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  let v5 =
    (match v5 with
    | Some tok -> R.Option (Some (
        (* ";" *) token env tok
      ))
    | None -> R.Option None)
  in
  let v6 = (* "}" *) token env v6 in
  R.Tuple [v1; v2; v3; v4; v5; v6]

and map_sequence_expression_ (env : env) (x : CST.sequence_expression_) =
  (match x with
  | `Exp x -> R.Case ("Exp",
      map_expression env x
    )
  | `Seq_exp (v1, v2, v3) -> R.Case ("Seq_exp",
      let v1 = map_expression_ext env v1 in
      let v2 = (* ";" *) token env v2 in
      let v3 =
        (match v3 with
        | Some (v1, v2) -> R.Option (Some (
            let v1 =
              (match v1 with
              | Some x -> R.Option (Some (
                  map_attribute env x
                ))
              | None -> R.Option None)
            in
            let v2 = map_sequence_expression_ext env v2 in
            R.Tuple [v1; v2]
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3]
    )
  )

and map_sequence_expression_ext (env : env) (x : CST.sequence_expression_ext) =
  (match x with
  | `Seq_exp_ x -> R.Case ("Seq_exp_",
      map_sequence_expression_ env x
    )
  | `Exte x -> R.Case ("Exte",
      map_extension env x
    )
  )

and map_signature (env : env) (x : CST.signature) =
  (match x with
  | `Rep1_SEMISEMI xs -> R.Case ("Rep1_SEMISEMI",
      R.List (List.map (token env (* ";;" *)) xs)
    )
  | `Rep1_rep_SEMISEMI_sign_item_ext_rep_SEMISEMI (v1, v2) -> R.Case ("Rep1_rep_SEMISEMI_sign_item_ext_rep_SEMISEMI",
      let v1 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = R.List (List.map (token env (* ";;" *)) v1) in
          let v2 = map_signature_item_ext env v2 in
          R.Tuple [v1; v2]
        ) v1)
      in
      let v2 = R.List (List.map (token env (* ";;" *)) v2) in
      R.Tuple [v1; v2]
    )
  )

and map_signature_item (env : env) (x : CST.signature_item) =
  (match x with
  | `Value_spec (v1, v2, v3, v4, v5) -> R.Case ("Value_spec",
      let v1 = (* "val" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_attribute env x
          ))
        | None -> R.Option None)
      in
      let v3 = map_value_name env v3 in
      let v4 = map_polymorphic_typed env v4 in
      let v5 = R.List (List.map (map_item_attribute env) v5) in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `Exte x -> R.Case ("Exte",
      map_external_ env x
    )
  | `Type_defi x -> R.Case ("Type_defi",
      map_type_definition env x
    )
  | `Exc_defi x -> R.Case ("Exc_defi",
      map_exception_definition env x
    )
  | `Module_defi x -> R.Case ("Module_defi",
      map_module_definition env x
    )
  | `Module_type_defi x -> R.Case ("Module_type_defi",
      map_module_type_definition env x
    )
  | `Open_module x -> R.Case ("Open_module",
      map_open_module env x
    )
  | `Incl_module_type (v1, v2, v3, v4) -> R.Case ("Incl_module_type",
      let v1 = (* "include" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_attribute env x
          ))
        | None -> R.Option None)
      in
      let v3 = map_module_type_ext env v3 in
      let v4 = R.List (List.map (map_item_attribute env) v4) in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Class_defi x -> R.Case ("Class_defi",
      map_class_definition env x
    )
  | `Class_type_defi x -> R.Case ("Class_type_defi",
      map_class_type_definition env x
    )
  | `Floa_attr x -> R.Case ("Floa_attr",
      map_floating_attribute env x
    )
  )

and map_signature_item_ext (env : env) (x : CST.signature_item_ext) =
  (match x with
  | `Sign_item x -> R.Case ("Sign_item",
      map_signature_item env x
    )
  | `Item_exte x -> R.Case ("Item_exte",
      map_item_extension env x
    )
  )

and map_simple_class_expression (env : env) (x : CST.simple_class_expression) =
  (match x with
  | `Class_path x -> R.Case ("Class_path",
      map_class_path env x
    )
  | `Inst_class (v1, v2, v3, v4, v5) -> R.Case ("Inst_class",
      let v1 = (* "[" *) token env v1 in
      let v2 = map_type_ext env v2 in
      let v3 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = (* "," *) token env v1 in
          let v2 = map_type_ext env v2 in
          R.Tuple [v1; v2]
        ) v3)
      in
      let v4 = (* "]" *) token env v4 in
      let v5 = map_class_path env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `Obj_exp x -> R.Case ("Obj_exp",
      map_object_expression env x
    )
  | `Typed_class_exp (v1, v2, v3, v4) -> R.Case ("Typed_class_exp",
      let v1 = (* "(" *) token env v1 in
      let v2 = map_class_expression_ext env v2 in
      let v3 = map_class_typed env v3 in
      let v4 = (* ")" *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Paren_class_exp (v1, v2, v3) -> R.Case ("Paren_class_exp",
      let v1 = (* "(" *) token env v1 in
      let v2 = map_class_expression_ext env v2 in
      let v3 = (* ")" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_simple_class_type (env : env) (x : CST.simple_class_type) =
  (match x with
  | `Class_type_path x -> R.Case ("Class_type_path",
      map_class_type_path env x
    )
  | `Inst_class_type (v1, v2, v3, v4, v5) -> R.Case ("Inst_class_type",
      let v1 = (* "[" *) token env v1 in
      let v2 = map_type_ext env v2 in
      let v3 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = (* "," *) token env v1 in
          let v2 = map_type_ext env v2 in
          R.Tuple [v1; v2]
        ) v3)
      in
      let v4 = (* "]" *) token env v4 in
      let v5 = map_class_type_path env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `Class_body_type (v1, v2, v3, v4) -> R.Case ("Class_body_type",
      let v1 = (* "object" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_parenthesized_type env x
          ))
        | None -> R.Option None)
      in
      let v3 =
        R.List (List.map (fun x ->
          (match x with
          | `Class_field_spec_ext x -> R.Case ("Class_field_spec_ext",
              map_class_field_specification_ext env x
            )
          | `Floa_attr x -> R.Case ("Floa_attr",
              map_floating_attribute env x
            )
          )
        ) v3)
      in
      let v4 = (* "end" *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Let_open_class_type (v1, v2, v3, v4) -> R.Case ("Let_open_class_type",
      let v1 = (* "let" *) token env v1 in
      let v2 = map_open_module env v2 in
      let v3 = (* "in" *) token env v3 in
      let v4 = map_simple_class_type_ext env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  )

and map_simple_class_type_ext (env : env) (x : CST.simple_class_type_ext) =
  (match x with
  | `Simple_class_type x -> R.Case ("Simple_class_type",
      map_simple_class_type env x
    )
  | `Exte x -> R.Case ("Exte",
      map_extension env x
    )
  )

and map_simple_expression (env : env) (x : CST.simple_expression) =
  (match x with
  | `Value_path x -> R.Case ("Value_path",
      map_value_path env x
    )
  | `Cst x -> R.Case ("Cst",
      map_constant env x
    )
  | `Typed_exp (v1, v2, v3, v4) -> R.Case ("Typed_exp",
      let v1 = (* "(" *) token env v1 in
      let v2 = map_sequence_expression_ext env v2 in
      let v3 = map_typed env v3 in
      let v4 = (* ")" *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Cons_path x -> R.Case ("Cons_path",
      map_constructor_path env x
    )
  | `Tag x -> R.Case ("Tag",
      map_tag env x
    )
  | `List_exp x -> R.Case ("List_exp",
      map_list_expression env x
    )
  | `Array_exp x -> R.Case ("Array_exp",
      map_array_expression env x
    )
  | `Record_exp x -> R.Case ("Record_exp",
      map_record_expression env x
    )
  | `Prefix_exp (v1, v2) -> R.Case ("Prefix_exp",
      let v1 = (* prefix_operator *) token env v1 in
      let v2 = map_simple_expression_ext env v2 in
      R.Tuple [v1; v2]
    )
  | `Hash_exp (v1, v2, v3) -> R.Case ("Hash_exp",
      let v1 = map_simple_expression_ext env v1 in
      let v2 = (* hash_operator *) token env v2 in
      let v3 = map_simple_expression_ext env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Field_get_exp x -> R.Case ("Field_get_exp",
      map_field_get_expression env x
    )
  | `Array_get_exp x -> R.Case ("Array_get_exp",
      map_array_get_expression env x
    )
  | `Str_get_exp x -> R.Case ("Str_get_exp",
      map_string_get_expression env x
    )
  | `Biga_get_exp x -> R.Case ("Biga_get_exp",
      map_bigarray_get_expression env x
    )
  | `Coer_exp (v1, v2, v3, v4, v5, v6) -> R.Case ("Coer_exp",
      let v1 = (* "(" *) token env v1 in
      let v2 = map_sequence_expression_ext env v2 in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_typed env x
          ))
        | None -> R.Option None)
      in
      let v4 = (* ":>" *) token env v4 in
      let v5 = map_type_ext env v5 in
      let v6 = (* ")" *) token env v6 in
      R.Tuple [v1; v2; v3; v4; v5; v6]
    )
  | `Local_open_exp (v1, v2, v3) -> R.Case ("Local_open_exp",
      let v1 = map_module_path env v1 in
      let v2 = (* "." *) token env v2 in
      let v3 =
        (match v3 with
        | `LPAR_opt_seq_exp_ext_RPAR (v1, v2, v3) -> R.Case ("LPAR_opt_seq_exp_ext_RPAR",
            let v1 = (* "(" *) token env v1 in
            let v2 =
              (match v2 with
              | Some x -> R.Option (Some (
                  map_sequence_expression_ext env x
                ))
              | None -> R.Option None)
            in
            let v3 = (* ")" *) token env v3 in
            R.Tuple [v1; v2; v3]
          )
        | `List_exp x -> R.Case ("List_exp",
            map_list_expression env x
          )
        | `Array_exp x -> R.Case ("Array_exp",
            map_array_expression env x
          )
        | `Record_exp x -> R.Case ("Record_exp",
            map_record_expression env x
          )
        | `Obj_copy_exp x -> R.Case ("Obj_copy_exp",
            map_object_copy_expression env x
          )
        | `Pack_exp x -> R.Case ("Pack_exp",
            map_package_expression env x
          )
        )
      in
      R.Tuple [v1; v2; v3]
    )
  | `Pack_exp x -> R.Case ("Pack_exp",
      map_package_expression env x
    )
  | `New_exp (v1, v2, v3) -> R.Case ("New_exp",
      let v1 = (* "new" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_attribute env x
          ))
        | None -> R.Option None)
      in
      let v3 = map_class_path env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Obj_copy_exp x -> R.Case ("Obj_copy_exp",
      map_object_copy_expression env x
    )
  | `Meth_invo (v1, v2, v3) -> R.Case ("Meth_invo",
      let v1 = map_simple_expression_ext env v1 in
      let v2 = (* "#" *) token env v2 in
      let v3 =
        (* pattern "[a-z_][a-zA-Z0-9_']*" *) token env v3
      in
      R.Tuple [v1; v2; v3]
    )
  | `Obj_exp x -> R.Case ("Obj_exp",
      map_object_expression env x
    )
  | `Paren_exp x -> R.Case ("Paren_exp",
      map_parenthesized_expression env x
    )
  | `Ocam_value tok -> R.Case ("Ocam_value",
      (* pattern \$[0-9]+ *) token env tok
    )
  )

and map_simple_expression_ext (env : env) (x : CST.simple_expression_ext) =
  (match x with
  | `Simple_exp x -> R.Case ("Simple_exp",
      map_simple_expression env x
    )
  | `Exte x -> R.Case ("Exte",
      map_extension env x
    )
  )

and map_simple_module_expression (env : env) (x : CST.simple_module_expression) =
  (match x with
  | `Typed_module_exp (v1, v2, v3, v4) -> R.Case ("Typed_module_exp",
      let v1 = (* "(" *) token env v1 in
      let v2 = map_module_expression_ext env v2 in
      let v3 = map_module_typed env v3 in
      let v4 = (* ")" *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Paren_module_exp (v1, v2, v3) -> R.Case ("Paren_module_exp",
      let v1 = (* "(" *) token env v1 in
      let v2 = map_module_expression_ext env v2 in
      let v3 = (* ")" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Packed_module (v1, v2, v3, v4, v5, v6) -> R.Case ("Packed_module",
      let v1 = (* "(" *) token env v1 in
      let v2 = (* "val" *) token env v2 in
      let v3 = map_expression_ext env v3 in
      let v4 =
        (match v4 with
        | Some x -> R.Option (Some (
            map_module_typed env x
          ))
        | None -> R.Option None)
      in
      let v5 =
        (match v5 with
        | Some (v1, v2) -> R.Option (Some (
            let v1 = (* ":>" *) token env v1 in
            let v2 = map_module_type_ext env v2 in
            R.Tuple [v1; v2]
          ))
        | None -> R.Option None)
      in
      let v6 = (* ")" *) token env v6 in
      R.Tuple [v1; v2; v3; v4; v5; v6]
    )
  )

and map_simple_module_expression_ext (env : env) (x : CST.simple_module_expression_ext) =
  (match x with
  | `Simple_module_exp x -> R.Case ("Simple_module_exp",
      map_simple_module_expression env x
    )
  | `Exte x -> R.Case ("Exte",
      map_extension env x
    )
  )

and map_simple_pattern (env : env) (x : CST.simple_pattern) =
  (match x with
  | `Value_pat x -> R.Case ("Value_pat",
      map_value_pattern env x
    )
  | `Signed_cst x -> R.Case ("Signed_cst",
      map_signed_constant env x
    )
  | `Typed_pat (v1, v2, v3, v4) -> R.Case ("Typed_pat",
      let v1 = (* "(" *) token env v1 in
      let v2 = map_pattern_ext env v2 in
      let v3 = map_typed env v3 in
      let v4 = (* ")" *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Cons_path x -> R.Case ("Cons_path",
      map_constructor_path env x
    )
  | `Tag x -> R.Case ("Tag",
      map_tag env x
    )
  | `Poly_vari_pat x -> R.Case ("Poly_vari_pat",
      map_polymorphic_variant_pattern env x
    )
  | `Record_pat x -> R.Case ("Record_pat",
      map_record_pattern env x
    )
  | `List_pat x -> R.Case ("List_pat",
      map_list_pattern env x
    )
  | `Array_pat x -> R.Case ("Array_pat",
      map_array_pattern env x
    )
  | `Local_open_pat (v1, v2, v3) -> R.Case ("Local_open_pat",
      let v1 = map_module_path env v1 in
      let v2 = (* "." *) token env v2 in
      let v3 =
        (match v3 with
        | `LPAR_opt_pat_ext_RPAR (v1, v2, v3) -> R.Case ("LPAR_opt_pat_ext_RPAR",
            let v1 = (* "(" *) token env v1 in
            let v2 =
              (match v2 with
              | Some x -> R.Option (Some (
                  map_pattern_ext env x
                ))
              | None -> R.Option None)
            in
            let v3 = (* ")" *) token env v3 in
            R.Tuple [v1; v2; v3]
          )
        | `List_pat x -> R.Case ("List_pat",
            map_list_pattern env x
          )
        | `Array_pat x -> R.Case ("Array_pat",
            map_array_pattern env x
          )
        | `Record_pat x -> R.Case ("Record_pat",
            map_record_pattern env x
          )
        )
      in
      R.Tuple [v1; v2; v3]
    )
  | `Pack_pat x -> R.Case ("Pack_pat",
      map_package_pattern env x
    )
  | `Paren_pat (v1, v2, v3) -> R.Case ("Paren_pat",
      let v1 = (* "(" *) token env v1 in
      let v2 = map_pattern_ext env v2 in
      let v3 = (* ")" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_simple_pattern_ext (env : env) (x : CST.simple_pattern_ext) =
  (match x with
  | `Simple_pat x -> R.Case ("Simple_pat",
      map_simple_pattern env x
    )
  | `Exte x -> R.Case ("Exte",
      map_extension env x
    )
  )

and map_simple_type (env : env) (x : CST.simple_type) =
  (match x with
  | `Type_var x -> R.Case ("Type_var",
      map_type_variable env x
    )
  | `Type_cons_path x -> R.Case ("Type_cons_path",
      map_type_constructor_path env x
    )
  | `Cons_type (v1, v2) -> R.Case ("Cons_type",
      let v1 = map_anon_choice_simple_type_ext_30dd028 env v1 in
      let v2 = map_type_constructor_path env v2 in
      R.Tuple [v1; v2]
    )
  | `Poly_vari_type v1 -> R.Case ("Poly_vari_type",
      (match v1 with
      | `LBRACK_tag_spec_RBRACK (v1, v2, v3) -> R.Case ("LBRACK_tag_spec_RBRACK",
          let v1 = (* "[" *) token env v1 in
          let v2 = map_tag_specification env v2 in
          let v3 = (* "]" *) token env v3 in
          R.Tuple [v1; v2; v3]
        )
      | `LBRACK_opt_tag_spec_BAR_tag_spec_rep_BAR_tag_spec_RBRACK (v1, v2, v3, v4, v5, v6) -> R.Case ("LBRACK_opt_tag_spec_BAR_tag_spec_rep_BAR_tag_spec_RBRACK",
          let v1 = (* "[" *) token env v1 in
          let v2 =
            (match v2 with
            | Some x -> R.Option (Some (
                map_tag_spec env x
              ))
            | None -> R.Option None)
          in
          let v3 = (* "|" *) token env v3 in
          let v4 = map_tag_spec env v4 in
          let v5 =
            R.List (List.map (fun (v1, v2) ->
              let v1 = (* "|" *) token env v1 in
              let v2 = map_tag_spec env v2 in
              R.Tuple [v1; v2]
            ) v5)
          in
          let v6 = (* "]" *) token env v6 in
          R.Tuple [v1; v2; v3; v4; v5; v6]
        )
      | `LBRACKGT_opt_BAR_opt_tag_spec_rep_BAR_tag_spec_RBRACK (v1, v2, v3, v4) -> R.Case ("LBRACKGT_opt_BAR_opt_tag_spec_rep_BAR_tag_spec_RBRACK",
          let v1 = (* "[>" *) token env v1 in
          let v2 =
            (match v2 with
            | Some tok -> R.Option (Some (
                (* "|" *) token env tok
              ))
            | None -> R.Option None)
          in
          let v3 =
            (match v3 with
            | Some (v1, v2) -> R.Option (Some (
                let v1 = map_tag_spec env v1 in
                let v2 =
                  R.List (List.map (fun (v1, v2) ->
                    let v1 = (* "|" *) token env v1 in
                    let v2 = map_tag_spec env v2 in
                    R.Tuple [v1; v2]
                  ) v2)
                in
                R.Tuple [v1; v2]
              ))
            | None -> R.Option None)
          in
          let v4 = (* "]" *) token env v4 in
          R.Tuple [v1; v2; v3; v4]
        )
      | `LBRACKLT_opt_BAR_tag_spec_rep_BAR_tag_spec_opt_GT_rep1_tag_RBRACK (v1, v2, v3, v4, v5, v6) -> R.Case ("LBRACKLT_opt_BAR_tag_spec_rep_BAR_tag_spec_opt_GT_rep1_tag_RBRACK",
          let v1 = (* "[<" *) token env v1 in
          let v2 =
            (match v2 with
            | Some tok -> R.Option (Some (
                (* "|" *) token env tok
              ))
            | None -> R.Option None)
          in
          let v3 = map_tag_spec env v3 in
          let v4 =
            R.List (List.map (fun (v1, v2) ->
              let v1 = (* "|" *) token env v1 in
              let v2 = map_tag_spec env v2 in
              R.Tuple [v1; v2]
            ) v4)
          in
          let v5 =
            (match v5 with
            | Some (v1, v2) -> R.Option (Some (
                let v1 = (* ">" *) token env v1 in
                let v2 = R.List (List.map (map_tag env) v2) in
                R.Tuple [v1; v2]
              ))
            | None -> R.Option None)
          in
          let v6 = (* "]" *) token env v6 in
          R.Tuple [v1; v2; v3; v4; v5; v6]
        )
      )
    )
  | `Pack_type (v1, v2, v3, v4, v5) -> R.Case ("Pack_type",
      let v1 = (* "(" *) token env v1 in
      let v2 = (* "module" *) token env v2 in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_attribute env x
          ))
        | None -> R.Option None)
      in
      let v4 = map_module_type_ext env v4 in
      let v5 = (* ")" *) token env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `Hash_type (v1, v2, v3) -> R.Case ("Hash_type",
      let v1 =
        (match v1 with
        | Some x -> R.Option (Some (
            map_anon_choice_simple_type_ext_30dd028 env x
          ))
        | None -> R.Option None)
      in
      let v2 = (* "#" *) token env v2 in
      let v3 = map_class_type_path env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Obj_type (v1, v2, v3) -> R.Case ("Obj_type",
      let v1 = (* "<" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            (match x with
            | `Choice_meth_type_rep_SEMI_choice_meth_type_opt_SEMI_opt_DOTDOT (v1, v2, v3) -> R.Case ("Choice_meth_type_rep_SEMI_choice_meth_type_opt_SEMI_opt_DOTDOT",
                let v1 = map_anon_choice_meth_type_345b567 env v1 in
                let v2 =
                  R.List (List.map (fun (v1, v2) ->
                    let v1 = (* ";" *) token env v1 in
                    let v2 = map_anon_choice_meth_type_345b567 env v2 in
                    R.Tuple [v1; v2]
                  ) v2)
                in
                let v3 =
                  (match v3 with
                  | Some (v1, v2) -> R.Option (Some (
                      let v1 = (* ";" *) token env v1 in
                      let v2 =
                        (match v2 with
                        | Some tok -> R.Option (Some (
                            (* ".." *) token env tok
                          ))
                        | None -> R.Option None)
                      in
                      R.Tuple [v1; v2]
                    ))
                  | None -> R.Option None)
                in
                R.Tuple [v1; v2; v3]
              )
            | `DOTDOT tok -> R.Case ("DOTDOT",
                (* ".." *) token env tok
              )
            )
          ))
        | None -> R.Option None)
      in
      let v3 = (* ">" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Paren_type x -> R.Case ("Paren_type",
      map_parenthesized_type env x
    )
  )

and map_simple_type_ext (env : env) (x : CST.simple_type_ext) =
  (match x with
  | `Simple_type x -> R.Case ("Simple_type",
      map_simple_type env x
    )
  | `Exte x -> R.Case ("Exte",
      map_extension env x
    )
  )

and map_simple_typed (env : env) ((v1, v2) : CST.simple_typed) =
  let v1 = (* ":" *) token env v1 in
  let v2 = map_simple_type_ext env v2 in
  R.Tuple [v1; v2]

and map_string_get_expression (env : env) ((v1, v2, v3, v4, v5, v6) : CST.string_get_expression) =
  let v1 = map_simple_expression_ext env v1 in
  let v2 = (* "." *) token env v2 in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_indexing_operator_path env x
      ))
    | None -> R.Option None)
  in
  let v4 = (* "[" *) token env v4 in
  let v5 = map_sequence_expression_ext env v5 in
  let v6 = (* "]" *) token env v6 in
  R.Tuple [v1; v2; v3; v4; v5; v6]

and map_structure (env : env) (x : CST.structure) =
  (match x with
  | `Rep1_SEMISEMI xs -> R.Case ("Rep1_SEMISEMI",
      R.List (List.map (token env (* ";;" *)) xs)
    )
  | `Rep_SEMISEMI_choice_stru_item_ext_rep_choice_rep_SEMISEMI_choice_stru_item_ext_rep_SEMISEMI (v1, v2, v3, v4) -> R.Case ("Rep_SEMISEMI_choice_stru_item_ext_rep_choice_rep_SEMISEMI_choice_stru_item_ext_rep_SEMISEMI",
      let v1 = R.List (List.map (token env (* ";;" *)) v1) in
      let v2 =
        (match v2 with
        | `Stru_item_ext x -> R.Case ("Stru_item_ext",
            map_structure_item_ext env x
          )
        | `Topl_dire x -> R.Case ("Topl_dire",
            map_toplevel_directive env x
          )
        | `Exp_item x -> R.Case ("Exp_item",
            map_expression_item env x
          )
        )
      in
      let v3 =
        R.List (List.map (fun x ->
          (match x with
          | `Rep_SEMISEMI_choice_stru_item_ext (v1, v2) -> R.Case ("Rep_SEMISEMI_choice_stru_item_ext",
              let v1 = R.List (List.map (token env (* ";;" *)) v1) in
              let v2 =
                (match v2 with
                | `Stru_item_ext x -> R.Case ("Stru_item_ext",
                    map_structure_item_ext env x
                  )
                | `Topl_dire x -> R.Case ("Topl_dire",
                    map_toplevel_directive env x
                  )
                )
              in
              R.Tuple [v1; v2]
            )
          | `Rep1_SEMISEMI_exp_item (v1, v2) -> R.Case ("Rep1_SEMISEMI_exp_item",
              let v1 = R.List (List.map (token env (* ";;" *)) v1) in
              let v2 = map_expression_item env v2 in
              R.Tuple [v1; v2]
            )
          )
        ) v3)
      in
      let v4 = R.List (List.map (token env (* ";;" *)) v4) in
      R.Tuple [v1; v2; v3; v4]
    )
  )

and map_structure_item (env : env) (x : CST.structure_item) =
  (match x with
  | `Value_defi x -> R.Case ("Value_defi",
      map_value_definition env x
    )
  | `Exte x -> R.Case ("Exte",
      map_external_ env x
    )
  | `Type_defi x -> R.Case ("Type_defi",
      map_type_definition env x
    )
  | `Exc_defi x -> R.Case ("Exc_defi",
      map_exception_definition env x
    )
  | `Module_defi x -> R.Case ("Module_defi",
      map_module_definition env x
    )
  | `Module_type_defi x -> R.Case ("Module_type_defi",
      map_module_type_definition env x
    )
  | `Open_module x -> R.Case ("Open_module",
      map_open_module env x
    )
  | `Incl_module (v1, v2, v3, v4) -> R.Case ("Incl_module",
      let v1 = (* "include" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_attribute env x
          ))
        | None -> R.Option None)
      in
      let v3 = map_module_expression_ext env v3 in
      let v4 = R.List (List.map (map_item_attribute env) v4) in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Class_defi x -> R.Case ("Class_defi",
      map_class_definition env x
    )
  | `Class_type_defi x -> R.Case ("Class_type_defi",
      map_class_type_definition env x
    )
  | `Floa_attr x -> R.Case ("Floa_attr",
      map_floating_attribute env x
    )
  )

and map_structure_item_ext (env : env) (x : CST.structure_item_ext) =
  (match x with
  | `Stru_item x -> R.Case ("Stru_item",
      map_structure_item env x
    )
  | `Item_exte x -> R.Case ("Item_exte",
      map_item_extension env x
    )
  )

and map_tag_spec (env : env) (x : CST.tag_spec) =
  (match x with
  | `Type_ext x -> R.Case ("Type_ext",
      map_type_ext env x
    )
  | `Tag_spec x -> R.Case ("Tag_spec",
      map_tag_specification env x
    )
  )

and map_tag_specification (env : env) ((v1, v2) : CST.tag_specification) =
  let v1 = map_tag env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2, v3, v4) -> R.Option (Some (
        let v1 = (* "of" *) token env v1 in
        let v2 =
          (match v2 with
          | Some tok -> R.Option (Some (
              (* "&" *) token env tok
            ))
          | None -> R.Option None)
        in
        let v3 = map_type_ext env v3 in
        let v4 =
          R.List (List.map (fun (v1, v2) ->
            let v1 = (* "&" *) token env v1 in
            let v2 = map_type_ext env v2 in
            R.Tuple [v1; v2]
          ) v4)
        in
        R.Tuple [v1; v2; v3; v4]
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2]

and map_then_clause (env : env) ((v1, v2) : CST.then_clause) =
  let v1 = (* "then" *) token env v1 in
  let v2 = map_expression_ext env v2 in
  R.Tuple [v1; v2]

and map_tuple_type_ (env : env) (x : CST.tuple_type_) =
  (match x with
  | `Simple_type x -> R.Case ("Simple_type",
      map_simple_type env x
    )
  | `Tuple_type (v1, v2, v3) -> R.Case ("Tuple_type",
      let v1 = map_tuple_type_ext env v1 in
      let v2 = (* "*" *) token env v2 in
      let v3 = map_simple_type_ext env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_tuple_type_ext (env : env) (x : CST.tuple_type_ext) =
  (match x with
  | `Tuple_type_ x -> R.Case ("Tuple_type_",
      map_tuple_type_ env x
    )
  | `Exte x -> R.Case ("Exte",
      map_extension env x
    )
  )

and map_type_ (env : env) (x : CST.type_) =
  (match x with
  | `Tuple_type_ x -> R.Case ("Tuple_type_",
      map_tuple_type_ env x
    )
  | `Func_type (v1, v2, v3) -> R.Case ("Func_type",
      let v1 =
        (match v1 with
        | `Typed_label x -> R.Case ("Typed_label",
            map_typed_label env x
          )
        | `Type_ext x -> R.Case ("Type_ext",
            map_type_ext env x
          )
        )
      in
      let v2 = (* "->" *) token env v2 in
      let v3 = map_type_ext env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Alia_type (v1, v2, v3) -> R.Case ("Alia_type",
      let v1 = map_type_ext env v1 in
      let v2 = (* "as" *) token env v2 in
      let v3 = map_type_variable env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_type_binding (env : env) ((v1, v2, v3) : CST.type_binding) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        map_type_params env x
      ))
    | None -> R.Option None)
  in
  let v2 =
    (match v2 with
    | `Id_opt_type_equa_opt_EQ_opt_priv_choice_vari_decl_rep_type_cons (v1, v2, v3, v4) -> R.Case ("Id_opt_type_equa_opt_EQ_opt_priv_choice_vari_decl_rep_type_cons",
        let v1 =
          (* pattern "[a-z_][a-zA-Z0-9_']*" *) token env v1
        in
        let v2 =
          (match v2 with
          | Some x -> R.Option (Some (
              map_type_equation env x
            ))
          | None -> R.Option None)
        in
        let v3 =
          (match v3 with
          | Some (v1, v2, v3) -> R.Option (Some (
              let v1 = (* "=" *) token env v1 in
              let v2 =
                (match v2 with
                | Some tok -> R.Option (Some (
                    (* "private" *) token env tok
                  ))
                | None -> R.Option None)
              in
              let v3 =
                (match v3 with
                | `Vari_decl x -> R.Case ("Vari_decl",
                    map_variant_declaration env x
                  )
                | `Record_decl x -> R.Case ("Record_decl",
                    map_record_declaration env x
                  )
                | `DOTDOT tok -> R.Case ("DOTDOT",
                    (* ".." *) token env tok
                  )
                )
              in
              R.Tuple [v1; v2; v3]
            ))
          | None -> R.Option None)
        in
        let v4 = R.List (List.map (map_type_constraint env) v4) in
        R.Tuple [v1; v2; v3; v4]
      )
    | `Type_cons_path_PLUSEQ_opt_priv_vari_decl (v1, v2, v3, v4) -> R.Case ("Type_cons_path_PLUSEQ_opt_priv_vari_decl",
        let v1 = map_type_constructor_path env v1 in
        let v2 = (* "+=" *) token env v2 in
        let v3 =
          (match v3 with
          | Some tok -> R.Option (Some (
              (* "private" *) token env tok
            ))
          | None -> R.Option None)
        in
        let v4 = map_variant_declaration env v4 in
        R.Tuple [v1; v2; v3; v4]
      )
    )
  in
  let v3 = R.List (List.map (map_item_attribute env) v3) in
  R.Tuple [v1; v2; v3]

and map_type_constraint (env : env) ((v1, v2, v3, v4) : CST.type_constraint) =
  let v1 = (* "constraint" *) token env v1 in
  let v2 = map_type_ext env v2 in
  let v3 = (* "=" *) token env v3 in
  let v4 = map_type_ext env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_type_definition (env : env) ((v1, v2, v3, v4, v5) : CST.type_definition) =
  let v1 = (* "type" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_attribute env x
      ))
    | None -> R.Option None)
  in
  let v3 =
    (match v3 with
    | Some tok -> R.Option (Some (
        (* "nonrec" *) token env tok
      ))
    | None -> R.Option None)
  in
  let v4 = map_type_binding env v4 in
  let v5 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "and" *) token env v1 in
      let v2 = map_type_binding env v2 in
      R.Tuple [v1; v2]
    ) v5)
  in
  R.Tuple [v1; v2; v3; v4; v5]

and map_type_equation (env : env) ((v1, v2, v3) : CST.type_equation) =
  let v1 = map_anon_choice_EQ_4ccabd6 env v1 in
  let v2 =
    (match v2 with
    | Some tok -> R.Option (Some (
        (* "private" *) token env tok
      ))
    | None -> R.Option None)
  in
  let v3 = map_type_ext env v3 in
  R.Tuple [v1; v2; v3]

and map_type_ext (env : env) (x : CST.type_ext) =
  (match x with
  | `Type x -> R.Case ("Type",
      map_type_ env x
    )
  | `Exte x -> R.Case ("Exte",
      map_extension env x
    )
  )

and map_type_parameter_constraint (env : env) ((v1, v2, v3, v4, v5) : CST.type_parameter_constraint) =
  let v1 = (* "constraint" *) token env v1 in
  let v2 = map_type_ext env v2 in
  let v3 = (* "=" *) token env v3 in
  let v4 = map_type_ext env v4 in
  let v5 = R.List (List.map (map_item_attribute env) v5) in
  R.Tuple [v1; v2; v3; v4; v5]

and map_typed (env : env) ((v1, v2) : CST.typed) =
  let v1 = (* ":" *) token env v1 in
  let v2 = map_type_ext env v2 in
  R.Tuple [v1; v2]

and map_typed_label (env : env) ((v1, v2, v3, v4) : CST.typed_label) =
  let v1 =
    (match v1 with
    | Some tok -> R.Option (Some (
        (* "?" *) token env tok
      ))
    | None -> R.Option None)
  in
  let v2 =
    (* pattern "[a-z_][a-zA-Z0-9_']*" *) token env v2
  in
  let v3 = (* ":" *) token env v3 in
  let v4 = map_type_ext env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_value_definition (env : env) ((v1, v2, v3) : CST.value_definition) =
  let v1 =
    (match v1 with
    | `Let_opt_attr_opt_rec (v1, v2, v3) -> R.Case ("Let_opt_attr_opt_rec",
        let v1 = (* "let" *) token env v1 in
        let v2 =
          (match v2 with
          | Some x -> R.Option (Some (
              map_attribute env x
            ))
          | None -> R.Option None)
        in
        let v3 =
          (match v3 with
          | Some tok -> R.Option (Some (
              (* "rec" *) token env tok
            ))
          | None -> R.Option None)
        in
        R.Tuple [v1; v2; v3]
      )
    | `Let_op tok -> R.Case ("Let_op",
        (* let_operator *) token env tok
      )
    )
  in
  let v2 = map_let_binding env v2 in
  let v3 =
    R.List (List.map (fun (v1, v2) ->
      let v1 =
        (match v1 with
        | `And tok -> R.Case ("And",
            (* "and" *) token env tok
          )
        | `Let_and_op tok -> R.Case ("Let_and_op",
            (* let_and_operator *) token env tok
          )
        )
      in
      let v2 = map_let_binding env v2 in
      R.Tuple [v1; v2]
    ) v3)
  in
  R.Tuple [v1; v2; v3]

and map_variant_declaration (env : env) (x : CST.variant_declaration) =
  (match x with
  | `BAR_opt_cons_decl_rep_BAR_cons_decl (v1, v2) -> R.Case ("BAR_opt_cons_decl_rep_BAR_cons_decl",
      let v1 = (* "|" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_anon_cons_decl_rep_BAR_cons_decl_fc0ccc5 env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2]
    )
  | `Cons_decl_rep_BAR_cons_decl x -> R.Case ("Cons_decl_rep_BAR_cons_decl",
      map_anon_cons_decl_rep_BAR_cons_decl_fc0ccc5 env x
    )
  )

let map_compilation_unit (env : env) (x : CST.compilation_unit) =
  (match x with
  | `Opt_sheb_opt_stru (v1, v2) -> R.Case ("Opt_sheb_opt_stru",
      let v1 =
        (match v1 with
        | Some tok -> R.Option (Some (
            (* pattern #!.* *) token env tok
          ))
        | None -> R.Option None)
      in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_structure env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2]
    )
  | `Sign x -> R.Case ("Sign",
      map_signature env x
    )
  )

let map_line_number_directive (env : env) (tok : CST.line_number_directive) =
  (* line_number_directive *) token env tok

let map_comment (env : env) (tok : CST.comment) =
  (* comment *) token env tok

let map_attribute_ (env : env) ((v1, v2, v3, v4) : CST.attribute_) =
  let v1 = map_pat_6c51254 env v1 in
  let v2 = map_attribute_id env v2 in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_attribute_payload env x
      ))
    | None -> R.Option None)
  in
  let v4 = (* "]" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

let dump_tree root =
  map_compilation_unit () root
  |> Tree_sitter_run.Raw_tree.to_channel stdout

let map_extra (env : env) (x : CST.extra) =
  match x with
  | `Comment (_loc, x) -> ("comment", "comment", map_comment env x)
  | `Line_number_directive (_loc, x) -> ("line_number_directive", "line_number_directive", map_line_number_directive env x)
  | `Attribute_ (_loc, x) -> ("attribute_", "attribute_", map_attribute_ env x)

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
