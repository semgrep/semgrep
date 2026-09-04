(**
   Boilerplate to be used as a template when mapping the move_on_aptos CST
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

let map_pat_c1c0c3a (env : env) (tok : CST.pat_c1c0c3a) =
  (* pattern [\da-fA-F][\da-fA-F_]* *) token env tok

let map_identifier (env : env) (tok : CST.identifier) =
  (* identifier *) token env tok

let map_imm_tok_prec_p1_pat_4fd4a56 (env : env) (tok : CST.imm_tok_prec_p1_pat_4fd4a56) =
  (* pattern .* *) token env tok

let map_pat_9bd0c11 (env : env) (tok : CST.pat_9bd0c11) =
  (* pattern [0-7][0-7_]* *) token env tok

let map_number_type (env : env) (x : CST.number_type) =
  (match x with
  | `U8 tok -> R.Case ("U8",
      (* "u8" *) token env tok
    )
  | `U16 tok -> R.Case ("U16",
      (* "u16" *) token env tok
    )
  | `U32 tok -> R.Case ("U32",
      (* "u32" *) token env tok
    )
  | `U64 tok -> R.Case ("U64",
      (* "u64" *) token env tok
    )
  | `U128 tok -> R.Case ("U128",
      (* "u128" *) token env tok
    )
  | `U256 tok -> R.Case ("U256",
      (* "u256" *) token env tok
    )
  )

let map_imm_tok_prec_p2_slashslash (env : env) (tok : CST.imm_tok_prec_p2_slashslash) =
  (* "//" *) token env tok

let map_pat_8bc5a9c (env : env) (tok : CST.pat_8bc5a9c) =
  (* pattern [01][01_]* *) token env tok

let map_block_doc_comment_marker (env : env) (tok : CST.block_doc_comment_marker) =
  (* block_doc_comment_marker *) token env tok

let map_pat_57a456d (env : env) (tok : CST.pat_57a456d) =
  (* pattern \d[\d_]* *) token env tok

let map_bool_literal (env : env) (x : CST.bool_literal) =
  (match x with
  | `True tok -> R.Case ("True",
      (* "true" *) token env tok
    )
  | `False tok -> R.Case ("False",
      (* "false" *) token env tok
    )
  )

let map_block_comment_content (env : env) (tok : CST.block_comment_content) =
  (* block_comment_content *) token env tok

let map_tok_bdquot_rep_choice_imm_tok_bslash_choice_pat_9c2bd89_dquot (env : env) (tok : CST.tok_bdquot_rep_choice_imm_tok_bslash_choice_pat_9c2bd89_dquot) =
  (* tok_bdquot_rep_choice_imm_tok_bslash_choice_pat_9c2bd89_dquot *) token env tok

let map_pat_4fd4a56 (env : env) (tok : CST.pat_4fd4a56) =
  (* pattern .* *) token env tok

let map_anon_field_index (env : env) (tok : CST.anon_field_index) =
  (* pattern \d+ *) token env tok

let map_imm_tok_lt (env : env) (tok : CST.imm_tok_lt) =
  (* "<" *) token env tok

let map_tok_xdquot_pat_92a0a93_dquot (env : env) (tok : CST.tok_xdquot_pat_92a0a93_dquot) =
  (* tok_xdquot_pat_92a0a93_dquot *) token env tok

let map_line_comment_explicit (env : env) (() : CST.line_comment_explicit) =
  R.Tuple []

let map_reuseable_keywords (env : env) (x : CST.reuseable_keywords) =
  (match x with
  | `For tok -> R.Case ("For",
      (* "for" *) token env tok
    )
  | `While tok -> R.Case ("While",
      (* "while" *) token env tok
    )
  | `Friend tok -> R.Case ("Friend",
      (* "friend" *) token env tok
    )
  | `Match tok -> R.Case ("Match",
      (* "match" *) token env tok
    )
  )

let map_pat_0o (env : env) (tok : CST.pat_0o) =
  (* pattern 0[oO] *) token env tok

let map_pat_0x (env : env) (tok : CST.pat_0x) =
  (* pattern 0[xX] *) token env tok

let map_block_comment_explicit (env : env) (() : CST.block_comment_explicit) =
  R.Tuple []

let map_imm_tok_prec_p2_slash (env : env) (tok : CST.imm_tok_prec_p2_slash) =
  (* "/" *) token env tok

let map_pat_0b (env : env) (tok : CST.pat_0b) =
  (* pattern 0[bB] *) token env tok

let map_doc_line_comment (env : env) (tok : CST.doc_line_comment) =
  (* doc_line_comment *) token env tok

let map_quantifier_directive (env : env) (x : CST.quantifier_directive) =
  (match x with
  | `Exists tok -> R.Case ("Exists",
      (* "exists" *) token env tok
    )
  | `Forall tok -> R.Case ("Forall",
      (* "forall" *) token env tok
    )
  | `Choose tok -> R.Case ("Choose",
      (* "choose" *) token env tok
    )
  | `Min tok -> R.Case ("Min",
      (* "min" *) token env tok
    )
  )

let map_primitive_type (env : env) (x : CST.primitive_type) =
  (match x with
  | `Num_type x -> R.Case ("Num_type",
      map_number_type env x
    )
  | `Bool tok -> R.Case ("Bool",
      (* "bool" *) token env tok
    )
  | `Addr tok -> R.Case ("Addr",
      (* "address" *) token env tok
    )
  | `Vec tok -> R.Case ("Vec",
      (* "vector" *) token env tok
    )
  )

let map_block_comment (env : env) ((v1, v2, v3) : CST.block_comment) =
  let v1 = (* "/*" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        (match x with
        | `Blk_doc_comm_marker_opt_blk_comm_content (v1, v2) -> R.Case ("Blk_doc_comm_marker_opt_blk_comm_content",
            let v1 = (* block_doc_comment_marker *) token env v1 in
            let v2 =
              (match v2 with
              | Some tok -> R.Option (Some (
                  (* block_comment_content *) token env tok
                ))
              | None -> R.Option None)
            in
            R.Tuple [v1; v2]
          )
        | `Blk_comm_content tok -> R.Case ("Blk_comm_content",
            (* block_comment_content *) token env tok
          )
        )
      ))
    | None -> R.Option None)
  in
  let v3 = (* "*/" *) token env v3 in
  R.Tuple [v1; v2; v3]

let map_ability (env : env) (x : CST.ability) =
  (match x with
  | `Choice_copy x -> R.Case ("Choice_copy",
      (match x with
      | `Copy tok -> R.Case ("Copy",
          (* "copy" *) token env tok
        )
      | `Drop tok -> R.Case ("Drop",
          (* "drop" *) token env tok
        )
      | `Store tok -> R.Case ("Store",
          (* "store" *) token env tok
        )
      | `Key tok -> R.Case ("Key",
          (* "key" *) token env tok
        )
      )
    )
  | `Ellips tok -> R.Case ("Ellips",
      (* "..." *) token env tok
    )
  )

let map_byte_string (env : env) (x : CST.byte_string) =
  (match x with
  | `Tok_xdquot_pat_92a0a93_dquot x -> R.Case ("Tok_xdquot_pat_92a0a93_dquot",
      map_tok_xdquot_pat_92a0a93_dquot env x
    )
  | `Tok_bdquot_rep_choice_imm_tok_bslash_choice_pat_9c2bd89_dquot x -> R.Case ("Tok_bdquot_rep_choice_imm_tok_bslash_choice_pat_9c2bd89_dquot",
      map_tok_bdquot_rep_choice_imm_tok_bslash_choice_pat_9c2bd89_dquot env x
    )
  )

let map_module_member_modifier (env : env) (x : CST.module_member_modifier) =
  (match x with
  | `Visi (v1, v2) -> R.Case ("Visi",
      let v1 = (* "public" *) token env v1 in
      let v2 =
        (match v2 with
        | Some (v1, v2, v3) -> R.Option (Some (
            let v1 = (* "(" *) token env v1 in
            let v2 =
              (match v2 with
              | `Script tok -> R.Case ("Script",
                  (* "script" *) token env tok
                )
              | `Friend tok -> R.Case ("Friend",
                  (* "friend" *) token env tok
                )
              | `Pack tok -> R.Case ("Pack",
                  (* "package" *) token env tok
                )
              )
            in
            let v3 = (* ")" *) token env v3 in
            R.Tuple [v1; v2; v3]
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2]
    )
  | `Native tok -> R.Case ("Native",
      (* "native" *) token env tok
    )
  | `Entry tok -> R.Case ("Entry",
      (* "entry" *) token env tok
    )
  )

let map_number (env : env) (x : CST.number) =
  (match x with
  | `Pat_57a456d x -> R.Case ("Pat_57a456d",
      map_pat_57a456d env x
    )
  | `Pat_0x_pat_c1c0c3a (v1, v2) -> R.Case ("Pat_0x_pat_c1c0c3a",
      let v1 = map_pat_0x env v1 in
      let v2 = map_pat_c1c0c3a env v2 in
      R.Tuple [v1; v2]
    )
  | `Pat_0b_pat_8bc5a9c (v1, v2) -> R.Case ("Pat_0b_pat_8bc5a9c",
      let v1 = map_pat_0b env v1 in
      let v2 = map_pat_8bc5a9c env v2 in
      R.Tuple [v1; v2]
    )
  | `Pat_0o_pat_9bd0c11 (v1, v2) -> R.Case ("Pat_0o_pat_9bd0c11",
      let v1 = map_pat_0o env v1 in
      let v2 = map_pat_9bd0c11 env v2 in
      R.Tuple [v1; v2]
    )
  )

let map_line_comment (env : env) ((v1, v2) : CST.line_comment) =
  let v1 = (* "//" *) token env v1 in
  let v2 =
    (match v2 with
    | `Imm_tok_prec_p2_slas_pat_4fd4a56 (v1, v2) -> R.Case ("Imm_tok_prec_p2_slas_pat_4fd4a56",
        let v1 = map_imm_tok_prec_p2_slashslash env v1 in
        let v2 = map_pat_4fd4a56 env v2 in
        R.Tuple [v1; v2]
      )
    | `Imm_tok_prec_p2_slash_doc_line_comm (v1, v2) -> R.Case ("Imm_tok_prec_p2_slash_doc_line_comm",
        let v1 = map_imm_tok_prec_p2_slash env v1 in
        let v2 = (* doc_line_comment *) token env v2 in
        R.Tuple [v1; v2]
      )
    | `Imm_tok_prec_p1_pat_4fd4a56 x -> R.Case ("Imm_tok_prec_p1_pat_4fd4a56",
        map_imm_tok_prec_p1_pat_4fd4a56 env x
      )
    )
  in
  R.Tuple [v1; v2]

let map_identifier_or_anon_field (env : env) (x : CST.identifier_or_anon_field) =
  (match x with
  | `Id tok -> R.Case ("Id",
      (* identifier *) token env tok
    )
  | `Anon_field_index tok -> R.Case ("Anon_field_index",
      (* pattern \d+ *) token env tok
    )
  )

let map_attribute_name (env : env) ((v1, v2) : CST.attribute_name) =
  let v1 = (* identifier *) token env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "::" *) token env v1 in
      let v2 = (* identifier *) token env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  R.Tuple [v1; v2]

let map_spec_apply_fragment (env : env) (x : CST.spec_apply_fragment) =
  (match x with
  | `STAR tok -> R.Case ("STAR",
      (* "*" *) token env tok
    )
  | `Id tok -> R.Case ("Id",
      (* identifier *) token env tok
    )
  )

let map_ident_or_wildcard (env : env) (x : CST.ident_or_wildcard) =
  (match x with
  | `Id tok -> R.Case ("Id",
      (* identifier *) token env tok
    )
  | `STAR tok -> R.Case ("STAR",
      (* "*" *) token env tok
    )
  )

let map_use_alias (env : env) ((v1, v2) : CST.use_alias) =
  let v1 = (* "as" *) token env v1 in
  let v2 = (* identifier *) token env v2 in
  R.Tuple [v1; v2]

let map_discouraged_name (env : env) (x : CST.discouraged_name) =
  (match x with
  | `Prim_type x -> R.Case ("Prim_type",
      map_primitive_type env x
    )
  | `Quan_dire x -> R.Case ("Quan_dire",
      map_quantifier_directive env x
    )
  | `Reus_keywos x -> R.Case ("Reus_keywos",
      map_reuseable_keywords env x
    )
  )

let map_abilities (env : env) ((v1, v2, v3) : CST.abilities) =
  let v1 = (* "has" *) token env v1 in
  let v2 = map_ability env v2 in
  let v3 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_ability env v2 in
      R.Tuple [v1; v2]
    ) v3)
  in
  R.Tuple [v1; v2; v3]

let map_constraints (env : env) ((v1, v2, v3) : CST.constraints) =
  let v1 = (* ":" *) token env v1 in
  let v2 = map_ability env v2 in
  let v3 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "+" *) token env v1 in
      let v2 = map_ability env v2 in
      R.Tuple [v1; v2]
    ) v3)
  in
  R.Tuple [v1; v2; v3]

let map_numerical_addr (env : env) (x : CST.numerical_addr) =
  map_number env x

let map_use_member (env : env) (x : CST.use_member) =
  (match x with
  | `Id_opt_use_alias (v1, v2) -> R.Case ("Id_opt_use_alias",
      let v1 = (* identifier *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_use_alias env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2]
    )
  | `Ellips tok -> R.Case ("Ellips",
      (* "..." *) token env tok
    )
  )

let map_var_name (env : env) (x : CST.var_name) =
  (match x with
  | `Id tok -> R.Case ("Id",
      (* identifier *) token env tok
    )
  | `Disc_name x -> R.Case ("Disc_name",
      map_discouraged_name env x
    )
  )

let map_type_param (env : env) (x : CST.type_param) =
  (match x with
  | `Id_opt_consts (v1, v2) -> R.Case ("Id_opt_consts",
      let v1 = (* identifier *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_constraints env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2]
    )
  | `Ellips tok -> R.Case ("Ellips",
      (* "..." *) token env tok
    )
  )

let map_leading_name_access (env : env) (x : CST.leading_name_access) =
  (match x with
  | `Nume_addr x -> R.Case ("Nume_addr",
      map_numerical_addr env x
    )
  | `Id tok -> R.Case ("Id",
      (* identifier *) token env tok
    )
  )

let map_leading_name_access_wildcard (env : env) (x : CST.leading_name_access_wildcard) =
  (match x with
  | `Nume_addr x -> R.Case ("Nume_addr",
      map_numerical_addr env x
    )
  | `Choice_id x -> R.Case ("Choice_id",
      map_ident_or_wildcard env x
    )
  )

let map_struct_type_parameter (env : env) ((v1, v2) : CST.struct_type_parameter) =
  let v1 =
    (match v1 with
    | Some tok -> R.Option (Some (
        (* "phantom" *) token env tok
      ))
    | None -> R.Option None)
  in
  let v2 = map_type_param env v2 in
  R.Tuple [v1; v2]

let map_type_params (env : env) ((v1, v2, v3, v4) : CST.type_params) =
  let v1 = (* "<" *) token env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = map_type_param env v1 in
        let v2 =
          R.List (List.map (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_type_param env v2 in
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
  let v4 = (* ">" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

let map_module_ident (env : env) ((v1, v2, v3) : CST.module_ident) =
  let v1 = map_leading_name_access env v1 in
  let v2 = (* "::" *) token env v2 in
  let v3 = (* identifier *) token env v3 in
  R.Tuple [v1; v2; v3]

let map_module_path (env : env) ((v1, v2) : CST.module_path) =
  let v1 =
    (match v1 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = map_leading_name_access env v1 in
        let v2 = (* "::" *) token env v2 in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  let v2 = (* identifier *) token env v2 in
  R.Tuple [v1; v2]

let map_name_access_chain (env : env) (x : CST.name_access_chain) =
  (match x with
  | `Choice_id x -> R.Case ("Choice_id",
      map_var_name env x
    )
  | `Choice_lead_name_access_COLONCOLON_id_opt_COLONCOLON_id_opt_COLONCOLON_id (v1, v2, v3, v4) -> R.Case ("Choice_lead_name_access_COLONCOLON_id_opt_COLONCOLON_id_opt_COLONCOLON_id",
      let v1 =
        (match v1 with
        | `Lead_name_access x -> R.Case ("Lead_name_access",
            map_leading_name_access env x
          )
        | `Disc_name x -> R.Case ("Disc_name",
            map_discouraged_name env x
          )
        )
      in
      let v2 = (* "::" *) token env v2 in
      let v3 = (* identifier *) token env v3 in
      let v4 =
        (match v4 with
        | Some (v1, v2, v3) -> R.Option (Some (
            let v1 = (* "::" *) token env v1 in
            let v2 = (* identifier *) token env v2 in
            let v3 =
              (match v3 with
              | Some (v1, v2) -> R.Option (Some (
                  let v1 = (* "::" *) token env v1 in
                  let v2 = (* identifier *) token env v2 in
                  R.Tuple [v1; v2]
                ))
              | None -> R.Option None)
            in
            R.Tuple [v1; v2; v3]
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3; v4]
    )
  )

let map_name_access_chain_wildcard (env : env) (x : CST.name_access_chain_wildcard) =
  (match x with
  | `Choice_choice_id x -> R.Case ("Choice_choice_id",
      (match x with
      | `Choice_id x -> R.Case ("Choice_id",
          map_ident_or_wildcard env x
        )
      | `Disc_name x -> R.Case ("Disc_name",
          map_discouraged_name env x
        )
      )
    )
  | `Choice_lead_name_access_wild_COLONCOLON_choice_id_opt_COLONCOLON_choice_id_opt_COLONCOLON_choice_id (v1, v2, v3, v4) -> R.Case ("Choice_lead_name_access_wild_COLONCOLON_choice_id_opt_COLONCOLON_choice_id_opt_COLONCOLON_choice_id",
      let v1 =
        (match v1 with
        | `Lead_name_access_wild x -> R.Case ("Lead_name_access_wild",
            map_leading_name_access_wildcard env x
          )
        | `Disc_name x -> R.Case ("Disc_name",
            map_discouraged_name env x
          )
        )
      in
      let v2 = (* "::" *) token env v2 in
      let v3 = map_ident_or_wildcard env v3 in
      let v4 =
        (match v4 with
        | Some (v1, v2, v3) -> R.Option (Some (
            let v1 = (* "::" *) token env v1 in
            let v2 = map_ident_or_wildcard env v2 in
            let v3 =
              (match v3 with
              | Some (v1, v2) -> R.Option (Some (
                  let v1 = (* "::" *) token env v1 in
                  let v2 = map_ident_or_wildcard env v2 in
                  R.Tuple [v1; v2]
                ))
              | None -> R.Option None)
            in
            R.Tuple [v1; v2; v3]
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3; v4]
    )
  )

let map_value (env : env) (x : CST.value) =
  (match x with
  | `AT_lead_name_access (v1, v2) -> R.Case ("AT_lead_name_access",
      let v1 = (* "@" *) token env v1 in
      let v2 = map_leading_name_access env v2 in
      R.Tuple [v1; v2]
    )
  | `Bool_lit x -> R.Case ("Bool_lit",
      map_bool_literal env x
    )
  | `Num x -> R.Case ("Num",
      map_numerical_addr env x
    )
  | `Typed_num (v1, v2) -> R.Case ("Typed_num",
      let v1 = map_numerical_addr env v1 in
      let v2 = map_number_type env v2 in
      R.Tuple [v1; v2]
    )
  | `Byte_str x -> R.Case ("Byte_str",
      map_byte_string env x
    )
  )

let map_struct_type_params (env : env) ((v1, v2, v3, v4) : CST.struct_type_params) =
  let v1 = (* "<" *) token env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = map_struct_type_parameter env v1 in
        let v2 =
          R.List (List.map (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_struct_type_parameter env v2 in
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
  let v4 = (* ">" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

let map_spec_apply_pattern (env : env) ((v1, v2, v3) : CST.spec_apply_pattern) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        (match x with
        | `Public tok -> R.Case ("Public",
            (* "public" *) token env tok
          )
        | `Inte tok -> R.Case ("Inte",
            (* "internal" *) token env tok
          )
        )
      ))
    | None -> R.Option None)
  in
  let v2 =
    R.List (List.map (map_spec_apply_fragment env) v2)
  in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_type_params env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3]

let map_use_decl (env : env) ((v1, v2, v3, v4) : CST.use_decl) =
  let v1 = (* "use" *) token env v1 in
  let v2 = map_module_ident env v2 in
  let v3 =
    (match v3 with
    | `Opt_use_alias opt -> R.Case ("Opt_use_alias",
        (match opt with
        | Some x -> R.Option (Some (
            map_use_alias env x
          ))
        | None -> R.Option None)
      )
    | `COLONCOLON_use_member (v1, v2) -> R.Case ("COLONCOLON_use_member",
        let v1 = (* "::" *) token env v1 in
        let v2 = map_use_member env v2 in
        R.Tuple [v1; v2]
      )
    | `COLONCOLON_LCURL_opt_use_member_rep_COMMA_use_member_opt_COMMA_RCURL (v1, v2, v3, v4, v5) -> R.Case ("COLONCOLON_LCURL_opt_use_member_rep_COMMA_use_member_opt_COMMA_RCURL",
        let v1 = (* "::" *) token env v1 in
        let v2 = (* "{" *) token env v2 in
        let v3 =
          (match v3 with
          | Some (v1, v2) -> R.Option (Some (
              let v1 = map_use_member env v1 in
              let v2 =
                R.List (List.map (fun (v1, v2) ->
                  let v1 = (* "," *) token env v1 in
                  let v2 = map_use_member env v2 in
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
    )
  in
  let v4 = (* ";" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

let rec map_anon_type__rep_COMMA_type__ac59fb8 (env : env) ((v1, v2) : CST.anon_type__rep_COMMA_type__ac59fb8) =
  let v1 = map_type__ env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_type__ env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  R.Tuple [v1; v2]

and map_ref_type (env : env) (x : CST.ref_type) =
  (match x with
  | `AMP_type (v1, v2) -> R.Case ("AMP_type",
      let v1 = (* "&" *) token env v1 in
      let v2 = map_type_ env v2 in
      R.Tuple [v1; v2]
    )
  | `AMPmut_type (v1, v2) -> R.Case ("AMPmut_type",
      let v1 = (* "&mut" *) token env v1 in
      let v2 = map_type_ env v2 in
      R.Tuple [v1; v2]
    )
  )

and map_type_ (env : env) (x : CST.type_) =
  (match x with
  | `Choice_name_access_chain_opt_type_args (v1, v2) -> R.Case ("Choice_name_access_chain_opt_type_args",
      let v1 =
        (match v1 with
        | `Name_access_chain x -> R.Case ("Name_access_chain",
            map_name_access_chain env x
          )
        | `Prim_type x -> R.Case ("Prim_type",
            map_primitive_type env x
          )
        )
      in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_type_args env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2]
    )
  | `Clos_type (v1, v2, v3, v4, v5) -> R.Case ("Clos_type",
      let v1 = (* "|" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_anon_type__rep_COMMA_type__ac59fb8 env x
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
      let v4 = (* "|" *) token env v4 in
      let v5 =
        (match v5 with
        | Some x -> R.Option (Some (
            map_type__ env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `Tuple_type (v1, v2, v3, v4) -> R.Case ("Tuple_type",
      let v1 = (* "(" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_anon_type__rep_COMMA_type__ac59fb8 env x
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
      let v4 = (* ")" *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  )

and map_type__ (env : env) (x : CST.type__) =
  (match x with
  | `Type x -> R.Case ("Type",
      map_type_ env x
    )
  | `Ref_type x -> R.Case ("Ref_type",
      map_ref_type env x
    )
  | `Ellips tok -> R.Case ("Ellips",
      (* "..." *) token env tok
    )
  )

and map_type_args (env : env) ((v1, v2, v3, v4) : CST.type_args) =
  let v1 = map_imm_tok_lt env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_anon_type__rep_COMMA_type__ac59fb8 env x
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
  let v4 = (* ">" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

let map_friend_decl (env : env) ((v1, v2, v3) : CST.friend_decl) =
  let v1 = (* "friend" *) token env v1 in
  let v2 = map_name_access_chain env v2 in
  let v3 = (* ";" *) token env v3 in
  R.Tuple [v1; v2; v3]

let map_anon_choice_value_f266929 (env : env) (x : CST.anon_choice_value_f266929) =
  (match x with
  | `Value x -> R.Case ("Value",
      map_value env x
    )
  | `Name_access_chain x -> R.Case ("Name_access_chain",
      map_name_access_chain env x
    )
  )

let map_struct_def_name (env : env) ((v1, v2) : CST.struct_def_name) =
  let v1 = (* identifier *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_struct_type_params env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2]

let map_anon_spec_apply_pat_rep_COMMA_spec_apply_pat_d9a21d6 (env : env) ((v1, v2) : CST.anon_spec_apply_pat_rep_COMMA_spec_apply_pat_d9a21d6) =
  let v1 = map_spec_apply_pattern env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_spec_apply_pattern env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  R.Tuple [v1; v2]

let map_anon_fields (env : env) ((v1, v2, v3, v4) : CST.anon_fields) =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_anon_type__rep_COMMA_type__ac59fb8 env x
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
  let v4 = (* ")" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

let rec map_anon_bind_rep_COMMA_bind_38cc8c1 (env : env) ((v1, v2) : CST.anon_bind_rep_COMMA_bind_38cc8c1) =
  let v1 = map_bind env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_bind env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  R.Tuple [v1; v2]

and map_bind (env : env) (x : CST.bind) =
  (match x with
  | `Var_name x -> R.Case ("Var_name",
      map_var_name env x
    )
  | `Name_access_chain_opt_type_args_opt_choice_bind_fields (v1, v2, v3) -> R.Case ("Name_access_chain_opt_type_args_opt_choice_bind_fields",
      let v1 = map_name_access_chain env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_type_args env x
          ))
        | None -> R.Option None)
      in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            (match x with
            | `Bind_fields x -> R.Case ("Bind_fields",
                map_bind_fields env x
              )
            | `Bind_tuple x -> R.Case ("Bind_tuple",
                map_bind_tuple env x
              )
            )
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3]
    )
  | `Ellips tok -> R.Case ("Ellips",
      (* "..." *) token env tok
    )
  )

and map_bind_field (env : env) (x : CST.bind_field) =
  (match x with
  | `Choice_var_name x -> R.Case ("Choice_var_name",
      (match x with
      | `Var_name x -> R.Case ("Var_name",
          map_var_name env x
        )
      | `Var_name_COLON_bind (v1, v2, v3) -> R.Case ("Var_name_COLON_bind",
          let v1 = map_var_name env v1 in
          let v2 = (* ":" *) token env v2 in
          let v3 = map_bind env v3 in
          R.Tuple [v1; v2; v3]
        )
      )
    )
  | `Ellips tok -> R.Case ("Ellips",
      (* "..." *) token env tok
    )
  )

and map_bind_fields (env : env) ((v1, v2, v3, v4) : CST.bind_fields) =
  let v1 = (* "{" *) token env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = map_bind_field env v1 in
        let v2 =
          R.List (List.map (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_bind_field env v2 in
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

and map_bind_tuple (env : env) ((v1, v2, v3, v4) : CST.bind_tuple) =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_anon_bind_rep_COMMA_bind_38cc8c1 env x
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
  let v4 = (* ")" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

let map_parameter (env : env) (x : CST.parameter) =
  (match x with
  | `Id_COLON_type_ (v1, v2, v3) -> R.Case ("Id_COLON_type_",
      let v1 = (* identifier *) token env v1 in
      let v2 = (* ":" *) token env v2 in
      let v3 = map_type__ env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Ellips tok -> R.Case ("Ellips",
      (* "..." *) token env tok
    )
  )

let map_field_annot (env : env) (x : CST.field_annot) =
  (match x with
  | `Id_COLON_type_ (v1, v2, v3) -> R.Case ("Id_COLON_type_",
      let v1 = (* identifier *) token env v1 in
      let v2 = (* ":" *) token env v2 in
      let v3 = map_type__ env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Ellips tok -> R.Case ("Ellips",
      (* "..." *) token env tok
    )
  )

let map_address_specifier (env : env) (x : CST.address_specifier) =
  (match x with
  | `STAR tok -> R.Case ("STAR",
      (* "*" *) token env tok
    )
  | `Nume_addr x -> R.Case ("Nume_addr",
      map_numerical_addr env x
    )
  | `Name_access_chain_opt_opt_type_args_LPAR_id_RPAR (v1, v2) -> R.Case ("Name_access_chain_opt_opt_type_args_LPAR_id_RPAR",
      let v1 = map_name_access_chain env v1 in
      let v2 =
        (match v2 with
        | Some (v1, v2, v3, v4) -> R.Option (Some (
            let v1 =
              (match v1 with
              | Some x -> R.Option (Some (
                  map_type_args env x
                ))
              | None -> R.Option None)
            in
            let v2 = (* "(" *) token env v2 in
            let v3 = (* identifier *) token env v3 in
            let v4 = (* ")" *) token env v4 in
            R.Tuple [v1; v2; v3; v4]
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2]
    )
  )

let map_attribute_val (env : env) (x : CST.attribute_val) =
  (match x with
  | `Choice_value x -> R.Case ("Choice_value",
      map_anon_choice_value_f266929 env x
    )
  | `Ellips tok -> R.Case ("Ellips",
      (* "..." *) token env tok
    )
  )

let map_spec_pragma_prop (env : env) ((v1, v2) : CST.spec_pragma_prop) =
  let v1 = map_var_name env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = (* "=" *) token env v1 in
        let v2 = map_anon_choice_value_f266929 env v2 in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2]

let map_struct_signature (env : env) ((v1, v2, v3) : CST.struct_signature) =
  let v1 = (* "struct" *) token env v1 in
  let v2 = map_struct_def_name env v2 in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_abilities env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3]

let map_enum_signature (env : env) ((v1, v2, v3) : CST.enum_signature) =
  let v1 = (* "enum" *) token env v1 in
  let v2 = map_struct_def_name env v2 in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_abilities env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3]

let map_enum_variant_posit (env : env) ((v1, v2) : CST.enum_variant_posit) =
  let v1 = (* identifier *) token env v1 in
  let v2 = map_anon_fields env v2 in
  R.Tuple [v1; v2]

let map_bind_list (env : env) (x : CST.bind_list) =
  (match x with
  | `Bind x -> R.Case ("Bind",
      map_bind env x
    )
  | `LPAR_opt_bind_rep_COMMA_bind_opt_COMMA_RPAR x -> R.Case ("LPAR_opt_bind_rep_COMMA_bind_opt_COMMA_RPAR",
      map_bind_tuple env x
    )
  )

let map_lambda_bind_list (env : env) ((v1, v2, v3, v4) : CST.lambda_bind_list) =
  let v1 = (* "|" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_anon_bind_rep_COMMA_bind_38cc8c1 env x
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
  let v4 = (* "|" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

let map_parameters (env : env) ((v1, v2, v3, v4) : CST.parameters) =
  let v1 = (* "(" *) token env v1 in
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
  let v3 =
    (match v3 with
    | Some tok -> R.Option (Some (
        (* "," *) token env tok
      ))
    | None -> R.Option None)
  in
  let v4 = (* ")" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

let map_struct_body (env : env) ((v1, v2, v3, v4) : CST.struct_body) =
  let v1 = (* "{" *) token env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = map_field_annot env v1 in
        let v2 =
          R.List (List.map (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_field_annot env v2 in
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

let map_access_specifier (env : env) ((v1, v2, v3) : CST.access_specifier) =
  let v1 = map_name_access_chain_wildcard env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_type_args env x
      ))
    | None -> R.Option None)
  in
  let v3 =
    (match v3 with
    | Some (v1, v2, v3) -> R.Option (Some (
        let v1 = (* "(" *) token env v1 in
        let v2 = map_address_specifier env v2 in
        let v3 = (* ")" *) token env v3 in
        R.Tuple [v1; v2; v3]
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3]

let rec map_anon_attr_rep_COMMA_attr_246bec5 (env : env) ((v1, v2) : CST.anon_attr_rep_COMMA_attr_246bec5) =
  let v1 = map_attribute env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_attribute env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  R.Tuple [v1; v2]

and map_attribute (env : env) (x : CST.attribute) =
  (match x with
  | `Choice_attr_name x -> R.Case ("Choice_attr_name",
      (match x with
      | `Attr_name x -> R.Case ("Attr_name",
          map_attribute_name env x
        )
      | `Attr_name_EQ_attr_val (v1, v2, v3) -> R.Case ("Attr_name_EQ_attr_val",
          let v1 = map_attribute_name env v1 in
          let v2 = (* "=" *) token env v2 in
          let v3 = map_attribute_val env v3 in
          R.Tuple [v1; v2; v3]
        )
      | `Attr_name_LPAR_opt_attr_rep_COMMA_attr_opt_COMMA_RPAR (v1, v2, v3, v4, v5) -> R.Case ("Attr_name_LPAR_opt_attr_rep_COMMA_attr_opt_COMMA_RPAR",
          let v1 = map_attribute_name env v1 in
          let v2 = (* "(" *) token env v2 in
          let v3 =
            (match v3 with
            | Some x -> R.Option (Some (
                map_anon_attr_rep_COMMA_attr_246bec5 env x
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
          let v5 = (* ")" *) token env v5 in
          R.Tuple [v1; v2; v3; v4; v5]
        )
      )
    )
  | `Ellips tok -> R.Case ("Ellips",
      (* "..." *) token env tok
    )
  )

let map_anon_spec_pragma_prop_rep_COMMA_spec_pragma_prop_588d25f (env : env) ((v1, v2) : CST.anon_spec_pragma_prop_rep_COMMA_spec_pragma_prop_588d25f) =
  let v1 = map_spec_pragma_prop env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_spec_pragma_prop env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  R.Tuple [v1; v2]

let map_spec_target_signature_opt (env : env) ((v1, v2, v3) : CST.spec_target_signature_opt) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        map_type_params env x
      ))
    | None -> R.Option None)
  in
  let v2 = map_parameters env v2 in
  let v3 =
    (match v3 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = (* ":" *) token env v1 in
        let v2 = map_type__ env v2 in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3]

let map_spec_func_signatures (env : env) ((v1, v2, v3, v4, v5) : CST.spec_func_signatures) =
  let v1 = (* identifier *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_type_params env x
      ))
    | None -> R.Option None)
  in
  let v3 = map_parameters env v3 in
  let v4 = (* ":" *) token env v4 in
  let v5 = map_type__ env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

let map_struct_decl (env : env) (x : CST.struct_decl) =
  (match x with
  | `Struct_sign_choice_struct_body (v1, v2) -> R.Case ("Struct_sign_choice_struct_body",
      let v1 = map_struct_signature env v1 in
      let v2 =
        (match v2 with
        | `Struct_body x -> R.Case ("Struct_body",
            map_struct_body env x
          )
        | `SEMI tok -> R.Case ("SEMI",
            (* ";" *) token env tok
          )
        )
      in
      R.Tuple [v1; v2]
    )
  | `Struct_struct_def_name_struct_body_opt_abilis_SEMI (v1, v2, v3, v4, v5) -> R.Case ("Struct_struct_def_name_struct_body_opt_abilis_SEMI",
      let v1 = (* "struct" *) token env v1 in
      let v2 = map_struct_def_name env v2 in
      let v3 = map_struct_body env v3 in
      let v4 =
        (match v4 with
        | Some x -> R.Option (Some (
            map_abilities env x
          ))
        | None -> R.Option None)
      in
      let v5 = (* ";" *) token env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `Struct_struct_def_name_anon_fields_opt_abilis_SEMI (v1, v2, v3, v4, v5) -> R.Case ("Struct_struct_def_name_anon_fields_opt_abilis_SEMI",
      let v1 = (* "struct" *) token env v1 in
      let v2 = map_struct_def_name env v2 in
      let v3 = map_anon_fields env v3 in
      let v4 =
        (match v4 with
        | Some x -> R.Option (Some (
            map_abilities env x
          ))
        | None -> R.Option None)
      in
      let v5 = (* ";" *) token env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  )

let map_enum_variant_struct (env : env) ((v1, v2) : CST.enum_variant_struct) =
  let v1 = (* identifier *) token env v1 in
  let v2 = map_struct_body env v2 in
  R.Tuple [v1; v2]

let map_access_specifier_list (env : env) ((v1, v2, v3) : CST.access_specifier_list) =
  let v1 = map_access_specifier env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_access_specifier env v2 in
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

let map_attributes (env : env) (xs : CST.attributes) =
  R.List (List.map (fun (v1, v2, v3, v4, v5) ->
    let v1 = (* "#" *) token env v1 in
    let v2 = (* "[" *) token env v2 in
    let v3 =
      (match v3 with
      | Some x -> R.Option (Some (
          map_anon_attr_rep_COMMA_attr_246bec5 env x
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
    let v5 = (* "]" *) token env v5 in
    R.Tuple [v1; v2; v3; v4; v5]
  ) xs)

let map_spec_pragma (env : env) ((v1, v2, v3, v4) : CST.spec_pragma) =
  let v1 = (* "pragma" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_anon_spec_pragma_prop_rep_COMMA_spec_pragma_prop_588d25f env x
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
  let v4 = (* ";" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

let map_condition_props (env : env) ((v1, v2, v3, v4) : CST.condition_props) =
  let v1 = (* "[" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_anon_spec_pragma_prop_rep_COMMA_spec_pragma_prop_588d25f env x
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
  let v4 = (* "]" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

let map_spec_block_target (env : env) (x : CST.spec_block_target) =
  (match x with
  | `Id_opt_spec_target_sign_opt (v1, v2) -> R.Case ("Id_opt_spec_target_sign_opt",
      let v1 = (* identifier *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_spec_target_signature_opt env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2]
    )
  | `Module tok -> R.Case ("Module",
      (* "module" *) token env tok
    )
  | `Schema_id_opt_type_params (v1, v2, v3) -> R.Case ("Schema_id_opt_type_params",
      let v1 = (* "schema" *) token env v1 in
      let v2 = (* identifier *) token env v2 in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_type_params env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3]
    )
  )

let map_variant (env : env) (x : CST.variant) =
  (match x with
  | `Choice_enum_vari_struct_opt_COMMA x -> R.Case ("Choice_enum_vari_struct_opt_COMMA",
      (match x with
      | `Enum_vari_struct_opt_COMMA (v1, v2) -> R.Case ("Enum_vari_struct_opt_COMMA",
          let v1 = map_enum_variant_struct env v1 in
          let v2 =
            (match v2 with
            | Some tok -> R.Option (Some (
                (* "," *) token env tok
              ))
            | None -> R.Option None)
          in
          R.Tuple [v1; v2]
        )
      | `Enum_vari_COMMA (v1, v2) -> R.Case ("Enum_vari_COMMA",
          let v1 = (* identifier *) token env v1 in
          let v2 = (* "," *) token env v2 in
          R.Tuple [v1; v2]
        )
      | `Enum_vari_posit_COMMA (v1, v2) -> R.Case ("Enum_vari_posit_COMMA",
          let v1 = map_enum_variant_posit env v1 in
          let v2 = (* "," *) token env v2 in
          R.Tuple [v1; v2]
        )
      )
    )
  | `Ellips tok -> R.Case ("Ellips",
      (* "..." *) token env tok
    )
  )

let map_variant_last (env : env) ((v1, v2) : CST.variant_last) =
  let v1 =
    (match v1 with
    | `Enum_vari tok -> R.Case ("Enum_vari",
        (* identifier *) token env tok
      )
    | `Enum_vari_struct x -> R.Case ("Enum_vari_struct",
        map_enum_variant_struct env x
      )
    | `Enum_vari_posit x -> R.Case ("Enum_vari_posit",
        map_enum_variant_posit env x
      )
    )
  in
  let v2 =
    (match v2 with
    | Some tok -> R.Option (Some (
        (* "," *) token env tok
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2]

let map_specifier (env : env) (x : CST.specifier) =
  (match x with
  | `Pure tok -> R.Case ("Pure",
      (* "pure" *) token env tok
    )
  | `Rep1_opt_BANG_choice_acquis_access_spec_list xs -> R.Case ("Rep1_opt_BANG_choice_acquis_access_spec_list",
      R.List (List.map (fun (v1, v2, v3) ->
        let v1 =
          (match v1 with
          | Some tok -> R.Option (Some (
              (* "!" *) token env tok
            ))
          | None -> R.Option None)
        in
        let v2 =
          (match v2 with
          | `Acquis tok -> R.Case ("Acquis",
              (* "acquires" *) token env tok
            )
          | `Reads tok -> R.Case ("Reads",
              (* "reads" *) token env tok
            )
          | `Writes tok -> R.Case ("Writes",
              (* "writes" *) token env tok
            )
          )
        in
        let v3 = map_access_specifier_list env v3 in
        R.Tuple [v1; v2; v3]
      ) xs)
    )
  )

let map_script_use_decl (env : env) (x : CST.script_use_decl) =
  (match x with
  | `Opt_attris_use_decl (v1, v2) -> R.Case ("Opt_attris_use_decl",
      let v1 =
        (match v1 with
        | Some x -> R.Option (Some (
            map_attributes env x
          ))
        | None -> R.Option None)
      in
      let v2 = map_use_decl env v2 in
      R.Tuple [v1; v2]
    )
  | `Ellips tok -> R.Case ("Ellips",
      (* "..." *) token env tok
    )
  )

let rec map_aborts_if (env : env) ((v1, v2, v3, v4) : CST.aborts_if) =
  let v1 = (* "aborts_if" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_condition_props env x
      ))
    | None -> R.Option None)
  in
  let v3 = map_expr env v3 in
  let v4 =
    (match v4 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = (* "with" *) token env v1 in
        let v2 = map_expr env v2 in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3; v4]

and map_aborts_with_or_modifies (env : env) ((v1, v2, v3, v4) : CST.aborts_with_or_modifies) =
  let v1 =
    (match v1 with
    | `Aborts_with tok -> R.Case ("Aborts_with",
        (* "aborts_with" *) token env tok
      )
    | `Modifs tok -> R.Case ("Modifs",
        (* "modifies" *) token env tok
      )
    )
  in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_condition_props env x
      ))
    | None -> R.Option None)
  in
  let v3 = map_expr env v3 in
  let v4 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_expr env v2 in
      R.Tuple [v1; v2]
    ) v4)
  in
  R.Tuple [v1; v2; v3; v4]

and map_anon_choice_blk_f78fea4 (env : env) (x : CST.anon_choice_blk_f78fea4) =
  (match x with
  | `Blk x -> R.Case ("Blk",
      map_block env x
    )
  | `SEMI tok -> R.Case ("SEMI",
      (* ";" *) token env tok
    )
  )

and map_anon_expr_rep_COMMA_expr_8e432c6 (env : env) ((v1, v2) : CST.anon_expr_rep_COMMA_expr_8e432c6) =
  let v1 = map_expr env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_expr env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  R.Tuple [v1; v2]

and map_asserts (env : env) ((v1, v2, v3) : CST.asserts) =
  let v1 =
    (match v1 with
    | `Assert tok -> R.Case ("Assert",
        (* "assert" *) token env tok
      )
    | `Assume tok -> R.Case ("Assume",
        (* "assume" *) token env tok
      )
    | `Ensures tok -> R.Case ("Ensures",
        (* "ensures" *) token env tok
      )
    | `Requis tok -> R.Case ("Requis",
        (* "requires" *) token env tok
      )
    )
  in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_condition_props env x
      ))
    | None -> R.Option None)
  in
  let v3 = map_expr env v3 in
  R.Tuple [v1; v2; v3]

and map_assignment (env : env) ((v1, v2, v3) : CST.assignment) =
  let v1 = map_unary_expr env v1 in
  let v2 = (* "=" *) token env v2 in
  let v3 = map_expr env v3 in
  R.Tuple [v1; v2; v3]

and map_bin_op_expr (env : env) (x : CST.bin_op_expr) =
  (match x with
  | `Op_expr_EQEQGT_op_expr (v1, v2, v3) -> R.Case ("Op_expr_EQEQGT_op_expr",
      let v1 = map_op_expr env v1 in
      let v2 = (* "==>" *) token env v2 in
      let v3 = map_op_expr env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Op_expr_LTEQEQGT_op_expr (v1, v2, v3) -> R.Case ("Op_expr_LTEQEQGT_op_expr",
      let v1 = map_op_expr env v1 in
      let v2 = (* "<==>" *) token env v2 in
      let v3 = map_op_expr env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Op_expr_BARBAR_op_expr (v1, v2, v3) -> R.Case ("Op_expr_BARBAR_op_expr",
      let v1 = map_op_expr env v1 in
      let v2 = (* "||" *) token env v2 in
      let v3 = map_op_expr env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Op_expr_AMPAMP_op_expr (v1, v2, v3) -> R.Case ("Op_expr_AMPAMP_op_expr",
      let v1 = map_op_expr env v1 in
      let v2 = (* "&&" *) token env v2 in
      let v3 = map_op_expr env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Op_expr_EQEQ_op_expr (v1, v2, v3) -> R.Case ("Op_expr_EQEQ_op_expr",
      let v1 = map_op_expr env v1 in
      let v2 = (* "==" *) token env v2 in
      let v3 = map_op_expr env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Op_expr_BANGEQ_op_expr (v1, v2, v3) -> R.Case ("Op_expr_BANGEQ_op_expr",
      let v1 = map_op_expr env v1 in
      let v2 = (* "!=" *) token env v2 in
      let v3 = map_op_expr env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Op_expr_LT_op_expr (v1, v2, v3) -> R.Case ("Op_expr_LT_op_expr",
      let v1 = map_op_expr env v1 in
      let v2 = (* "<" *) token env v2 in
      let v3 = map_op_expr env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Op_expr_GT_op_expr (v1, v2, v3) -> R.Case ("Op_expr_GT_op_expr",
      let v1 = map_op_expr env v1 in
      let v2 = (* ">" *) token env v2 in
      let v3 = map_op_expr env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Op_expr_LTEQ_op_expr (v1, v2, v3) -> R.Case ("Op_expr_LTEQ_op_expr",
      let v1 = map_op_expr env v1 in
      let v2 = (* "<=" *) token env v2 in
      let v3 = map_op_expr env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Op_expr_GTEQ_op_expr (v1, v2, v3) -> R.Case ("Op_expr_GTEQ_op_expr",
      let v1 = map_op_expr env v1 in
      let v2 = (* ">=" *) token env v2 in
      let v3 = map_op_expr env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Op_expr_DOTDOT_op_expr (v1, v2, v3) -> R.Case ("Op_expr_DOTDOT_op_expr",
      let v1 = map_op_expr env v1 in
      let v2 = (* ".." *) token env v2 in
      let v3 = map_op_expr env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Op_expr_BAR_op_expr (v1, v2, v3) -> R.Case ("Op_expr_BAR_op_expr",
      let v1 = map_op_expr env v1 in
      let v2 = (* "|" *) token env v2 in
      let v3 = map_op_expr env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Op_expr_HAT_op_expr (v1, v2, v3) -> R.Case ("Op_expr_HAT_op_expr",
      let v1 = map_op_expr env v1 in
      let v2 = (* "^" *) token env v2 in
      let v3 = map_op_expr env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Op_expr_AMP_op_expr (v1, v2, v3) -> R.Case ("Op_expr_AMP_op_expr",
      let v1 = map_op_expr env v1 in
      let v2 = (* "&" *) token env v2 in
      let v3 = map_op_expr env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Op_expr_LTLT_op_expr (v1, v2, v3) -> R.Case ("Op_expr_LTLT_op_expr",
      let v1 = map_op_expr env v1 in
      let v2 = (* "<<" *) token env v2 in
      let v3 = map_op_expr env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Op_expr_GTGT_op_expr (v1, v2, v3) -> R.Case ("Op_expr_GTGT_op_expr",
      let v1 = map_op_expr env v1 in
      let v2 = (* ">>" *) token env v2 in
      let v3 = map_op_expr env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Op_expr_PLUS_op_expr (v1, v2, v3) -> R.Case ("Op_expr_PLUS_op_expr",
      let v1 = map_op_expr env v1 in
      let v2 = (* "+" *) token env v2 in
      let v3 = map_op_expr env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Op_expr_DASH_op_expr (v1, v2, v3) -> R.Case ("Op_expr_DASH_op_expr",
      let v1 = map_op_expr env v1 in
      let v2 = (* "-" *) token env v2 in
      let v3 = map_op_expr env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Op_expr_STAR_op_expr (v1, v2, v3) -> R.Case ("Op_expr_STAR_op_expr",
      let v1 = map_op_expr env v1 in
      let v2 = (* "*" *) token env v2 in
      let v3 = map_op_expr env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Op_expr_SLASH_op_expr (v1, v2, v3) -> R.Case ("Op_expr_SLASH_op_expr",
      let v1 = map_op_expr env v1 in
      let v2 = (* "/" *) token env v2 in
      let v3 = map_op_expr env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Op_expr_PERC_op_expr (v1, v2, v3) -> R.Case ("Op_expr_PERC_op_expr",
      let v1 = map_op_expr env v1 in
      let v2 = (* "%" *) token env v2 in
      let v3 = map_op_expr env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_block (env : env) ((v1, v2, v3, v4, v5) : CST.block) =
  let v1 = (* "{" *) token env v1 in
  let v2 = R.List (List.map (map_use_decl env) v2) in
  let v3 = R.List (List.map (map_sequence_item env) v3) in
  let v4 =
    (match v4 with
    | Some x -> R.Option (Some (
        map_expr env x
      ))
    | None -> R.Option None)
  in
  let v5 = (* "}" *) token env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

and map_call_args (env : env) ((v1, v2, v3, v4) : CST.call_args) =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_anon_expr_rep_COMMA_expr_8e432c6 env x
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
  let v4 = (* ")" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_control_body (env : env) (x : CST.control_body) =
  (match x with
  | `Blk x -> R.Case ("Blk",
      map_block env x
    )
  | `Expr x -> R.Case ("Expr",
      map_expr env x
    )
  )

and map_deep_ellipsis (env : env) ((v1, v2, v3) : CST.deep_ellipsis) =
  let v1 = (* "<..." *) token env v1 in
  let v2 = map_expr env v2 in
  let v3 = (* "...>" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_dot_or_index_chain (env : env) (x : CST.dot_or_index_chain) =
  (match x with
  | `Access_field (v1, v2, v3) -> R.Case ("Access_field",
      let v1 = map_dot_or_index_chain env v1 in
      let v2 = (* "." *) token env v2 in
      let v3 = map_identifier_or_anon_field env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Rece_call (v1, v2, v3, v4, v5) -> R.Case ("Rece_call",
      let v1 = map_dot_or_index_chain env v1 in
      let v2 = (* "." *) token env v2 in
      let v3 = map_identifier_or_anon_field env v3 in
      let v4 =
        (match v4 with
        | Some (v1, v2) -> R.Option (Some (
            let v1 = (* "::" *) token env v1 in
            let v2 = map_type_args env v2 in
            R.Tuple [v1; v2]
          ))
        | None -> R.Option None)
      in
      let v5 = map_call_args env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `Mem_access (v1, v2, v3, v4) -> R.Case ("Mem_access",
      let v1 = map_dot_or_index_chain env v1 in
      let v2 = (* "[" *) token env v2 in
      let v3 = map_expr env v3 in
      let v4 = (* "]" *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Term x -> R.Case ("Term",
      map_term env x
    )
  | `Field_access_ellips_expr x -> R.Case ("Field_access_ellips_expr",
      map_field_access_ellipsis_expr env x
    )
  )

and map_emits (env : env) ((v1, v2, v3, v4, v5, v6) : CST.emits) =
  let v1 = (* "emits" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_condition_props env x
      ))
    | None -> R.Option None)
  in
  let v3 = map_expr env v3 in
  let v4 = (* "to" *) token env v4 in
  let v5 = map_expr env v5 in
  let v6 =
    (match v6 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = (* "if" *) token env v1 in
        let v2 = map_expr env v2 in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3; v4; v5; v6]

and map_expr (env : env) (x : CST.expr) =
  (match x with
  | `Assign x -> R.Case ("Assign",
      map_assignment env x
    )
  | `Op_expr x -> R.Case ("Op_expr",
      map_op_expr env x
    )
  | `Quan x -> R.Case ("Quan",
      map_quantifier env x
    )
  | `Lambda_bind_list_expr (v1, v2) -> R.Case ("Lambda_bind_list_expr",
      let v1 = map_lambda_bind_list env v1 in
      let v2 = map_expr env v2 in
      R.Tuple [v1; v2]
    )
  | `Ellips tok -> R.Case ("Ellips",
      (* "..." *) token env tok
    )
  | `Deep_ellips x -> R.Case ("Deep_ellips",
      map_deep_ellipsis env x
    )
  )

and map_expr_field (env : env) (x : CST.expr_field) =
  (match x with
  | `Id tok -> R.Case ("Id",
      (* identifier *) token env tok
    )
  | `Id_COLON_expr (v1, v2, v3) -> R.Case ("Id_COLON_expr",
      let v1 = (* identifier *) token env v1 in
      let v2 = (* ":" *) token env v2 in
      let v3 = map_expr env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Ellips tok -> R.Case ("Ellips",
      (* "..." *) token env tok
    )
  )

and map_field_access_ellipsis_expr (env : env) ((v1, v2, v3) : CST.field_access_ellipsis_expr) =
  let v1 = map_dot_or_index_chain env v1 in
  let v2 = (* "." *) token env v2 in
  let v3 = (* "..." *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_for_loop_expr (env : env) (x : CST.for_loop_expr) =
  (match x with
  | `For_LPAR_var_name_in_un_expr_DOTDOT_un_expr_opt_spec_loop_inva_RPAR_blk (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10) -> R.Case ("For_LPAR_var_name_in_un_expr_DOTDOT_un_expr_opt_spec_loop_inva_RPAR_blk",
      let v1 = (* "for" *) token env v1 in
      let v2 = (* "(" *) token env v2 in
      let v3 = map_var_name env v3 in
      let v4 = (* "in" *) token env v4 in
      let v5 = map_unary_expr env v5 in
      let v6 = (* ".." *) token env v6 in
      let v7 = map_unary_expr env v7 in
      let v8 =
        (match v8 with
        | Some x -> R.Option (Some (
            map_spec_loop_invariant env x
          ))
        | None -> R.Option None)
      in
      let v9 = (* ")" *) token env v9 in
      let v10 = map_block env v10 in
      R.Tuple [v1; v2; v3; v4; v5; v6; v7; v8; v9; v10]
    )
  | `For_LPAR_ellips_RPAR_blk (v1, v2, v3, v4, v5) -> R.Case ("For_LPAR_ellips_RPAR_blk",
      let v1 = (* "for" *) token env v1 in
      let v2 = (* "(" *) token env v2 in
      let v3 = (* "..." *) token env v3 in
      let v4 = (* ")" *) token env v4 in
      let v5 = map_block env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  )

and map_match_arm (env : env) (x : CST.match_arm) =
  (match x with
  | `Bind_list_opt_if_expr_EQGT_cont_body (v1, v2, v3, v4) -> R.Case ("Bind_list_opt_if_expr_EQGT_cont_body",
      let v1 = map_bind_list env v1 in
      let v2 =
        (match v2 with
        | Some (v1, v2) -> R.Option (Some (
            let v1 = (* "if" *) token env v1 in
            let v2 = map_expr env v2 in
            R.Tuple [v1; v2]
          ))
        | None -> R.Option None)
      in
      let v3 = (* "=>" *) token env v3 in
      let v4 = map_control_body env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Ellips tok -> R.Case ("Ellips",
      (* "..." *) token env tok
    )
  )

and map_name_expr (env : env) (x : CST.name_expr) =
  (match x with
  | `Var (v1, v2) -> R.Case ("Var",
      let v1 = map_name_access_chain env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_type_args env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2]
    )
  | `Call_expr (v1, v2, v3) -> R.Case ("Call_expr",
      let v1 = map_name_access_chain env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_type_args env x
          ))
        | None -> R.Option None)
      in
      let v3 = map_call_args env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Pack_expr (v1, v2, v3, v4, v5, v6) -> R.Case ("Pack_expr",
      let v1 = map_name_access_chain env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_type_args env x
          ))
        | None -> R.Option None)
      in
      let v3 = (* "{" *) token env v3 in
      let v4 =
        (match v4 with
        | Some (v1, v2) -> R.Option (Some (
            let v1 = map_expr_field env v1 in
            let v2 =
              R.List (List.map (fun (v1, v2) ->
                let v1 = (* "," *) token env v1 in
                let v2 = map_expr_field env v2 in
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
  | `Macro_call_expr (v1, v2, v3) -> R.Case ("Macro_call_expr",
      let v1 = map_name_access_chain env v1 in
      let v2 = (* "!" *) token env v2 in
      let v3 = map_call_args env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_op_expr (env : env) (x : CST.op_expr) =
  (match x with
  | `Un_expr x -> R.Case ("Un_expr",
      map_unary_expr env x
    )
  | `Bin_op_expr x -> R.Case ("Bin_op_expr",
      map_bin_op_expr env x
    )
  )

and map_parenthesized_expr (env : env) ((v1, v2, v3) : CST.parenthesized_expr) =
  let v1 = (* "(" *) token env v1 in
  let v2 = map_expr env v2 in
  let v3 = (* ")" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_quantifier (env : env) (x : CST.quantifier) =
  (match x with
  | `Choice_forall_quan_bind_rep_COMMA_quan_bind_opt_triggs_opt_where_expr_COLON_expr (v1, v2, v3, v4, v5, v6, v7) -> R.Case ("Choice_forall_quan_bind_rep_COMMA_quan_bind_opt_triggs_opt_where_expr_COLON_expr",
      let v1 =
        (match v1 with
        | `Forall tok -> R.Case ("Forall",
            (* "forall" *) token env tok
          )
        | `Exists tok -> R.Case ("Exists",
            (* "exists" *) token env tok
          )
        )
      in
      let v2 = map_quantifier_bind env v2 in
      let v3 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = (* "," *) token env v1 in
          let v2 = map_quantifier_bind env v2 in
          R.Tuple [v1; v2]
        ) v3)
      in
      let v4 =
        (match v4 with
        | Some x -> R.Option (Some (
            map_triggers env x
          ))
        | None -> R.Option None)
      in
      let v5 =
        (match v5 with
        | Some (v1, v2) -> R.Option (Some (
            let v1 = (* "where" *) token env v1 in
            let v2 = map_expr env v2 in
            R.Tuple [v1; v2]
          ))
        | None -> R.Option None)
      in
      let v6 = (* ":" *) token env v6 in
      let v7 = map_expr env v7 in
      R.Tuple [v1; v2; v3; v4; v5; v6; v7]
    )
  | `Choose_opt_min_quan_bind_where_expr (v1, v2, v3, v4, v5) -> R.Case ("Choose_opt_min_quan_bind_where_expr",
      let v1 = (* "choose" *) token env v1 in
      let v2 =
        (match v2 with
        | Some tok -> R.Option (Some (
            (* "min" *) token env tok
          ))
        | None -> R.Option None)
      in
      let v3 = map_quantifier_bind env v3 in
      let v4 = (* "where" *) token env v4 in
      let v5 = map_expr env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  )

and map_quantifier_bind (env : env) (x : CST.quantifier_bind) =
  (match x with
  | `Id_COLON_type_ (v1, v2, v3) -> R.Case ("Id_COLON_type_",
      let v1 = (* identifier *) token env v1 in
      let v2 = (* ":" *) token env v2 in
      let v3 = map_type__ env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Id_in_expr (v1, v2, v3) -> R.Case ("Id_in_expr",
      let v1 = (* identifier *) token env v1 in
      let v2 = (* "in" *) token env v2 in
      let v3 = map_expr env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_return_expr (env : env) (x : CST.return_expr) =
  (match x with
  | `Ret tok -> R.Case ("Ret",
      (* "return" *) token env tok
    )
  | `Ret_expr (v1, v2) -> R.Case ("Ret_expr",
      let v1 = (* "return" *) token env v1 in
      let v2 = map_expr env v2 in
      R.Tuple [v1; v2]
    )
  )

and map_semgrep_expression (env : env) (x : CST.semgrep_expression) =
  (match x with
  | `Expr x -> R.Case ("Expr",
      map_expr env x
    )
  | `Let_expr (v1, v2, v3, v4) -> R.Case ("Let_expr",
      let v1 = (* "let" *) token env v1 in
      let v2 = map_bind_list env v2 in
      let v3 =
        (match v3 with
        | Some (v1, v2) -> R.Option (Some (
            let v1 = (* ":" *) token env v1 in
            let v2 = map_type__ env v2 in
            R.Tuple [v1; v2]
          ))
        | None -> R.Option None)
      in
      let v4 =
        (match v4 with
        | Some (v1, v2) -> R.Option (Some (
            let v1 = (* "=" *) token env v1 in
            let v2 = map_expr env v2 in
            R.Tuple [v1; v2]
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3; v4]
    )
  )

and map_sequence_item (env : env) (x : CST.sequence_item) =
  (match x with
  | `Choice_expr_SEMI (v1, v2) -> R.Case ("Choice_expr_SEMI",
      let v1 = map_semgrep_expression env v1 in
      let v2 = (* ";" *) token env v2 in
      R.Tuple [v1; v2]
    )
  | `Ellips tok -> R.Case ("Ellips",
      (* "..." *) token env tok
    )
  )

and map_spec_apply (env : env) ((v1, v2, v3, v4, v5, v6, v7) : CST.spec_apply) =
  let v1 = (* "apply" *) token env v1 in
  let v2 = map_expr env v2 in
  let v3 = (* "to" *) token env v3 in
  let v4 =
    (match v4 with
    | Some x -> R.Option (Some (
        map_anon_spec_apply_pat_rep_COMMA_spec_apply_pat_d9a21d6 env x
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
  let v6 =
    (match v6 with
    | Some (v1, v2, v3) -> R.Option (Some (
        let v1 = (* "except" *) token env v1 in
        let v2 =
          (match v2 with
          | Some x -> R.Option (Some (
              map_anon_spec_apply_pat_rep_COMMA_spec_apply_pat_d9a21d6 env x
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
        R.Tuple [v1; v2; v3]
      ))
    | None -> R.Option None)
  in
  let v7 = (* ";" *) token env v7 in
  R.Tuple [v1; v2; v3; v4; v5; v6; v7]

and map_spec_axiom (env : env) ((v1, v2, v3, v4, v5) : CST.spec_axiom) =
  let v1 = (* "axiom" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_type_params env x
      ))
    | None -> R.Option None)
  in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_condition_props env x
      ))
    | None -> R.Option None)
  in
  let v4 = map_expr env v4 in
  let v5 = (* ";" *) token env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

and map_spec_block (env : env) ((v1, v2) : CST.spec_block) =
  let v1 = (* "spec" *) token env v1 in
  let v2 =
    (match v2 with
    | `Spec_func x -> R.Case ("Spec_func",
        map_spec_func env x
      )
    | `Opt_spec_blk_target_LCURL_rep_use_decl_rep_spec_blk_member_RCURL (v1, v2, v3, v4, v5) -> R.Case ("Opt_spec_blk_target_LCURL_rep_use_decl_rep_spec_blk_member_RCURL",
        let v1 =
          (match v1 with
          | Some x -> R.Option (Some (
              map_spec_block_target env x
            ))
          | None -> R.Option None)
        in
        let v2 = (* "{" *) token env v2 in
        let v3 = R.List (List.map (map_use_decl env) v3) in
        let v4 = R.List (List.map (map_spec_block_member env) v4) in
        let v5 = (* "}" *) token env v5 in
        R.Tuple [v1; v2; v3; v4; v5]
      )
    )
  in
  R.Tuple [v1; v2]

and map_spec_block_member (env : env) (x : CST.spec_block_member) =
  (match x with
  | `Choice_spec_inva x -> R.Case ("Choice_spec_inva",
      (match x with
      | `Spec_inva x -> R.Case ("Spec_inva",
          map_spec_invariant env x
        )
      | `Spec_cond x -> R.Case ("Spec_cond",
          map_spec_condition env x
        )
      | `Spec_func x -> R.Case ("Spec_func",
          map_spec_func env x
        )
      | `Spec_var x -> R.Case ("Spec_var",
          map_spec_variable env x
        )
      | `Spec_incl x -> R.Case ("Spec_incl",
          map_spec_include env x
        )
      | `Spec_apply x -> R.Case ("Spec_apply",
          map_spec_apply env x
        )
      | `Spec_pragma x -> R.Case ("Spec_pragma",
          map_spec_pragma env x
        )
      | `Spec_let x -> R.Case ("Spec_let",
          map_spec_let env x
        )
      | `Spec_update x -> R.Case ("Spec_update",
          map_spec_update env x
        )
      | `Spec_axiom x -> R.Case ("Spec_axiom",
          map_spec_axiom env x
        )
      )
    )
  | `Ellips tok -> R.Case ("Ellips",
      (* "..." *) token env tok
    )
  )

and map_spec_condition (env : env) ((v1, v2) : CST.spec_condition) =
  let v1 =
    (match v1 with
    | `Asserts x -> R.Case ("Asserts",
        map_asserts env x
      )
    | `Aborts_if x -> R.Case ("Aborts_if",
        map_aborts_if env x
      )
    | `Aborts_with_or_modifs x -> R.Case ("Aborts_with_or_modifs",
        map_aborts_with_or_modifies env x
      )
    | `Emits x -> R.Case ("Emits",
        map_emits env x
      )
    )
  in
  let v2 = (* ";" *) token env v2 in
  R.Tuple [v1; v2]

and map_spec_func (env : env) (x : CST.spec_func) =
  (match x with
  | `Fun_spec_func_signas_choice_blk (v1, v2, v3) -> R.Case ("Fun_spec_func_signas_choice_blk",
      let v1 = (* "fun" *) token env v1 in
      let v2 = map_spec_func_signatures env v2 in
      let v3 = map_anon_choice_blk_f78fea4 env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Native_fun_spec_func_signas_SEMI (v1, v2, v3, v4) -> R.Case ("Native_fun_spec_func_signas_SEMI",
      let v1 = (* "native" *) token env v1 in
      let v2 = (* "fun" *) token env v2 in
      let v3 = map_spec_func_signatures env v3 in
      let v4 = (* ";" *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  )

and map_spec_include (env : env) ((v1, v2, v3, v4) : CST.spec_include) =
  let v1 = (* "include" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_condition_props env x
      ))
    | None -> R.Option None)
  in
  let v3 = map_expr env v3 in
  let v4 = (* ";" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_spec_invariant (env : env) ((v1, v2, v3, v4, v5, v6) : CST.spec_invariant) =
  let v1 = (* "invariant" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_type_params env x
      ))
    | None -> R.Option None)
  in
  let v3 =
    (match v3 with
    | Some tok -> R.Option (Some (
        (* "update" *) token env tok
      ))
    | None -> R.Option None)
  in
  let v4 =
    (match v4 with
    | Some x -> R.Option (Some (
        map_condition_props env x
      ))
    | None -> R.Option None)
  in
  let v5 = map_expr env v5 in
  let v6 = (* ";" *) token env v6 in
  R.Tuple [v1; v2; v3; v4; v5; v6]

and map_spec_let (env : env) ((v1, v2, v3, v4, v5, v6) : CST.spec_let) =
  let v1 = (* "let" *) token env v1 in
  let v2 =
    (match v2 with
    | Some tok -> R.Option (Some (
        (* "post" *) token env tok
      ))
    | None -> R.Option None)
  in
  let v3 = map_var_name env v3 in
  let v4 = (* "=" *) token env v4 in
  let v5 = map_expr env v5 in
  let v6 = (* ";" *) token env v6 in
  R.Tuple [v1; v2; v3; v4; v5; v6]

and map_spec_loop_invariant (env : env) (x : CST.spec_loop_invariant) =
  map_spec_block env x

and map_spec_update (env : env) ((v1, v2, v3) : CST.spec_update) =
  let v1 = (* "update" *) token env v1 in
  let v2 = map_assignment env v2 in
  let v3 = (* ";" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_spec_variable (env : env) ((v1, v2, v3, v4, v5, v6, v7) : CST.spec_variable) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        (match x with
        | `Global tok -> R.Case ("Global",
            (* "global" *) token env tok
          )
        | `Local tok -> R.Case ("Local",
            (* "local" *) token env tok
          )
        )
      ))
    | None -> R.Option None)
  in
  let v2 = (* identifier *) token env v2 in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_type_params env x
      ))
    | None -> R.Option None)
  in
  let v4 = (* ":" *) token env v4 in
  let v5 = map_type__ env v5 in
  let v6 =
    (match v6 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = (* "=" *) token env v1 in
        let v2 = map_expr env v2 in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  let v7 = (* ";" *) token env v7 in
  R.Tuple [v1; v2; v3; v4; v5; v6; v7]

and map_term (env : env) (x : CST.term) =
  (match x with
  | `Brk tok -> R.Case ("Brk",
      (* "break" *) token env tok
    )
  | `Cont tok -> R.Case ("Cont",
      (* "continue" *) token env tok
    )
  | `Vec_value_expr (v1, v2, v3, v4, v5, v6) -> R.Case ("Vec_value_expr",
      let v1 = (* "vector" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_type_args env x
          ))
        | None -> R.Option None)
      in
      let v3 = (* "[" *) token env v3 in
      let v4 =
        (match v4 with
        | Some x -> R.Option (Some (
            map_anon_expr_rep_COMMA_expr_8e432c6 env x
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
      let v6 = (* "]" *) token env v6 in
      R.Tuple [v1; v2; v3; v4; v5; v6]
    )
  | `Value x -> R.Case ("Value",
      map_value env x
    )
  | `Tuple_expr (v1, v2, v3, v4) -> R.Case ("Tuple_expr",
      let v1 = (* "(" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_anon_expr_rep_COMMA_expr_8e432c6 env x
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
      let v4 = (* ")" *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Type_hint_expr (v1, v2, v3, v4, v5) -> R.Case ("Type_hint_expr",
      let v1 = (* "(" *) token env v1 in
      let v2 = map_expr env v2 in
      let v3 = (* ":" *) token env v3 in
      let v4 = map_type__ env v4 in
      let v5 = (* ")" *) token env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `Cast_expr (v1, v2, v3, v4, v5) -> R.Case ("Cast_expr",
      let v1 = (* "(" *) token env v1 in
      let v2 = map_expr env v2 in
      let v3 = (* "as" *) token env v3 in
      let v4 = map_type__ env v4 in
      let v5 = (* ")" *) token env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `Blk x -> R.Case ("Blk",
      map_block env x
    )
  | `Choice_var x -> R.Case ("Choice_var",
      map_name_expr env x
    )
  | `Spec_blk x -> R.Case ("Spec_blk",
      map_spec_loop_invariant env x
    )
  | `If_expr (v1, v2, v3, v4) -> R.Case ("If_expr",
      let v1 = (* "if" *) token env v1 in
      let v2 = map_parenthesized_expr env v2 in
      let v3 = map_control_body env v3 in
      let v4 =
        (match v4 with
        | Some (v1, v2) -> R.Option (Some (
            let v1 = (* "else" *) token env v1 in
            let v2 = map_control_body env v2 in
            R.Tuple [v1; v2]
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Match_expr (v1, v2, v3, v4, v5, v6, v7) -> R.Case ("Match_expr",
      let v1 = (* "match" *) token env v1 in
      let v2 = (* "(" *) token env v2 in
      let v3 = map_expr env v3 in
      let v4 = (* ")" *) token env v4 in
      let v5 = (* "{" *) token env v5 in
      let v6 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = map_match_arm env v1 in
          let v2 =
            (match v2 with
            | Some tok -> R.Option (Some (
                (* "," *) token env tok
              ))
            | None -> R.Option None)
          in
          R.Tuple [v1; v2]
        ) v6)
      in
      let v7 = (* "}" *) token env v7 in
      R.Tuple [v1; v2; v3; v4; v5; v6; v7]
    )
  | `While_expr (v1, v2, v3, v4) -> R.Case ("While_expr",
      let v1 = (* "while" *) token env v1 in
      let v2 = map_parenthesized_expr env v2 in
      let v3 = map_control_body env v3 in
      let v4 =
        (match v4 with
        | Some x -> R.Option (Some (
            map_spec_loop_invariant env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Loop_expr (v1, v2) -> R.Case ("Loop_expr",
      let v1 = (* "loop" *) token env v1 in
      let v2 = map_control_body env v2 in
      R.Tuple [v1; v2]
    )
  | `Ret_expr x -> R.Case ("Ret_expr",
      map_return_expr env x
    )
  | `Abort_expr (v1, v2) -> R.Case ("Abort_expr",
      let v1 = (* "abort" *) token env v1 in
      let v2 = map_expr env v2 in
      R.Tuple [v1; v2]
    )
  | `For_loop_expr x -> R.Case ("For_loop_expr",
      map_for_loop_expr env x
    )
  | `Ellips tok -> R.Case ("Ellips",
      (* "..." *) token env tok
    )
  | `Deep_ellips x -> R.Case ("Deep_ellips",
      map_deep_ellipsis env x
    )
  )

and map_triggers (env : env) (xs : CST.triggers) =
  R.List (List.map (fun (v1, v2, v3, v4) ->
    let v1 = (* "{" *) token env v1 in
    let v2 =
      (match v2 with
      | Some x -> R.Option (Some (
          map_anon_expr_rep_COMMA_expr_8e432c6 env x
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
  ) xs)

and map_unary_expr (env : env) (x : CST.unary_expr) =
  (match x with
  | `Not_expr (v1, v2) -> R.Case ("Not_expr",
      let v1 = (* "!" *) token env v1 in
      let v2 = map_unary_expr env v2 in
      R.Tuple [v1; v2]
    )
  | `Ref_expr (v1, v2) -> R.Case ("Ref_expr",
      let v1 = (* "&" *) token env v1 in
      let v2 = map_unary_expr env v2 in
      R.Tuple [v1; v2]
    )
  | `Ref_mut_expr (v1, v2) -> R.Case ("Ref_mut_expr",
      let v1 = (* "&mut" *) token env v1 in
      let v2 = map_unary_expr env v2 in
      R.Tuple [v1; v2]
    )
  | `Deref_expr (v1, v2) -> R.Case ("Deref_expr",
      let v1 = (* "*" *) token env v1 in
      let v2 = map_unary_expr env v2 in
      R.Tuple [v1; v2]
    )
  | `Move_expr (v1, v2) -> R.Case ("Move_expr",
      let v1 = (* "move" *) token env v1 in
      let v2 = (* identifier *) token env v2 in
      R.Tuple [v1; v2]
    )
  | `Copy_expr (v1, v2) -> R.Case ("Copy_expr",
      let v1 = (* "copy" *) token env v1 in
      let v2 = (* identifier *) token env v2 in
      R.Tuple [v1; v2]
    )
  | `Choice_access_field x -> R.Case ("Choice_access_field",
      map_dot_or_index_chain env x
    )
  | `Ellips tok -> R.Case ("Ellips",
      (* "..." *) token env tok
    )
  | `Deep_ellips x -> R.Case ("Deep_ellips",
      map_deep_ellipsis env x
    )
  | `Field_access_ellips_expr x -> R.Case ("Field_access_ellips_expr",
      map_field_access_ellipsis_expr env x
    )
  | `Typed_meta (v1, v2, v3, v4, v5) -> R.Case ("Typed_meta",
      let v1 = (* "(" *) token env v1 in
      let v2 = (* identifier *) token env v2 in
      let v3 = (* ":" *) token env v3 in
      let v4 = map_type__ env v4 in
      let v5 = (* ")" *) token env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  )

let map_enum_body (env : env) ((v1, v2, v3, v4) : CST.enum_body) =
  let v1 = (* "{" *) token env v1 in
  let v2 = R.List (List.map (map_variant env) v2) in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_variant_last env x
      ))
    | None -> R.Option None)
  in
  let v4 = (* "}" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

let map_function_signature (env : env) ((v1, v2, v3, v4, v5, v6, v7) : CST.function_signature) =
  let v1 =
    (match v1 with
    | Some tok -> R.Option (Some (
        (* "inline" *) token env tok
      ))
    | None -> R.Option None)
  in
  let v2 = (* "fun" *) token env v2 in
  let v3 = (* identifier *) token env v3 in
  let v4 =
    (match v4 with
    | Some x -> R.Option (Some (
        map_type_params env x
      ))
    | None -> R.Option None)
  in
  let v5 = map_parameters env v5 in
  let v6 =
    (match v6 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = (* ":" *) token env v1 in
        let v2 = map_type__ env v2 in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  let v7 =
    (match v7 with
    | Some x -> R.Option (Some (
        map_specifier env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3; v4; v5; v6; v7]

let map_constant_decl (env : env) ((v1, v2, v3, v4, v5, v6, v7) : CST.constant_decl) =
  let v1 = (* "const" *) token env v1 in
  let v2 = (* identifier *) token env v2 in
  let v3 = (* ":" *) token env v3 in
  let v4 = map_type__ env v4 in
  let v5 = (* "=" *) token env v5 in
  let v6 = map_expr env v6 in
  let v7 = (* ";" *) token env v7 in
  R.Tuple [v1; v2; v3; v4; v5; v6; v7]

let map_script_spec_block (env : env) (x : CST.script_spec_block) =
  (match x with
  | `Opt_attris_spec_blk (v1, v2) -> R.Case ("Opt_attris_spec_blk",
      let v1 =
        (match v1 with
        | Some x -> R.Option (Some (
            map_attributes env x
          ))
        | None -> R.Option None)
      in
      let v2 = map_spec_loop_invariant env v2 in
      R.Tuple [v1; v2]
    )
  | `Ellips tok -> R.Case ("Ellips",
      (* "..." *) token env tok
    )
  )

let map_enum_decl (env : env) (x : CST.enum_decl) =
  (match x with
  | `Enum_sign_enum_body (v1, v2) -> R.Case ("Enum_sign_enum_body",
      let v1 = map_enum_signature env v1 in
      let v2 = map_enum_body env v2 in
      R.Tuple [v1; v2]
    )
  | `Enum_struct_def_name_enum_body_opt_abilis_SEMI (v1, v2, v3, v4, v5) -> R.Case ("Enum_struct_def_name_enum_body_opt_abilis_SEMI",
      let v1 = (* "enum" *) token env v1 in
      let v2 = map_struct_def_name env v2 in
      let v3 = map_enum_body env v3 in
      let v4 =
        (match v4 with
        | Some x -> R.Option (Some (
            map_abilities env x
          ))
        | None -> R.Option None)
      in
      let v5 = (* ";" *) token env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  )

let map_function_decl (env : env) ((v1, v2) : CST.function_decl) =
  let v1 = map_function_signature env v1 in
  let v2 = map_anon_choice_blk_f78fea4 env v2 in
  R.Tuple [v1; v2]

let map_script_constant_decl (env : env) (x : CST.script_constant_decl) =
  (match x with
  | `Opt_attris_cst_decl (v1, v2) -> R.Case ("Opt_attris_cst_decl",
      let v1 =
        (match v1 with
        | Some x -> R.Option (Some (
            map_attributes env x
          ))
        | None -> R.Option None)
      in
      let v2 = map_constant_decl env v2 in
      R.Tuple [v1; v2]
    )
  | `Ellips tok -> R.Case ("Ellips",
      (* "..." *) token env tok
    )
  )

let map_struct_or_enum_decl (env : env) (x : CST.struct_or_enum_decl) =
  (match x with
  | `Struct_decl x -> R.Case ("Struct_decl",
      map_struct_decl env x
    )
  | `Enum_decl x -> R.Case ("Enum_decl",
      map_enum_decl env x
    )
  )

let map_script_func_decl (env : env) (x : CST.script_func_decl) =
  (match x with
  | `Opt_attris_rep_module_member_modi_func_decl (v1, v2, v3) -> R.Case ("Opt_attris_rep_module_member_modi_func_decl",
      let v1 =
        (match v1 with
        | Some x -> R.Option (Some (
            map_attributes env x
          ))
        | None -> R.Option None)
      in
      let v2 =
        R.List (List.map (map_module_member_modifier env) v2)
      in
      let v3 = map_function_decl env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Ellips tok -> R.Case ("Ellips",
      (* "..." *) token env tok
    )
  )

let map_declaration (env : env) (x : CST.declaration) =
  (match x with
  | `Opt_attris_choice_use_decl (v1, v2) -> R.Case ("Opt_attris_choice_use_decl",
      let v1 =
        (match v1 with
        | Some x -> R.Option (Some (
            map_attributes env x
          ))
        | None -> R.Option None)
      in
      let v2 =
        (match v2 with
        | `Use_decl x -> R.Case ("Use_decl",
            map_use_decl env x
          )
        | `Friend_decl x -> R.Case ("Friend_decl",
            map_friend_decl env x
          )
        | `Spec_spec_func (v1, v2) -> R.Case ("Spec_spec_func",
            let v1 = (* "spec" *) token env v1 in
            let v2 = map_spec_func env v2 in
            R.Tuple [v1; v2]
          )
        | `Spec_blk x -> R.Case ("Spec_blk",
            map_spec_loop_invariant env x
          )
        | `Spec_inva x -> R.Case ("Spec_inva",
            map_spec_invariant env x
          )
        | `Rep_module_member_modi_choice_cst_decl (v1, v2) -> R.Case ("Rep_module_member_modi_choice_cst_decl",
            let v1 =
              R.List (List.map (map_module_member_modifier env) v1)
            in
            let v2 =
              (match v2 with
              | `Cst_decl x -> R.Case ("Cst_decl",
                  map_constant_decl env x
                )
              | `Struct_or_enum_decl x -> R.Case ("Struct_or_enum_decl",
                  map_struct_or_enum_decl env x
                )
              | `Func_decl x -> R.Case ("Func_decl",
                  map_function_decl env x
                )
              )
            in
            R.Tuple [v1; v2]
          )
        )
      in
      R.Tuple [v1; v2]
    )
  | `Ellips tok -> R.Case ("Ellips",
      (* "..." *) token env tok
    )
  )

let map_script (env : env) ((v1, v2, v3, v4, v5, v6, v7) : CST.script) =
  let v1 = (* "script" *) token env v1 in
  let v2 = (* "{" *) token env v2 in
  let v3 = R.List (List.map (map_script_use_decl env) v3) in
  let v4 =
    R.List (List.map (map_script_constant_decl env) v4)
  in
  let v5 = map_script_func_decl env v5 in
  let v6 = R.List (List.map (map_script_spec_block env) v6) in
  let v7 = (* "}" *) token env v7 in
  R.Tuple [v1; v2; v3; v4; v5; v6; v7]

let map_module_ (env : env) ((v1, v2, v3, v4, v5) : CST.module_) =
  let v1 =
    (match v1 with
    | `Spec tok -> R.Case ("Spec",
        (* "spec" *) token env tok
      )
    | `Module_kw tok -> R.Case ("Module_kw",
        (* "module" *) token env tok
      )
    )
  in
  let v2 = map_module_path env v2 in
  let v3 = (* "{" *) token env v3 in
  let v4 = R.List (List.map (map_declaration env) v4) in
  let v5 = (* "}" *) token env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

let map_semgrep_statement (env : env) (xs : CST.semgrep_statement) =
  R.List (List.map (fun x ->
    (match x with
    | `Seq_item x -> R.Case ("Seq_item",
        map_sequence_item env x
      )
    | `Decl x -> R.Case ("Decl",
        map_declaration env x
      )
    )
  ) xs)

let map_address_member (env : env) (x : CST.address_member) =
  (match x with
  | `Opt_attris_module (v1, v2) -> R.Case ("Opt_attris_module",
      let v1 =
        (match v1 with
        | Some x -> R.Option (Some (
            map_attributes env x
          ))
        | None -> R.Option None)
      in
      let v2 = map_module_ env v2 in
      R.Tuple [v1; v2]
    )
  | `Ellips tok -> R.Case ("Ellips",
      (* "..." *) token env tok
    )
  )

let map_address_block (env : env) ((v1, v2, v3, v4, v5) : CST.address_block) =
  let v1 = (* "address" *) token env v1 in
  let v2 = map_leading_name_access env v2 in
  let v3 = (* "{" *) token env v3 in
  let v4 = R.List (List.map (map_address_member env) v4) in
  let v5 = (* "}" *) token env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

let map_source_file (env : env) (x : CST.source_file) =
  (match x with
  | `Rep_opt_attris_choice_module xs -> R.Case ("Rep_opt_attris_choice_module",
      R.List (List.map (fun (v1, v2) ->
        let v1 =
          (match v1 with
          | Some x -> R.Option (Some (
              map_attributes env x
            ))
          | None -> R.Option None)
        in
        let v2 =
          (match v2 with
          | `Module x -> R.Case ("Module",
              map_module_ env x
            )
          | `Script x -> R.Case ("Script",
              map_script env x
            )
          | `Addr_blk x -> R.Case ("Addr_blk",
              map_address_block env x
            )
          )
        in
        R.Tuple [v1; v2]
      ) xs)
    )
  | `Semg_exp x -> R.Case ("Semg_exp",
      map_semgrep_expression env x
    )
  | `Semg_stmt x -> R.Case ("Semg_stmt",
      map_semgrep_statement env x
    )
  | `Semg_part (v1, v2, v3) -> R.Case ("Semg_part",
      let v1 =
        (match v1 with
        | Some x -> R.Option (Some (
            map_attributes env x
          ))
        | None -> R.Option None)
      in
      let v2 =
        R.List (List.map (map_module_member_modifier env) v2)
      in
      let v3 =
        (match v3 with
        | `Func_sign x -> R.Case ("Func_sign",
            map_function_signature env x
          )
        | `Struct_sign x -> R.Case ("Struct_sign",
            map_struct_signature env x
          )
        | `Enum_sign x -> R.Case ("Enum_sign",
            map_enum_signature env x
          )
        )
      in
      R.Tuple [v1; v2; v3]
    )
  )

let dump_tree root =
  map_source_file () root
  |> Tree_sitter_run.Raw_tree.to_channel stdout

let map_extra (env : env) (x : CST.extra) =
  match x with
  | `Line_comment (_loc, x) -> ("line_comment", "line_comment", map_line_comment env x)
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
