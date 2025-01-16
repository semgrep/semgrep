open Fpath_.Operators
module CST = Tree_sitter_move_on_aptos.CST
module H = Parse_tree_sitter_helpers
open AST_generic
module G = AST_generic
module H2 = AST_generic_helpers

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* Disable warnings against unused variables *)
[@@@warning "-26-27-32"]

(* Disable warning against unused 'rec' *)
[@@@warning "-39"]

type mode = Pattern | Target
type env = mode H.env

let token = H.token
let str = H.str
let fb = Tok.unsafe_fake_bracket

(* let fake s = Tok.unsafe_fake_tok s *)
let fake_id s = (s, G.fake s)

let in_pattern env =
  match env.H.extra with
  | Target -> false
  | Pattern -> true

(*****************************************************************************)
(* Boilerplate converter *)
(*****************************************************************************)
(* This was started by copying tree-sitter-lang/semgrep-move-on-aptos/Boilerplate.ml *)

(**
   Boilerplate to be used as a template when mapping the move_on_aptos CST
   to another type of tree.
*)

let remove_char str ch = String.split_on_char ch str |> String.concat ""

(* let map_block_comment_content (env : env) (tok : CST.block_comment_content) =
   (* block_comment_content *) token env tok *)

(* let map_doc_line_comment (env : env) (tok : CST.doc_line_comment) =
   (* doc_line_comment *) token env tok *)

(* let map_pat_4fd4a56 (env : env) (tok : CST.pat_4fd4a56) =
   (* pattern .* *) token env tok *)

let map_number_type (env : env) (x : CST.number_type) : G.ident =
  match x with
  | `U8 tok -> (* "u8" *) str env tok
  | `U16 tok -> (* "u16" *) str env tok
  | `U32 tok -> (* "u32" *) str env tok
  | `U64 tok -> (* "u64" *) str env tok
  | `U128 tok -> str env tok
  | `U256 tok -> str env tok

let map_reuseable_keywords (env : env) (x : CST.reuseable_keywords) : G.ident =
  match x with
  | `For tok -> (* "for" *) str env tok
  | `While tok -> (* "while" *) str env tok
  | `Friend tok -> (* "friend" *) str env tok
  | `Match tok -> (* "match" *) str env tok

let map_ability (env : env) (x : CST.ability) : G.type_ =
  match x with
  | `Choice_copy x ->
      let cap =
        match x with
        | `Copy tok -> (* "copy" *) str env tok
        | `Drop tok -> (* "drop" *) str env tok
        | `Store tok -> (* "store" *) str env tok
        | `Key tok -> (* "key" *) str env tok
      in
      G.ty_builtin cap
  | `Ellips tok -> (* "..." *) G.TyEllipsis (token env tok) |> G.t

let map_bool_literal (env : env) (x : CST.bool_literal) : G.literal =
  match x with
  | `True tok -> (* "true" *) G.Bool (true, token env tok)
  | `False tok -> (* "false" *) G.Bool (false, token env tok)

let map_quantifier_directive (env : env) (x : CST.quantifier_directive) =
  match x with
  | `Exists tok -> (* "exists" *) str env tok
  | `Forall tok -> (* "forall" *) str env tok
  | `Choose tok -> (* "choose" *) str env tok
  | `Min tok -> (* "min" *) str env tok

let map_deci_num (env : env) (tok : CST.pat_57a456d) =
  let literal, tok = (* pattern \d[\d_]* *) str env tok in
  (remove_char literal '_', tok)

(* let map_comment_string (env : env) (tok : CST.imm_tok_prec_p1_pat_4fd4a56) =
   (* pattern .* *) token env tok *)

let map_oct_num (env : env) (tok : CST.pat_9bd0c11) =
  let literal, tok = (* pattern [0-7][0-7_]* *) str env tok in
  (remove_char literal '_', tok)

let map_hex_num (env : env) (tok : CST.pat_c1c0c3a) =
  let literal, tok = (* pattern [\da-fA-F][\da-fA-F_]* *) str env tok in
  (remove_char literal '_', tok)

let map_bin_num (env : env) (tok : CST.pat_8bc5a9c) =
  let literal, tok = (* pattern 0b[01][01_]* *) str env tok in
  (remove_char literal '_', tok)

let map_hex_string (env : env) (tok : CST.tok_xdquot_pat_92a0a93_dquot) =
  let content, tok = (* pattern "x\\\"[\\da-fA-F]*\\\"" *) str env tok in
  let new_len = String.length content - 3 in
  let content = String.sub content 2 new_len in
  (content, tok)

(* let map_imm_tok_prec_p2_slash (env : env) (tok : CST.imm_tok_prec_p2_slash) =
   (* "/" *) token env tok *)

let map_raw_string (env : env)
    (tok : CST.tok_bdquot_rep_choice_imm_tok_bslash_choice_pat_9c2bd89_dquot) =
  (* tok_bdquot_rep_choice_imm_tok_bslash_choice_pat_9c2bd89_dquot *)
  let content, tok = str env tok in
  let new_len = String.length content - 3 in
  let content = String.sub content 2 new_len in
  (content, tok)

(* let map_imm_tok_prec_p2_slashslash (env : env)
     (tok : CST.imm_tok_prec_p2_slashslash) =
   (* "//" *) token env tok *)

(* let map_identifier (env : env) (tok : CST.identifier) : G.ident =
   (* identifier *) str env tok *)

(* let map_block_doc_comment_marker (env : env)
     (tok : CST.block_doc_comment_marker) =
   (* block_doc_comment_marker *) token env tok *)

let map_module_member_modifier (env : env) (x : CST.module_member_modifier) :
    G.attribute =
  match x with
  | `Visi (v1, v2) -> (
      let pub = (* "public" *) token env v1 in
      match v2 with
      | Some (v1, scope, v2) -> (
          let lb = (* "(" *) token env v1 in
          let rb = (* ")" *) token env v2 in
          let combine = Tok.combine_toks pub in
          match scope with
          | `Script tok ->
              (* "script" *)
              G.attr G.Static (combine [ lb; token env tok; rb ])
          (* abuse `protected` for `friend` *)
          | `Friend tok ->
              (* "friend" *)
              G.attr G.Protected (combine [ lb; token env tok; rb ])
          (* abuse `extern` for `package` *)
          | `Pack tok ->
              (* "package" *)
              G.attr G.Extern (combine [ lb; token env tok; rb ]))
      | None -> (* "public" *) G.attr G.Public pub)
  | `Native tok -> (* "native" *) G.attr G.Extern (token env tok)
  | `Entry tok -> (* "entry" *) G.OtherAttribute (str env tok, [])

let map_primitive_type (env : env) (x : CST.primitive_type) : G.ident =
  match x with
  | `Num_type x -> map_number_type env x
  | `Bool tok -> (* "bool" *) str env tok
  | `Addr tok -> (* "address" *) str env tok
  | `Vec tok -> (* "vector" *) str env tok

let map_constraints (env : env) ((v1, v2, v3) : CST.constraints) : G.type_ list
    =
  let v1 = (* ":" *) token env v1 in
  let v2 = map_ability env v2 in
  let v3 =
    v3
    |> List_.map (fun (v1, v2) ->
           let v1 = (* "+" *) token env v1 in
           let v2 = map_ability env v2 in
           v2)
  in
  v2 :: v3

let map_abilities (env : env) ((v0, v1, v2) : CST.abilities) : G.type_ list =
  let v0 = (* "has" *) token env v0 in
  let v1 = map_ability env v1 in
  let v2 =
    v2
    |> List_.map (fun (v1, v2) ->
           let v1 = (* "," *) token env v1 in
           let v2 = map_ability env v2 in
           v2)
  in
  v1 :: v2

let map_number (env : env) (x : CST.number) =
  match x with
  | `Pat_57a456d x -> map_deci_num env x
  | `Pat_0x_pat_c1c0c3a (v1, v2) ->
      let prefix, _ = str env v1 in
      let content, tok = map_hex_num env v2 in
      (prefix ^ content, tok)
  | `Pat_0b_pat_8bc5a9c (v1, v2) ->
      let prefix, _ = str env v1 in
      let content, tok = map_bin_num env v2 in
      (prefix ^ content, tok)
  | `Pat_0o_pat_9bd0c11 (v1, v2) ->
      let prefix, _ = str env v1 in
      let content, tok = map_oct_num env v2 in
      (prefix ^ content, tok)

let map_byte_string (env : env) (x : CST.byte_string) =
  match x with
  | `Tok_xdquot_pat_92a0a93_dquot x ->
      (G.fake "x\"", map_hex_string env x, G.fake "\"")
  | `Tok_bdquot_rep_choice_imm_tok_bslash_choice_pat_9c2bd89_dquot x ->
      (G.fake "b\"", map_raw_string env x, G.fake "\"")

(* let map_line_comment (env : env) ((v1, v2) : CST.line_comment) =
   let v1 = (* "//" *) token env v1 in
   let v2 =
     match v2 with
     | `Imm_tok_prec_p2_slas_pat_4fd4a56 (v1, v2) ->
         let v1 = map_imm_tok_prec_p2_slashslash env v1 in
         let v2 = map_pat_4fd4a56 env v2 in
         v2
     | `Imm_tok_prec_p2_slash_doc_line_comm (v1, v2) ->
         let v1 = map_imm_tok_prec_p2_slash env v1 in
         let v2 = (* doc_line_comment *) token env v2 in
         v2
     | `Imm_tok_prec_p1_pat_4fd4a56 x -> map_comment_string env x
   in
   () *)

let map_spec_apply_fragment (env : env) (x : CST.spec_apply_fragment) =
  match x with
  | `STAR tok -> (* "*" *) G.PatWildcard (token env tok)
  | `Id tok -> (* identifier *) G.PatId (str env tok, G.empty_id_info ())

let map_attribute_name (env : env) ((v1, v2) : CST.attribute_name) : G.name =
  let first = (* identifier *) str env v1 in
  let rest =
    v2
    |> List_.map (fun (v1, v2) ->
           let v1 = (* "::" *) token env v1 in
           let v2 = (* identifier *) str env v2 in
           v2)
  in
  H2.name_of_ids (first :: rest)

let map_ident_or_wildcard (env : env) (x : CST.ident_or_wildcard) =
  match x with
  | `Id tok -> (* identifier *) str env tok
  | `STAR tok -> (* "*" *) str env tok

let map_use_alias (env : env) ((v1, v2) : CST.use_alias) : G.ident =
  let v1 = (* "as" *) token env v1 in
  let v2 = (* identifier *) str env v2 in
  v2

(* let map_block_comment (env : env) ((v1, v2, v3) : CST.block_comment) =
   let v1 = (* "/*" *) token env v1 in
   let comment_TODO =
     Option.map
       (fun x ->
         match x with
         | `Blk_doc_comm_marker_opt_blk_comm_content (v1, v2) ->
             let v1 = (* block_doc_comment_marker *) token env v1 in
             Option.map (fun x -> map_block_comment_content env x) v2
         | `Blk_comm_content tok -> Some (token env tok))
       v2
   in

   let v3 = (* "*/" *) token env v3 in
   ( (* TODO: comments are ignored for now *) ) *)

let map_discouraged_name (env : env) (x : CST.discouraged_name) : G.ident =
  match x with
  | `Prim_type x -> map_primitive_type env x
  | `Quan_dire x -> map_quantifier_directive env x
  | `Reus_keywos x -> map_reuseable_keywords env x

let map_type_param (env : env) attrs (x : CST.type_param) : G.type_parameter =
  match x with
  | `Id_opt_consts (v1, v2) ->
      let type_name : G.ident = (* identifier *) str env v1 in
      let constraints =
        Option.map (fun x -> map_constraints env x) v2
        |> Option.value ~default:[]
      in
      G.tparam_of_id ~tp_attrs:(attrs @ attrs) type_name
  | `Ellips tok -> G.TParamEllipsis (token env tok)

let map_numerical_addr (env : env) (x : CST.numerical_addr) = map_number env x

let map_use_member (env : env) (x : CST.use_member) :
    (G.ident * G.ident option) option =
  match x with
  | `Id_opt_use_alias (v1, v2) ->
      let name = (* identifier *) str env v1 in
      let alias = Option.map (fun x -> map_use_alias env x) v2 in
      Some (name, alias)
  | `Ellips tok -> (* "..." *) None

let map_var_name (env : env) (x : CST.var_name) : G.ident =
  match x with
  | `Id tok -> (* identifier *) str env tok
  | `Disc_name x -> map_discouraged_name env x

let map_type_params (env : env) ((v1, v2, v3, v4) : CST.type_params) :
    G.type_parameters =
  let lt = (* "<" *) token env v1 in

  let params =
    Option.map
      (fun (first, rest) ->
        let first = map_type_param env [] first in
        let rest =
          rest
          |> List_.map (fun (_comma, type_param) ->
                 let _comma = (* "," *) token env _comma in
                 map_type_param env [] type_param)
        in
        first :: rest)
      v2
    |> Option.value ~default:[]
  in

  let _comma = Option.map (fun x -> (* "," *) token env x) v3 in
  let gt = (* ">" *) token env v4 in
  (lt, params, gt)

let map_struct_type_parameter (env : env) ((v1, v2) : CST.struct_type_parameter)
    =
  let phatom =
    Option.map ((* "phatom" *) str env) v1
    |> Option.to_list
    |> List_.map (fun x ->
           G.NamedAttr
             ( (match x with
               | _, tok -> tok),
               H2.name_of_id x,
               (sc, [], sc) ))
  in
  let v2 = map_type_param env phatom v2 in
  v2

let map_leading_name_access_wildcard (env : env)
    (x : CST.leading_name_access_wildcard) : G.ident =
  match x with
  | `Nume_addr x -> map_number env x
  | `Choice_id x -> map_ident_or_wildcard env x

let map_leading_name_access (env : env) (x : CST.leading_name_access) : G.ident
    =
  match x with
  | `Nume_addr x -> map_number env x
  | `Id tok -> (* identifier *) str env tok

let map_spec_apply_pattern (env : env) ((v1, v2, v3) : CST.spec_apply_pattern) =
  let v1 =
    v1
    |> Option.map (fun x ->
           match x with
           | `Public tok -> (* "public" *) str env tok
           | `Inte tok -> (* "internal" *) str env tok)
    |> Option.to_list
    |> List_.map (fun x -> G.I x)
  in
  let patterns =
    List_.map (map_spec_apply_fragment env) v2 |> List_.map (fun x -> G.P x)
  in
  let type_params =
    Option.map (map_type_params env) v3
    |> Option.map (fun x ->
           let _, types, _ = x in
           types)
    |> Option.value ~default:[]
    |> List_.map (fun x -> G.Tp x)
  in
  G.OtherPat (("SpecApplyPattern", G.fake ""), v1 @ patterns @ type_params)

let map_struct_type_params (env : env)
    ((v1, v2, v3, v4) : CST.struct_type_params) : G.type_parameters =
  let lt = (* "<" *) token env v1 in
  let params =
    v2
    |> Option.map (fun (v1, v2) ->
           let first = map_struct_type_parameter env v1 in
           let rest =
             List_.map
               (fun (v1, v2) ->
                 let v1 = (* "," *) token env v1 in
                 let v2 = map_struct_type_parameter env v2 in
                 v2)
               v2
           in
           first :: rest)
    |> Option.value ~default:[]
  in
  let _comma = Option.map (fun x -> (* "," *) token env x) v3 in
  let gt = (* ">" *) token env v4 in
  (lt, params, gt)

let map_name_access_chain_wildcard (env : env)
    (x : CST.name_access_chain_wildcard) =
  match x with
  | `Choice_choice_id x -> (
      match x with
      | `Choice_id x -> H2.name_of_id (map_ident_or_wildcard env x)
      | `Disc_name x -> H2.name_of_id (map_discouraged_name env x))
  | `Choice_lead_name_access_wild_COLONCOLON_choice_id_opt_COLONCOLON_choice_id_opt_COLONCOLON_choice_id
      (v1, v2, v3, v4) -> (
      let first =
        match v1 with
        | `Lead_name_access_wild x -> map_leading_name_access_wildcard env x
        | `Disc_name x -> map_discouraged_name env x
      in
      let v2 = (* "::" *) token env v2 in
      let second = map_ident_or_wildcard env v3 in

      match v4 with
      | Some (v1, v2, v3) ->
          let v1 = (* "::" *) token env v1 in
          let third = map_ident_or_wildcard env v2 in
          let forth_opt =
            v3
            |> Option.map (fun (v1, v2) ->
                   let v1 = (* "::" *) token env v1 in
                   map_ident_or_wildcard env v2)
            |> Option.to_list
          in
          H2.name_of_ids ([ first; second; third ] @ forth_opt)
      | None -> H2.name_of_ids [ first; second ])

let map_module_ident (env : env) ((v1, v2, v3) : CST.module_ident) =
  let addr = map_leading_name_access env v1 in
  let v2 = (* "::" *) token env v2 in
  let mod_name = (* identifier *) str env v3 in
  (addr, mod_name)

let map_number (num : G.ident) : G.literal =
  let num_str, _ = num in
  let num_val = Parsed_int.parse num in
  (* Numbers in move can be up to 256 bits, so we use rational numbers to represent them *)
  match num_val with
  | None, _ -> G.Ratio (num_str, G.fake "")
  | _ -> G.Int num_val
(* G.Int num_val *)

let map_value (env : env) (x : CST.value) : G.expr_kind =
  match x with
  | `AT_lead_name_access (v1, v2) ->
      let v1 = (* "@" *) token env v1 in
      let v2 = map_leading_name_access env v2 in
      G.OtherExpr (("address literal", v1), [ G.Tk v1; G.I v2 ])
  | `Bool_lit x -> G.L (map_bool_literal env x)
  | `Num x -> G.L (map_number (map_numerical_addr env x))
  | `Typed_num (v1, v2) ->
      (* let num, _ = map_numerical_addr env v1 in *)
      let _, num_type = map_number_type env v2 in
      G.L (map_number (map_numerical_addr env v1))
  | `Byte_str x -> G.L (G.String (map_byte_string env x))

let map_name_access_chain (env : env) (x : CST.name_access_chain) : G.name =
  match x with
  | `Choice_id x -> H2.name_of_id (map_var_name env x)
  | `Choice_lead_name_access_COLONCOLON_id_opt_COLONCOLON_id_opt_COLONCOLON_id
      (v1, v2, v3, v4) -> (
      let top =
        match v1 with
        | `Lead_name_access x -> map_leading_name_access env x
        | `Disc_name x -> map_discouraged_name env x
      in
      let v2 = (* "::" *) token env v2 in
      let access_one = (* identifier *) str env v3 in

      match v4 with
      | Some (v1, v2, v3) ->
          let v1 = (* "::" *) token env v1 in
          let access_two = (* identifier *) str env v2 in
          let access_three_opt =
            v3
            |> Option.map (fun (v1, v2) ->
                   let v1 = (* "::" *) token env v1 in
                   (* identifier *) str env v2)
            |> Option.to_list
          in
          H2.name_of_ids ([ top; access_one; access_two ] @ access_three_opt)
      | None -> H2.name_of_ids [ top; access_one ])

let map_anon_spec_apply_pat_rep_COMMA_spec_apply_pat_d9a21d6 (env : env)
    ((v1, v2) : CST.anon_spec_apply_pat_rep_COMMA_spec_apply_pat_d9a21d6) =
  let first = map_spec_apply_pattern env v1 in
  let rest =
    List_.map
      (fun (v1, v2) ->
        let v1 = (* "," *) token env v1 in
        let v2 = map_spec_apply_pattern env v2 in
        v2)
      v2
  in
  first :: rest

let map_struct_def_name (env : env) ((v1, v2) : CST.struct_def_name) =
  let ident = (* identifier *) str env v1 in
  let type_params = v2 |> Option.map (fun x -> map_struct_type_params env x) in
  (ident, type_params)

let map_use_decl (env : env) attrs ((v1, v2, v3, v4) : CST.use_decl) :
    G.directive list =
  let use_ = (* "use" *) token env v1 in
  let addr, mod_name = map_module_ident env v2 in
  let v3 =
    match v3 with
    | `Opt_use_alias opt -> (
        match opt with
        | Some x ->
            let alias = map_use_alias env x in
            [
              G.ImportFrom
                ( use_,
                  G.DottedName [ addr ],
                  [ H2.mk_import_from_kind mod_name (Some alias) ] );
            ]
        | None ->
            [
              G.ImportFrom
                ( use_,
                  G.DottedName [ addr ],
                  [ H2.mk_import_from_kind mod_name None ] );
            ])
    | `COLONCOLON_use_member (v1, v2) ->
        let v1 = (* "::" *) token env v1 in
        map_use_member env v2
        |> Option.map (fun (name, alias) ->
               match name with
               | "Self", _ ->
                   G.ImportFrom
                     ( use_,
                       G.DottedName [ addr ],
                       [ H2.mk_import_from_kind mod_name alias ] )
               | _ ->
                   G.ImportFrom
                     ( use_,
                       G.DottedName [ addr; mod_name ],
                       [ H2.mk_import_from_kind name alias ] ))
        |> Option.to_list
    | `COLONCOLON_LCURL_opt_use_member_rep_COMMA_use_member_opt_COMMA_RCURL
        (v1, v2, v3, v4, v5) ->
        let v1 = (* "::" *) token env v1 in
        let v2 = (* "{" *) token env v2 in
        let members =
          v3
          |> Option.map (fun (v1, v2) ->
                 let first = map_use_member env v1 in
                 let rest =
                   List_.map
                     (fun (v1, v2) ->
                       let v1 = (* "," *) token env v1 in
                       let v2 = map_use_member env v2 in
                       v2)
                     v2
                 in
                 first :: rest)
          |> Option.value ~default:[]
          |> List_.filter_map (fun x -> x)
        in

        let _comma = v4 |> Option.map (fun x -> (* "," *) token env x) in
        let v5 = (* "}" *) token env v5 in

        (* [ G.ImportFrom (use_, G.DottedName [ addr; mod_name_resolvable ], members) ] *)
        members
        |> List_.map (fun (name, alias) ->
               match name with
               | "Self", _ ->
                   G.ImportFrom
                     ( use_,
                       G.DottedName [ addr ],
                       [ H2.mk_import_from_kind mod_name alias ] )
               | _ ->
                   G.ImportFrom
                     ( use_,
                       G.DottedName [ addr; mod_name ],
                       [ H2.mk_import_from_kind name alias ] ))
  in

  let _semicolon = (* ";" *) token env v4 in
  v3 |> List_.map (fun x -> { d = x; d_attrs = attrs })

let map_friend_decl (env : env) ((v1, v2, v3) : CST.friend_decl) =
  let friend = (* "friend" *) str env v1 in
  let member = map_name_access_chain env v2 in
  let v3 = (* ";" *) token env v3 in

  G.OtherDirective (friend, [ G.Name member ]) |> G.d

let rec map_type_list (env : env)
    ((v1, v2) : CST.anon_type__rep_COMMA_type__ac59fb8) : G.type_ list =
  let first = map_type__ env v1 in
  let rest =
    v2
    |> List_.map (fun (v1, v2) ->
           let v1 = (* "," *) token env v1 in
           let v2 = map_type__ env v2 in
           v2)
  in
  first :: rest

and map_ref_type (env : env) (x : CST.ref_type) : G.type_ =
  match x with
  | `AMP_type (v1, v2) ->
      let v1 = (* "&" *) token env v1 in
      let v2 = map_type_ env v2 in
      G.TyRef (v1, v2) |> G.t
  | `AMPmut_type (v1, v2) ->
      let v1 = (* "&mut" *) token env v1 in
      let v2 = map_type_ env v2 in
      { t = G.TyRef (v1, v2); t_attrs = [ G.KeywordAttr (G.Mutable, v1) ] }

and map_type_ (env : env) (x : CST.type_) : G.type_ =
  match x with
  | `Choice_name_access_chain_opt_type_args (v1, v2) -> (
      let v1 =
        match v1 with
        | `Name_access_chain x -> G.TyN (map_name_access_chain env x) |> G.t
        | `Prim_type x -> G.ty_builtin (map_primitive_type env x)
      in
      match v2 with
      | Some x -> G.TyApply (v1, map_type_args env x) |> G.t
      | None -> v1)
  | `Clos_type (v1, v2, v3, v4, v5) ->
      let v1 = (* "|" *) token env v1 in
      let params =
        v2
        |> Option.map (fun x -> map_type_list env x)
        |> Option.value ~default:[] |> List_.map param_of_type
        |> List_.map (fun x -> G.Param x)
      in
      let v3 = v3 |> Option.map (fun x -> (* "," *) token env x) in
      let v4 = (* "|" *) token env v4 in
      let ret_type =
        v5
        |> Option.map (map_type__ env)
        |> Option.value ~default:(G.TyTuple (sc, [], sc) |> G.t)
      in

      G.TyFun (params, ret_type) |> G.t
  | `Tuple_type (v1, v2, v3, v4) ->
      let v1 = (* "(" *) token env v1 in
      let type_list =
        v2 |> Option.map (map_type_list env) |> Option.value ~default:[]
      in
      let v3 = v3 |> Option.map (fun x -> (* "," *) token env x) in
      let v4 = (* ")" *) token env v4 in
      G.TyTuple (v1, type_list, v4) |> G.t

and map_type__ (env : env) (x : CST.type__) =
  match x with
  | `Type x -> map_type_ env x
  | `Ref_type x -> map_ref_type env x
  | `Ellips tok -> (* "..." *) G.TyEllipsis (token env tok) |> G.t

and map_type_args (env : env) ((v1, v2, v3, v4) : CST.type_args) :
    G.type_arguments =
  let lt = (* "<" *) token env v1 in
  let types =
    v2
    |> Option.map (fun x -> map_type_list env x)
    |> Option.value ~default:[]
    |> List_.map (fun x -> G.TA x)
  in
  let v3 = v3 |> Option.map (fun x -> (* "," *) token env x) in
  let gt = (* ">" *) token env v4 in
  (lt, types, gt)

let map_attribute_pair (env : env) (x : CST.anon_choice_value_f266929) =
  match x with
  | `Value x -> map_value env x
  | `Name_access_chain x -> G.N (map_name_access_chain env x)

let map_attribute_val (env : env) (x : CST.attribute_val) =
  match x with
  | `Choice_value x -> map_attribute_pair env x
  | `Ellips tok -> (* "..." *) G.Ellipsis (token env tok)

let rec map_bind (env : env) (x : CST.bind) : G.pattern =
  match x with
  | `Var_name x -> (
      let name = map_var_name env x in
      match name with
      | "_", tok -> G.PatWildcard tok
      | _ -> G.PatId (name, G.empty_id_info ()))
  | `Name_access_chain_opt_type_args_opt_choice_bind_fields (v1, v2, v3) -> (
      let struct_name = map_name_access_chain env v1 in
      let type_args = v2 |> Option.map (fun x -> map_type_args env x) in
      let struct_type =
        G.PatType
          ((match type_args with
           | Some x -> G.TyApply (G.TyN struct_name |> G.t, x)
           | None -> G.TyN struct_name)
          |> G.t)
      in
      match v3 with
      | Some v3 -> (
          match v3 with
          | `Bind_fields x ->
              let lbrace, fields, rbrace = map_bind_fields env x in
              (* abused for record binding in move, so that struct is matchable *)
              G.PatDisj (struct_type, G.PatRecord (lbrace, fields, rbrace))
          | `Bind_tuple x -> G.PatConstructor (struct_name, map_bind_tuple env x)
          )
      | None -> struct_type)
  | `Ellips tok -> G.PatEllipsis (token env tok)

and map_bind_tuple (env : env) ((v1, v2, v3, v4) : CST.bind_tuple) =
  let v1 = (* "(" *) token env v1 in
  let items =
    v2
    |> Option.map (fun (v1, v2) ->
           let first = map_bind env v1 in
           let rest =
             v2
             |> List_.map (fun (v1, v2) ->
                    let v1 = (* "," *) token env v1 in
                    map_bind env v2)
           in
           first :: rest)
    |> Option.value ~default:[]
  in
  let v3 = v3 |> Option.map (fun x -> (* "," *) token env x) in
  let v4 = (* ")" *) token env v4 in
  items

and map_bind_field (env : env) (x : CST.bind_field) : G.dotted_ident * G.pattern
    =
  match x with
  | `Choice_var_name x -> (
      match x with
      | `Var_name x ->
          let ident = map_var_name env x in
          ([ ident ], G.PatId (ident, G.empty_id_info ()))
      | `Var_name_COLON_bind (v1, v2, v3) ->
          let ident = map_var_name env v1 in
          let v2 = (* ":" *) token env v2 in
          let pat = map_bind env v3 in
          ([ ident ], pat))
  | `Ellips tok ->
      (* "..." *)
      (* Unfortunately, this is not working. *)
      let ident = str env tok in
      ([ ident ], G.PatEllipsis (token env tok))

and map_bind_fields (env : env) ((v1, v2, v3, v4) : CST.bind_fields) =
  let v1 = (* "{" *) token env v1 in
  let v2 =
    v2
    |> Option.map (fun (v1, v2) ->
           let first = map_bind_field env v1 in
           let rest =
             v2
             |> List_.map (fun (v1, v2) ->
                    let v1 = (* "," *) token env v1 in
                    let v2 = map_bind_field env v2 in
                    v2)
           in
           first :: rest)
    |> Option.value ~default:[]
  in
  let v3 = v3 |> Option.map (fun x -> (* "," *) token env x) in
  let v4 = (* "}" *) token env v4 in
  (v1, v2, v4)

let map_address_specifier (env : env) (x : CST.address_specifier) : G.pattern =
  match x with
  | `STAR tok -> (* "*" *) G.PatWildcard (token env tok)
  | `Nume_addr x -> G.PatId (map_numerical_addr env x, G.empty_id_info ())
  | `Name_access_chain_opt_opt_type_args_LPAR_id_RPAR (v1, v2) -> (
      let name = map_name_access_chain env v1 in
      match v2 with
      | Some (v1, v2, v3, v4) ->
          let type_args = Option.map (map_type_args env) v1 in
          let v2 = (* "(" *) token env v2 in
          let ident = (* identifier *) str env v3 in
          let v4 = (* ")" *) token env v4 in
          let name = G.Name (H2.add_type_args_opt_to_name name type_args) in
          G.OtherPat (("AddressSpecifier", G.fake ""), [ name; G.I ident ])
      | None -> G.OtherPat (("AddressSpecifier", G.fake ""), [ G.Name name ]))

let map_spec_pragma_prop (env : env) ((v1, v2) : CST.spec_pragma_prop) =
  let name = map_var_name env v1 in
  let name = G.N (H2.name_of_id name) |> G.e in

  match v2 with
  | Some (v1, v2) ->
      let v1 = (* "=" *) token env v1 in
      let v2 = map_attribute_pair env v2 in
      G.Assign (name, v1, v2 |> G.e) |> G.e
  | None -> name

let rec build_nested_attr_arguments (exprs : G.expr list) : G.argument list =
  exprs
  |> List_.map (fun item ->
         match item.e with
         | G.Ellipsis _ -> G.Arg item
         | G.N name -> G.Arg item
         | G.Assign (name, _, rhs) -> (
             match rhs.e with
             | G.Container (G.Dict, (lbrace, attrs, rbrace)) ->
                 let args = build_nested_attr_arguments attrs in
                 G.Arg (G.Call (name, (lbrace, args, rbrace)) |> G.e)
             | _ -> (
                 match name.e with
                 | G.N (G.Id (ident, _)) -> G.ArgKwd (ident, rhs)
                 (* Attribute names can be FQNs *)
                 | _ -> G.Arg item))
         | _ -> failwith "unexpected expression type in nested attr arguments")

let build_attr_list (exprs : G.expr list) : G.attribute list =
  (* The highest level of items in attribute list are treated as attributes *)
  exprs
  |> List_.map (fun item ->
         match item.e with
         | G.Ellipsis tok -> [ (* Ellipsis is essentially empty *) ]
         | G.N name -> [ G.NamedAttr (sc, name, (sc, [], sc)) ]
         | G.Assign (name, _, expr) -> (
             let name =
               match name.e with
               | G.N name -> name
               | _ -> failwith "unexpected name in nested attribute"
             in
             match expr.e with
             | G.Container (G.Dict, (lbrace, attrs, rbrace)) ->
                 (* Nested attributes are arguments *)
                 let args = build_nested_attr_arguments attrs in
                 [ G.NamedAttr (sc, name, (lbrace, args, rbrace)) ]
             | G.N _
             | G.Ellipsis _ ->
                 [ G.NamedAttr (sc, name, (sc, [ G.Arg expr ], sc)) ]
             | _ -> failwith "unexpected expression in nested attribute")
         | _ -> failwith "Unsupported pattern in let binding")
  |> List_.flatten

let rec map_attribute_expr_list (env : env) tok
    ((v1, v2) : CST.anon_attr_rep_COMMA_attr_246bec5) =
  let first = map_attribute_expr env tok v1 in
  let rest =
    List_.map
      (fun (v1, v2) ->
        let v1 = (* "," *) token env v1 in
        let v2 = map_attribute_expr env tok v2 in
        v2)
      v2
  in
  first :: rest

and map_attribute_list (env : env) tok
    (x : CST.anon_attr_rep_COMMA_attr_246bec5) : G.attribute list =
  (* Parse attributes as expressions first. Then, transform them into attributes.
     This is because
      1. expr's are more expressive;
      2. move supports nested attributes while semgrep doesn't;
      3. attributes have limited variants.
     Top-level attributes are treated as attributes. Nested attributes are placed
      in arguments.
  *)
  let expr_list = map_attribute_expr_list env tok x in
  build_attr_list expr_list

and map_attribute_expr (env : env) tok (x : CST.attribute) : G.expr =
  match x with
  | `Choice_attr_name x -> (
      match x with
      | `Attr_name x ->
          let name = map_attribute_name env x in
          G.N name |> G.e
      | `Attr_name_EQ_attr_val (v1, v2, v3) ->
          let name = map_attribute_name env v1 in
          let v2 = (* "=" *) token env v2 in
          let expr = map_attribute_val env v3 |> G.e in
          G.Assign (G.N name |> G.e, v2, expr) |> G.e
      | `Attr_name_LPAR_opt_attr_rep_COMMA_attr_opt_COMMA_RPAR
          (v1, v2, v3, v4, v5) ->
          let name = map_attribute_name env v1 in
          let v2 = (* "(" *) token env v2 in
          let attrs =
            v3
            |> Option.map (map_attribute_expr_list env tok)
            |> Option.value ~default:[]
          in
          let _comma = Option.map (fun x -> (* "," *) token env x) v4 in
          let v5 = (* ")" *) token env v5 in

          (* We don't want to use function call here because arguments are hard
             to convert. There is a separate pass to convert expressions into
             arguments. *)
          G.Assign
            (G.N name |> G.e, sc, G.Container (G.Dict, (v2, attrs, v5)) |> G.e)
          |> G.e)
  | `Ellips tok -> G.Ellipsis (token env tok) |> G.e

let map_anon_bind_rep_COMMA_bind_38cc8c1 (env : env)
    ((v1, v2) : CST.anon_bind_rep_COMMA_bind_38cc8c1) =
  let first = map_bind env v1 in
  let rest =
    v2
    |> List_.map (fun (v1, v2) ->
           let v1 = (* "," *) token env v1 in
           let v2 = map_bind env v2 in
           v2)
  in
  first :: rest

let map_access_specifier (env : env) ((v1, v2, v3) : CST.access_specifier) :
    G.name =
  let ident = map_name_access_chain_wildcard env v1 in
  let type_args = v2 |> Option.map (map_type_args env) in
  let addr_spec_TODO =
    v3
    |> Option.map (fun (v1, v2, v3) ->
           let v1 = (* "(" *) token env v1 in
           let v2 = map_address_specifier env v2 in
           let v3 = (* ")" *) token env v3 in
           v2)
  in
  H2.add_type_args_opt_to_name ident type_args

let map_parameter (env : env) (x : CST.parameter) : G.parameter =
  match x with
  | `Id_COLON_type_ (v1, v2, v3) ->
      let ident = (* identifier *) str env v1 in
      let v2 = (* ":" *) token env v2 in
      let param_type = map_type__ env v3 in
      G.Param (G.param_of_id ~ptype:param_type ident)
  | `Ellips tok -> (* "..." *) G.ParamEllipsis (token env tok)

let map_field_annot (env : env) (x : CST.field_annot) : G.field =
  match x with
  | `Id_COLON_type_ (v1, v2, v3) ->
      let ident = (* identifier *) str env v1 in
      let v2 = (* ":" *) token env v2 in
      let field_type = map_type__ env v3 in

      let var_def =
        { G.vinit = None; G.vtype = Some field_type; vtok = G.no_sc }
      in
      let ent = G.basic_entity ident in
      G.fld (ent, G.FieldDefColon var_def)
  | `Ellips tok -> (* "..." *) G.field_ellipsis (token env tok)

let map_anon_spec_pragma_prop_rep_COMMA_spec_pragma_prop_588d25f (env : env)
    ((v1, v2) : CST.anon_spec_pragma_prop_rep_COMMA_spec_pragma_prop_588d25f) =
  let first = map_spec_pragma_prop env v1 in
  let rest =
    List_.map
      (fun (v1, v2) ->
        let v1 = (* "," *) token env v1 in
        let v2 = map_spec_pragma_prop env v2 in
        v2)
      v2
  in
  first :: rest

let map_attributes (env : env) (xs : CST.attributes) =
  xs
  |> List_.map (fun (v1, v2, v3, v4, v5) ->
         let hash = (* "#" *) token env v1 in
         let v2 = (* "[" *) token env v2 in
         let attrs =
           v3
           |> Option.map (map_attribute_list env hash)
           |> Option.value ~default:[]
         in
         let _comma = v4 |> Option.map (fun x -> (* "," *) token env x) in
         let v5 = (* "]" *) token env v5 in
         attrs)
  |> List_.flatten

let map_bind_list (env : env) (x : CST.bind_list) =
  match x with
  | `Bind x -> map_bind env x
  | `LPAR_opt_bind_rep_COMMA_bind_opt_COMMA_RPAR (v1, v2, v3, v4) ->
      let v1 = (* "(" *) token env v1 in
      let v2 =
        v2
        |> Option.map (map_anon_bind_rep_COMMA_bind_38cc8c1 env)
        |> Option.value ~default:[]
      in
      let _comma = v3 |> Option.map (fun x -> (* "," *) token env x) in
      let v4 = (* ")" *) token env v4 in
      G.PatTuple (v1, v2, v4)

let map_lambda_bind_list (env : env) ((v1, v2, v3, v4) : CST.lambda_bind_list) =
  let lp = (* "|" *) token env v1 in
  let binds =
    v2
    |> Option.map (map_anon_bind_rep_COMMA_bind_38cc8c1 env)
    |> Option.value ~default:[]
    |> List_.map (fun x -> G.ParamPattern x)
  in
  let _comma = v3 |> Option.map (fun x -> (* "," *) token env x) in
  let rp = (* "|" *) token env v4 in
  let params = (lp, binds, rp) in

  (lp, params, rp)

let map_access_specifier_list (env : env)
    ((v1, v2, v3) : CST.access_specifier_list) =
  let first = map_access_specifier env v1 in
  let rest =
    List_.map
      (fun (v1, v2) ->
        let v1 = (* "," *) token env v1 in
        let v2 = map_access_specifier env v2 in
        v2)
      v2
  in
  let v3 = Option.map (fun x -> (* "," *) token env x) v3 in
  first :: rest

let map_parameters (env : env) ((v1, v2, v3, v4) : CST.parameters) :
    G.parameters =
  let lb = (* "(" *) token env v1 in
  let params =
    v2
    |> Option.map (fun (v1, v2) ->
           let first = map_parameter env v1 in
           let rest =
             v2
             |> List_.map (fun (v1, v2) ->
                    let v1 = (* "," *) token env v1 in
                    let v2 = map_parameter env v2 in
                    v2)
           in
           first :: rest)
    |> Option.value ~default:[]
  in
  let _ = v3 |> Option.map (fun x -> (* "," *) token env x) in
  let rb = (* ")" *) token env v4 in
  (lb, params, rb)

let map_struct_signature (env : env) attrs ((v1, v2, v3) : CST.struct_signature)
    =
  let struct_ = (* "struct" *) token env v1 in
  let name, type_params = map_struct_def_name env v2 in
  let abilities =
    v3 |> Option.map (map_abilities env) |> Option.value ~default:[]
  in
  let struct_ent = G.basic_entity ?tparams:type_params ~attrs name in
  (struct_, abilities, struct_ent)

let map_struct_body (env : env) ((v1, v2, v3, v4) : CST.struct_body) =
  let lb = (* "{" *) token env v1 in
  let fields =
    v2
    |> Option.map (fun (v1, v2) ->
           let first = map_field_annot env v1 in
           let rest =
             List_.map
               (fun (v1, v2) ->
                 let v1 = (* "," *) token env v1 in
                 map_field_annot env v2)
               v2
           in
           first :: rest)
    |> Option.value ~default:[]
  in
  let _comma = v3 |> Option.map (fun x -> (* "," *) token env x) in
  let rb = (* "}" *) token env v4 in
  (lb, fields, rb)

let map_anon_fields (env : env) ((v1, v2, v3, v4) : CST.anon_fields) =
  let lp = (* "(" *) token env v1 in
  let items =
    v2
    |> Option.map (fun (v1, v2) ->
           let first = map_type__ env v1 in
           let rest =
             List_.map
               (fun (v1, v2) ->
                 let v1 = (* "," *) token env v1 in
                 map_type__ env v2)
               v2
           in
           first :: rest)
    |> Option.value ~default:[]
  in
  let v3 = v3 |> Option.map (fun x -> (* "," *) token env x) in
  let rp = (* ")" *) token env v4 in
  (lp, items, rp)

let map_condition_props (env : env) ((v1, v2, v3, v4) : CST.condition_props) =
  let v1 = (* "[" *) token env v1 in
  let props =
    v2
    |> Option.map
         (map_anon_spec_pragma_prop_rep_COMMA_spec_pragma_prop_588d25f env)
    |> Option.value ~default:[]
    |> List_.map (fun x -> G.E x)
  in
  let v3 = Option.map (fun x -> (* "," *) token env x) v3 in
  let v4 = (* "]" *) token env v4 in
  G.OtherAttribute (("ConditionProps", v1), props)

let map_spec_pragma (env : env) ((v1, v2, v3, v4) : CST.spec_pragma) =
  let v1 = (* "pragma" *) str env v1 in
  let v2 =
    v2
    |> Option.map
         (map_anon_spec_pragma_prop_rep_COMMA_spec_pragma_prop_588d25f env)
    |> Option.value ~default:[]
    |> List_.map (fun x -> G.E x)
  in
  let v3 = Option.map (fun x -> (* "," *) token env x) v3 in
  let v4 = (* ";" *) token env v4 in
  (* G.Call (G.N (H2.name_of_id v1) |> G.e, (sc, v2, sc)) |> G.e |> G.exprstmt *)
  G.OtherStmt (G.OS_Todo, v2)

let map_script_use_decl (env : env) ext_attrs (x : CST.script_use_decl) =
  match x with
  | `Opt_attris_use_decl (v1, v2) ->
      let attrs =
        Option.map (map_attributes env) v1 |> Option.value ~default:[]
      in
      map_use_decl env (ext_attrs @ attrs) v2
      |> List_.map (fun x -> G.DirectiveStmt x |> G.s)
  | `Ellips tok -> [ G.Ellipsis (token env tok) |> G.e |> G.exprstmt ]

let map_specifier (env : env) (x : CST.specifier) : G.attribute list =
  match x with
  | `Pure tok ->
      (* "pure" *)
      let pure = token env tok in
      [ G.NamedAttr (pure, H2.name_of_id (str env tok), fb []) ]
  | `Rep1_opt_BANG_choice_acquis_access_spec_list xs ->
      xs
      |> List_.map (fun (v1, v2, v3) ->
             let negate =
               match v1 with
               | Some tok ->
                   let tok = (* "!" *) token env tok in
                   G.Arg (G.L (G.Bool (false, tok)) |> G.e)
               | None -> G.Arg (G.L (G.Bool (true, sc)) |> G.e)
             in
             let specifier =
               match v2 with
               | `Acquis tok -> (* "acquires" *) str env tok
               | `Reads tok -> (* "reads" *) str env tok
               | `Writes tok -> (* "writes" *) str env tok
             in
             let objects =
               map_access_specifier_list env v3
               |> List_.map (fun x -> G.ArgType (G.TyN x |> G.t))
               |> fun args -> (sc, negate :: args, sc)
             in
             G.NamedAttr (snd specifier, H2.name_of_id specifier, objects))

let map_spec_func_signatures (env : env)
    ((v1, v2, v3, v4, v5) : CST.spec_func_signatures) =
  let func_name = (* identifier *) str env v1 in
  let type_params = Option.map (map_type_params env) v2 in
  let params = map_parameters env v3 in
  let v4 = (* ":" *) token env v4 in
  let ret_type = map_type__ env v5 in

  let entity = G.basic_entity ?tparams:type_params func_name in
  (entity, params, ret_type)

let map_spec_target_signature_opt (env : env)
    ((v1, v2, v3) : CST.spec_target_signature_opt) =
  let type_params = Option.map (map_type_params env) v1 in
  let params = map_parameters env v2 in
  let ret_type =
    Option.map
      (fun (v1, v2) ->
        let v1 = (* ":" *) token env v1 in
        let v2 = map_type__ env v2 in
        v2)
      v3
  in
  (type_params, params, ret_type)

let map_struct_decl (env : env) attrs (x : CST.struct_decl) : G.stmt =
  match x with
  | `Struct_sign_choice_struct_body (v1, v2) ->
      let struct_, abilities, struct_ent = map_struct_signature env attrs v1 in
      let body =
        match v2 with
        | `Struct_body x -> map_struct_body env x
        | `SEMI tok -> (* ";" *) (sc, [], sc)
      in
      let struct_def =
        {
          ckind = (G.Class, struct_);
          cextends = [];
          cimplements = abilities;
          cmixins = [];
          cparams = fb [];
          cbody = body;
        }
      in
      G.DefStmt (struct_ent, G.ClassDef struct_def) |> G.s
  | `Struct_struct_def_name_struct_body_opt_abilis_SEMI (v1, v2, v3, v4, v5) ->
      let struct_ = (* "struct" *) token env v1 in
      let name, tparams = map_struct_def_name env v2 in
      let struct_ent = G.basic_entity name ?tparams in

      let body = map_struct_body env v3 in
      let abilities =
        v4 |> Option.map (map_abilities env) |> Option.value ~default:[]
      in
      let v5 = (* ";" *) token env v5 in
      let struct_def =
        {
          ckind = (G.Class, struct_);
          cextends = [];
          cimplements = abilities;
          cmixins = [];
          cparams = fb [];
          cbody = body;
        }
      in
      G.DefStmt (struct_ent, G.ClassDef struct_def) |> G.s
  | `Struct_struct_def_name_anon_fields_opt_abilis_SEMI (v1, v2, v3, v4, v5) ->
      let struct_ = (* "struct" *) token env v1 in
      let name, tparams = map_struct_def_name env v2 in
      let struct_ent = G.basic_entity name ?tparams in

      let lp, body, rp = map_anon_fields env v3 in
      let abilities =
        v4 |> Option.map (map_abilities env) |> Option.value ~default:[]
      in
      let v5 = (* ";" *) token env v5 in

      let body =
        body
        |> List_.mapi (fun idx x ->
               let var_def =
                 { G.vinit = None; G.vtype = Some x; vtok = G.no_sc }
               in
               let name =
                 G.EDynamic (G.L (G.Int (Parsed_int.of_int idx)) |> G.e)
               in
               let ent = { G.name; G.attrs = []; G.tparams = None } in
               G.fld (ent, G.FieldDefColon var_def))
      in
      let struct_def =
        {
          ckind = (G.Class, struct_);
          cextends = [];
          cimplements = abilities;
          cmixins = [];
          cparams = fb [];
          cbody = (lp, body, rp);
        }
      in
      G.DefStmt (struct_ent, G.ClassDef struct_def) |> G.s

let map_enum_variant_struct (env : env) ((v1, v2) : CST.enum_variant_struct) =
  let variant = (* identifier *) str env v1 in
  let lbrace, fields, rbrace = map_struct_body env v2 in
  let record_type = G.TyRecordAnon ((G.Class, sc), (lbrace, fields, rbrace)) in
  G.OrUnion (variant, record_type |> G.t)

let map_enum_variant_posit (env : env) ((v1, v2) : CST.enum_variant_posit) =
  let variant = (* identifier *) str env v1 in
  let lp, fields, rp = map_anon_fields env v2 in
  G.OrConstructor (variant, fields)

let map_variant (env : env) (x : CST.variant) : G.or_type_element =
  match x with
  | `Choice_enum_vari_struct_opt_COMMA x -> (
      match x with
      | `Enum_vari_struct_opt_COMMA (v1, v2) ->
          let v1 = map_enum_variant_struct env v1 in
          let v2 = v2 |> Option.map ((* "," *) token env) in
          v1
      | `Enum_vari_COMMA (v1, v2) ->
          let v1 = (* identifier *) str env v1 in
          let v2 = (* "," *) token env v2 in
          G.OrEnum (v1, None)
      | `Enum_vari_posit_COMMA (v1, v2) ->
          let v1 = map_enum_variant_posit env v1 in
          let v2 = (* "," *) token env v2 in
          v1)
  | `Ellips tok -> (* "..." *) G.OrEllipsis (token env tok)

let map_enum_body (env : env) ((v1, v2, v3, v4) : CST.enum_body) =
  let lb = (* "{" *) token env v1 in
  let variants = List_.map (map_variant env) v2 in

  (* last `identifier` type variant can omit its ','*)
  let last_opt =
    v3
    |> Option.map (fun (v1, _) ->
           match v1 with
           | `Enum_vari tok ->
               let variant = (* identifier *) str env tok in
               G.OrEnum (variant, None)
           | `Enum_vari_struct x -> map_enum_variant_struct env x
           | `Enum_vari_posit x -> map_enum_variant_posit env x)
    |> Option.to_list
  in
  let rb = (* "}" *) token env v4 in
  (lb, variants @ last_opt, rb)

let map_enum_signature (env : env) attrs ((v1, v2, v3) : CST.enum_signature) =
  let enum_ = (* "enum" *) token env v1 in
  let name, tparams = map_struct_def_name env v2 in
  let abilities =
    v3 |> Option.map (map_abilities env) |> Option.value ~default:[]
  in
  let ent = G.basic_entity ?tparams ~attrs name in
  (enum_, abilities, ent)

let inject_enum_abilities (env : env) (abilities : G.type_ list)
    (ent : G.entity) =
  let ability_attr =
    G.NamedAttr
      ( sc,
        G.Id (("has", sc), G.empty_id_info ()),
        (sc, abilities |> List_.map (fun x -> G.ArgType x), sc) )
  in
  { ent with G.attrs = ent.G.attrs @ [ ability_attr ] }

let map_enum_decl (env : env) attrs (x : CST.enum_decl) =
  match x with
  | `Enum_sign_enum_body (v1, v2) ->
      let _, abilities, ent = map_enum_signature env attrs v1 in
      let lb, variants, rb = map_enum_body env v2 in

      let ent = inject_enum_abilities env abilities ent in
      let def = G.TypeDef { G.tbody = G.OrType variants } in
      G.DefStmt (ent, def) |> G.s
  | `Enum_struct_def_name_enum_body_opt_abilis_SEMI (v1, v2, v3, v4, v5) ->
      let enum_ = (* "enum" *) token env v1 in
      let name, tparams = map_struct_def_name env v2 in
      let lb, variants, rb = map_enum_body env v3 in
      let abilities =
        v4 |> Option.map (map_abilities env) |> Option.value ~default:[]
      in
      let v5 = (* ";" *) token env v5 in

      let ent = G.basic_entity ?tparams ~attrs name in
      let ent = inject_enum_abilities env abilities ent in
      let def = G.TypeDef { G.tbody = G.OrType variants } in
      G.DefStmt (ent, def) |> G.s

let map_spec_block_target (env : env) (x : CST.spec_block_target) : G.any =
  match x with
  | `Id_opt_spec_target_sign_opt (v1, v2) -> (
      let ident = (* identifier *) str env v1 in
      match v2 with
      | Some x ->
          let type_params, params, ret_type =
            map_spec_target_signature_opt env x
          in
          let entity = G.basic_entity ?tparams:type_params ident in
          let def_ =
            {
              G.fkind = (G.Function, sc);
              G.fparams = params;
              G.frettype = ret_type;
              G.fbody = G.FBNothing;
            }
          in
          G.Anys [ G.Def (entity, G.FuncDef def_) ]
      | None -> G.Name (H2.name_of_id ident))
  | `Module tok -> (* "module" *) G.I (str env tok)
  | `Schema_id_opt_type_params (v1, v2, v3) ->
      let v1 = (* "schema" *) G.I (str env v1) in
      let ident = (* identifier *) token env v2 in
      let type_params = v3 |> Option.map (map_type_params env) in

      let entity = G.basic_entity ?tparams:type_params (str env v2) in
      G.Anys [ v1; G.En entity ]

let rec transpile_let_bind (env : env) (left : G.pattern) (right : G.expr) :
    G.field list =
  match left with
  | G.PatId (var, _) -> [ G.basic_field var (Some right) None ]
  | G.PatEllipsis tok -> [ G.F (G.Ellipsis tok |> G.e |> G.exprstmt) ]
  | G.PatDisj (G.PatType inner_type, inner) ->
      (* SomeStruct { ... } | right: expr
         =>            ...  | (expr as SomeStruct) *)
      let typed_right = G.Cast (inner_type, sc, right) |> G.e in
      transpile_let_bind env inner typed_right
  | G.PatWildcard _ -> []
  | G.PatRecord (_, fields, _) ->
      let fields =
        fields
        |> List_.map (fun (field, pat) ->
               match pat with
               | PatEllipsis tok ->
                   [ G.F (G.Ellipsis tok |> G.e |> G.exprstmt) ]
               | PatId (var, _) ->
                   let ident = List.nth field 0 in
                   let field_name = G.FN (H2.name_of_id ident) in
                   let vinit =
                     Some (G.DotAccess (right, sc, field_name) |> G.e)
                   in
                   [ G.basic_field var vinit None ]
               | PatWildcard _ -> []
               | _ -> transpile_let_bind env pat right)
      in
      let inner = List_.flatten fields in
      [ G.F (G.Record (sc, inner, sc) |> G.e |> G.exprstmt) ]
  | G.PatTuple (_, elements, _) ->
      elements
      |> List_.mapi (fun idx pat ->
             let idx = G.L (G.Int (Some (Int64.of_int idx), sc)) |> G.e in
             (* (element, ..., _) | expr
                =>        element | expr.idx *)
             let element = G.DotAccess (right, sc, G.FDynamic idx) |> G.e in
             transpile_let_bind env pat element)
      |> List_.flatten
  | G.PatConstructor (name, elements) ->
      elements
      |> List_.mapi (fun idx pat ->
             let idx = G.L (G.Int (Some (Int64.of_int idx), sc)) |> G.e in
             let right = G.Cast (G.TyN name |> G.t, sc, right) |> G.e in
             (* Name(_, element, ..., _) | expr
                =>               element | (expr as Name).idx *)
             let element = G.DotAccess (right, sc, G.FDynamic idx) |> G.e in
             transpile_let_bind env pat element)
      |> List_.flatten
  | G.PatWhen (pat, cond) ->
      (* when(cond, pat) | expr
         =>          pat | if(cond) expr *)
      let if_stmt =
        G.If (sc, G.Cond cond, right |> G.exprstmt, None)
        |> G.s |> G.stmt_to_expr
      in
      transpile_let_bind env pat right
  | _ -> failwith "Unsupported pattern in let binding"

let map_function_signature (env : env) attrs
    ((v1, v2, v3, v4, v5, v6, v7) : CST.function_signature) =
  let is_inline =
    v1
    |> Option.map (fun x -> (* "inline" *) G.attr G.Inline (token env x))
    |> Option.to_list
  in

  let fun_ = (* "fun" *) token env v2 in
  let name = str env v3 in
  let type_params = Option.map (map_type_params env) v4 in
  let params = map_parameters env v5 in
  let ret_type =
    v6
    |> Option.map (fun (_, v2) -> map_type__ env v2)
    |> Option.value ~default:(G.TyTuple (sc, [], sc) |> G.t)
  in
  let specifier =
    Option.map (map_specifier env) v7 |> Option.value ~default:[]
  in
  let fn_def =
    {
      fkind = (G.Function, fun_);
      fparams = params;
      frettype = Some ret_type;
      fbody = G.FBNothing;
    }
  in
  let fn_ent =
    {
      name = G.EN (H2.name_of_id name);
      attrs = attrs @ is_inline @ specifier;
      tparams = type_params;
    }
  in
  (fn_def, fn_ent)

let rec map_aborts_if (env : env) ((v1, v2, v3, v4) : CST.aborts_if) =
  let v1 = (* "aborts_if" *) G.I (str env v1) in
  let props =
    match v2 with
    | Some x -> [ G.At (map_condition_props env x) ]
    | None -> []
  in
  let v3 = G.E (map_expr env v3) in
  let with_value =
    v4
    |> Option.map (fun (v1, v2) ->
           let v1 = (* "with" *) G.Tk (token env v1) in
           let v2 = G.E (map_expr env v2) in
           G.Anys [ v1; v2 ])
    |> Option.to_list
  in
  G.OtherStmt (G.OS_Todo, (v1 :: props) @ (v3 :: with_value))

and map_aborts_with_or_modifies (env : env)
    ((v1, v2, v3, v4) : CST.aborts_with_or_modifies) =
  let v1 =
    match v1 with
    | `Aborts_with tok -> (* "aborts_with" *) str env tok
    | `Modifs tok -> (* "modifies" *) str env tok
  in
  let props =
    Option.map (map_condition_props env) v2
    |> Option.to_list
    |> List_.map (fun x -> G.At x)
  in
  let first = G.E (map_expr env v3) in
  let rest =
    List_.map
      (fun (v1, v2) ->
        let v1 = (* "," *) token env v1 in
        let v2 = map_expr env v2 in
        G.E v2)
      v4
  in
  G.OtherStmt (G.OS_Todo, (G.I v1 :: props) @ (first :: rest))

and map_anon_choice_blk_f78fea4 (env : env) (x : CST.anon_choice_blk_f78fea4) =
  match x with
  | `Blk x -> Some (map_block env x)
  | `SEMI tok -> (* ";" *) None

and map_expr_list (env : env) ((v1, v2) : CST.anon_expr_rep_COMMA_expr_8e432c6)
    =
  let first = map_expr env v1 in
  let rest =
    v2
    |> List_.map (fun (v1, v2) ->
           let v1 = (* "," *) token env v1 in
           let v2 = map_expr env v2 in
           v2)
  in
  first :: rest

and map_asserts (env : env) ((v1, v2, v3) : CST.asserts) =
  let assert_ =
    match v1 with
    | `Assert tok -> (* "assert" *) token env tok
    | `Assume tok -> (* "assume" *) token env tok
    | `Ensures tok -> (* "ensures" *) token env tok
    | `Requis tok -> (* "requires" *) token env tok
  in
  let props =
    Option.map (map_condition_props env) v2
    |> Option.to_list
    |> List_.map (fun x -> G.OtherArg (("ConditionProps", sc), [ G.At x ]))
  in
  let v3 = G.Arg (map_expr env v3) in
  G.Assert (assert_, (sc, props @ [ v3 ], sc), sc)

and map_assignment (env : env) ((v1, v2, v3) : CST.assignment) =
  let lhs = map_unary_expr env v1 in
  let v2 = (* "=" *) token env v2 in
  let rhs = map_expr env v3 in
  G.Assign (lhs, v2, rhs) |> G.e

and map_bin_op_expr (env : env) (x : CST.bin_op_expr) : G.expr =
  match x with
  | `Op_expr_EQEQGT_op_expr (v1, v2, v3) ->
      let v1 = map_op_expr env v1 in
      let v2 = (* "==>" *) token env v2 in
      let v3 = map_op_expr env v3 in
      (* Only used in specification *)
      (* Boolean implication, v1 ==> v2: !v1 || v2 *)
      let not_v1 = G.opcall (G.Not, v2) [ v1 ] in
      G.opcall (G.Or, v2) [ not_v1; v3 ]
  | `Op_expr_LTEQEQGT_op_expr (v1, v2, v3) ->
      let v1 = map_op_expr env v1 in
      let v2 = (* "<==>" *) token env v2 in
      let v3 = map_op_expr env v3 in
      (* FIXME: Abuse G.PhysEq here *)
      G.opcall (G.PhysEq, v2) [ v1; v3 ]
  | `Op_expr_BARBAR_op_expr (v1, v2, v3) ->
      let v1 = map_op_expr env v1 in
      let v2 = (* "||" *) token env v2 in
      let v3 = map_op_expr env v3 in
      G.opcall (G.Or, v2) [ v1; v3 ]
  | `Op_expr_AMPAMP_op_expr (v1, v2, v3) ->
      let v1 = map_op_expr env v1 in
      let v2 = (* "&&" *) token env v2 in
      let v3 = map_op_expr env v3 in
      G.opcall (G.And, v2) [ v1; v3 ]
  | `Op_expr_EQEQ_op_expr (v1, v2, v3) ->
      let v1 = map_op_expr env v1 in
      let v2 = (* "==" *) token env v2 in
      let v3 = map_op_expr env v3 in
      G.opcall (G.Eq, v2) [ v1; v3 ]
  | `Op_expr_BANGEQ_op_expr (v1, v2, v3) ->
      let v1 = map_op_expr env v1 in
      let v2 = (* "!=" *) token env v2 in
      let v3 = map_op_expr env v3 in
      G.opcall (G.NotEq, v2) [ v1; v3 ]
  | `Op_expr_LT_op_expr (v1, v2, v3) ->
      let v1 = map_op_expr env v1 in
      let v2 = (* "<" *) token env v2 in
      let v3 = map_op_expr env v3 in
      G.opcall (G.Lt, v2) [ v1; v3 ]
  | `Op_expr_GT_op_expr (v1, v2, v3) ->
      let v1 = map_op_expr env v1 in
      let v2 = (* ">" *) token env v2 in
      let v3 = map_op_expr env v3 in
      G.opcall (G.Gt, v2) [ v1; v3 ]
  | `Op_expr_LTEQ_op_expr (v1, v2, v3) ->
      let v1 = map_op_expr env v1 in
      let v2 = (* "<=" *) token env v2 in
      let v3 = map_op_expr env v3 in
      G.opcall (G.LtE, v2) [ v1; v3 ]
  | `Op_expr_GTEQ_op_expr (v1, v2, v3) ->
      let v1 = map_op_expr env v1 in
      let v2 = (* ">=" *) token env v2 in
      let v3 = map_op_expr env v3 in
      G.opcall (G.GtE, v2) [ v1; v3 ]
  | `Op_expr_DOTDOT_op_expr (v1, v2, v3) ->
      let v1 = map_op_expr env v1 in
      let v2 = (* ".." *) token env v2 in
      let v3 = map_op_expr env v3 in
      G.opcall (G.Range, v2) [ v1; v3 ]
  | `Op_expr_BAR_op_expr (v1, v2, v3) ->
      let v1 = map_op_expr env v1 in
      let v2 = (* "|" *) token env v2 in
      let v3 = map_op_expr env v3 in
      G.opcall (G.BitOr, v2) [ v1; v3 ]
  | `Op_expr_HAT_op_expr (v1, v2, v3) ->
      let v1 = map_op_expr env v1 in
      let v2 = (* "^" *) token env v2 in
      let v3 = map_op_expr env v3 in
      G.opcall (G.BitXor, v2) [ v1; v3 ]
  | `Op_expr_AMP_op_expr (v1, v2, v3) ->
      let v1 = map_op_expr env v1 in
      let v2 = (* "&" *) token env v2 in
      let v3 = map_op_expr env v3 in
      G.opcall (G.BitAnd, v2) [ v1; v3 ]
  | `Op_expr_LTLT_op_expr (v1, v2, v3) ->
      let v1 = map_op_expr env v1 in
      let v2 = (* "<<" *) token env v2 in
      let v3 = map_op_expr env v3 in
      G.opcall (G.LSL, v2) [ v1; v3 ]
  | `Op_expr_GTGT_op_expr (v1, v2, v3) ->
      let v1 = map_op_expr env v1 in
      let v2 = (* ">>" *) token env v2 in
      let v3 = map_op_expr env v3 in
      G.opcall (G.LSR, v2) [ v1; v3 ]
  | `Op_expr_PLUS_op_expr (v1, v2, v3) ->
      let v1 = map_op_expr env v1 in
      let v2 = (* "+" *) token env v2 in
      let v3 = map_op_expr env v3 in
      G.opcall (G.Plus, v2) [ v1; v3 ]
  | `Op_expr_DASH_op_expr (v1, v2, v3) ->
      let v1 = map_op_expr env v1 in
      let v2 = (* "-" *) token env v2 in
      let v3 = map_op_expr env v3 in
      G.opcall (G.Minus, v2) [ v1; v3 ]
  | `Op_expr_STAR_op_expr (v1, v2, v3) ->
      let v1 = map_op_expr env v1 in
      let v2 = (* "*" *) token env v2 in
      let v3 = map_op_expr env v3 in
      G.opcall (G.Mult, v2) [ v1; v3 ]
  | `Op_expr_SLASH_op_expr (v1, v2, v3) ->
      let v1 = map_op_expr env v1 in
      let v2 = (* "/" *) token env v2 in
      let v3 = map_op_expr env v3 in
      G.opcall (G.Div, v2) [ v1; v3 ]
  | `Op_expr_PERC_op_expr (v1, v2, v3) ->
      let v1 = map_op_expr env v1 in
      let v2 = (* "%" *) token env v2 in
      let v3 = map_op_expr env v3 in
      G.opcall (G.Mod, v2) [ v1; v3 ]

and map_block (env : env) ((v1, v2, v3, v4, v5) : CST.block) : G.stmt =
  let lb = (* "{" *) token env v1 in
  let uses =
    v2
    |> List_.map (map_use_decl env [])
    |> List_.flatten
    |> List_.map (fun x -> G.DirectiveStmt x |> G.s)
  in
  let stmts = List_.map (map_sequence_item env) v3 in
  let final_expr =
    v4
    |> Option.map (map_expr env)
    |> Option.to_list
    |> List_.map (fun x -> G.ExprStmt (x, sc) |> G.s)
  in
  let rb = (* "}" *) token env v5 in
  G.Block (lb, uses @ stmts @ final_expr, rb) |> G.s

and map_call_args (env : env) ((v1, v2, v3, v4) : CST.call_args) =
  let lb = (* "(" *) token env v1 in
  let args =
    v2
    |> Option.map (map_expr_list env)
    |> Option.value ~default:[]
    |> List_.map (fun arg -> G.Arg arg)
  in
  let _comma = v3 |> Option.map (fun x -> (* "," *) token env x) in
  let rb = (* ")" *) token env v4 in
  (lb, args, rb)

and map_control_body (env : env) (x : CST.control_body) =
  match x with
  | `Blk x -> map_block env x
  | `Expr x -> G.ExprStmt (map_expr env x, sc) |> G.s

and map_deep_ellipsis (env : env) ((v1, v2, v3) : CST.deep_ellipsis) =
  let v1 = (* "<..." *) token env v1 in
  let v2 = map_expr env v2 in
  let v3 = (* "...>" *) token env v3 in
  G.DeepEllipsis (v1, v2, v3) |> G.e

and map_identifier_or_anon_field (env : env)
    ?(type_args : type_arguments option) (x : CST.identifier_or_anon_field) =
  match x with
  | `Id tok ->
      let ident = (* identifier *) str env tok in
      G.FN (H2.add_type_args_opt_to_name (H2.name_of_id ident) type_args)
  | `Anon_field_index tok -> (
      (* pattern \d+ *)
      let pos_id = str env tok in
      let literal = G.L (G.Int (Parsed_int.parse pos_id)) |> G.e in
      match type_args with
      | Some _ ->
          (* although it's impossible for a positional field to have  *)
          G.FN (H2.name_of_ids_with_opt_typeargs [ (pos_id, type_args) ])
      | None -> G.FDynamic literal)

and map_dot_or_index_chain (env : env) (x : CST.dot_or_index_chain) =
  match x with
  | `Access_field (v1, v2, v3) ->
      let expr_ = map_dot_or_index_chain env v1 in
      let dot = (* "." *) token env v2 in
      let field = map_identifier_or_anon_field env v3 in
      G.DotAccess (expr_, dot, field) |> G.e
  | `Rece_call (v1, v2, v3, v4, v5) ->
      let expr_ = map_dot_or_index_chain env v1 in
      let dot = (* "." *) token env v2 in
      let type_args =
        v4
        |> Option.map (fun (v1, v2) ->
               let v1 = (* "::" *) token env v1 in
               let v2 = map_type_args env v2 in
               v2)
      in
      let func_name = map_identifier_or_anon_field ?type_args env v3 in
      let func_expr = G.DotAccess (expr_, dot, func_name) |> G.e in
      let args = map_call_args env v5 in
      G.Call (func_expr, args) |> G.e
  | `Mem_access (v1, v2, v3, v4) ->
      let arr = map_dot_or_index_chain env v1 in
      let v2 = (* "[" *) token env v2 in
      let index = map_expr env v3 in
      let v4 = (* "]" *) token env v4 in
      G.ArrayAccess (arr, (v2, index, v4)) |> G.e
  | `Field_access_ellips_expr x -> map_field_access_ellipsis_expr env x
  | `Term x -> map_term env x

and map_emits (env : env) ((v1, v2, v3, v4, v5, v6) : CST.emits) =
  let emits = (* "emits" *) G.I (str env v1) in
  let condition_props =
    Option.map (map_condition_props env) v2
    |> Option.to_list
    |> List_.map (fun x -> G.At x)
  in
  let expr = G.E (map_expr env v3) in
  let to_ = (* "to" *) G.Tk (token env v4) in
  let target = G.E (map_expr env v5) in
  let condition =
    Option.map
      (fun (v1, v2) ->
        let if_ = (* "if" *) G.Tk (token env v1) in
        let cond = G.E (map_expr env v2) in
        G.Anys [ if_; cond ])
      v6
    |> Option.to_list
  in

  G.OtherStmt
    (G.OS_Todo, (emits :: condition_props) @ [ expr; to_; target ] @ condition)

and map_expr (env : env) (x : CST.expr) =
  match x with
  | `Assign x -> map_assignment env x
  | `Op_expr x -> map_op_expr env x
  | `Quan x -> map_quantifier env x
  | `Lambda_bind_list_expr (v1, v2) ->
      let lp, params, _ = map_lambda_bind_list env v1 in
      let body = map_expr env v2 in
      let func_def =
        {
          fkind = (G.LambdaKind, lp);
          fparams = params;
          frettype = None;
          fbody = G.FBExpr body;
        }
      in
      G.Lambda func_def |> G.e
  | `Ellips tok -> (* "..." *) G.Ellipsis (token env tok) |> G.e
  | `Deep_ellips x -> map_deep_ellipsis env x

and map_expr_field (env : env) (x : CST.expr_field) : G.argument =
  match x with
  | `Id tok ->
      (* identifier (shorthand) *)
      let field = str env tok in
      let rhs = G.N (H2.name_of_id field) |> G.e in
      G.ArgKwd (field, rhs)
  | `Id_COLON_expr (v1, v2, v3) ->
      let field = (* identifier *) str env v1 in
      let _colon = (* ":" *) token env v2 in
      let value = map_expr env v3 in

      G.ArgKwd (field, value)
  | `Ellips tok -> (* "..." *) G.Arg (G.Ellipsis (token env tok) |> G.e)

and map_field_access_ellipsis_expr (env : env)
    ((v1, v2, v3) : CST.field_access_ellipsis_expr) =
  let v1 = map_dot_or_index_chain env v1 in
  let v2 = (* "." *) token env v2 in
  let v3 = (* "..." *) token env v3 in
  G.DotAccessEllipsis (v1, v3) |> G.e

and map_let_expr (env : env) ((v1, v2, v3, v4) : CST.let_expr) : G.expr =
  let let_ = (* "let" *) token env v1 in
  let bind = map_bind_list env v2 in
  let type_hint =
    v3
    |> Option.map (fun (v1, v2) ->
           let v1 = (* ":" *) token env v1 in
           let v2 = map_type__ env v2 in
           v2)
  in
  let value =
    v4
    |> Option.map (fun (v1, v2) ->
           let v1 = (* "=" *) token env v1 in
           let v2 = map_expr env v2 in
           v2)
  in
  match bind with
  | G.PatId (var, _) ->
      let var_def = { G.vinit = value; G.vtype = type_hint; vtok = None } in
      G.DefStmt (G.basic_entity var, G.VarDef var_def) |> G.s |> G.stmt_to_expr
  | G.PatEllipsis _ ->
      let ent = { name = G.EPattern bind; attrs = []; tparams = None } in
      let var_def = { G.vinit = value; G.vtype = type_hint; vtok = None } in
      G.DefStmt (ent, G.VarDef var_def) |> G.s |> G.stmt_to_expr
  | _ ->
      let transpiled =
        transpile_let_bind env bind
          (Option.value ~default:(G.L (G.Null sc) |> G.e) value)
      in
      G.Record (sc, transpiled, sc) |> G.e

and map_name_expr (env : env) (x : CST.name_expr) : G.expr =
  match x with
  | `Var (v1, v2) ->
      let name_chain = map_name_access_chain env v1 in
      let type_args = Option.map (map_type_args env) v2 in
      let typed_name = H2.add_type_args_opt_to_name name_chain type_args in
      G.N typed_name |> G.e
  | `Call_expr (v1, v2, v3) ->
      let name_chain = map_name_access_chain env v1 in
      let type_args = Option.map (map_type_args env) v2 in
      let call_args = map_call_args env v3 in
      let typed_func = H2.add_type_args_opt_to_name name_chain type_args in
      G.Call (G.N typed_func |> G.e, call_args) |> G.e
  | `Pack_expr (v1, v2, v3, v4, v5, v6) ->
      let name_chain = map_name_access_chain env v1 in
      let type_args = Option.map (map_type_args env) v2 in
      let lb = (* "{" *) token env v3 in
      let fields =
        v4
        |> Option.map (fun (v1, v2) ->
               let first = map_expr_field env v1 in
               let rest =
                 List_.map
                   (fun (v1, v2) ->
                     let v1 = (* "," *) token env v1 in
                     let v2 = map_expr_field env v2 in
                     v2)
                   v2
               in
               first :: rest)
        |> Option.value ~default:[]
      in
      let _comma = Option.map (fun x -> (* "," *) token env x) v5 in
      let rb = (* "}" *) token env v6 in
      let struct_type =
        G.TyN (H2.add_type_args_opt_to_name name_chain type_args) |> G.t
      in
      (* We mimic Go's way of translating struct composition *)
      (* Rust's `Constructor` approach does not support ellipsis *)
      G.New (G.fake "new", struct_type, G.empty_id_info (), (lb, fields, rb))
      |> G.e
  | `Macro_call_expr (v1, v2, v3) ->
      let name = map_name_access_chain env v1 in
      let bang = (* "!" *) token env v2 in

      let name =
        match name with
        | G.Id ((s, i1), info) ->
            G.Id ((s ^ "!", Tok.combine_toks i1 [ bang ]), info)
        | G.IdQualified ({ name_last = (s, i1), topt; _ } as qualified_info) ->
            let s, t = (s ^ "!", Tok.combine_toks i1 [ bang ]) in
            G.IdQualified { qualified_info with name_last = ((s, t), topt) }
      in
      let args = map_call_args env v3 in
      G.Call (G.N name |> G.e, args) |> G.e

and map_op_expr (env : env) (x : CST.op_expr) : G.expr =
  match x with
  | `Un_expr x -> map_unary_expr env x
  | `Bin_op_expr x -> map_bin_op_expr env x

and map_parenthesized_expr (env : env) ((v1, v2, v3) : CST.parenthesized_expr) :
    G.expr =
  let v1 = (* "(" *) token env v1 in
  let v2 = map_expr env v2 in
  let v3 = (* ")" *) token env v3 in
  v2

and map_quantifier (env : env) (x : CST.quantifier) =
  match x with
  | `Choice_forall_quan_bind_rep_COMMA_quan_bind_opt_triggs_opt_where_expr_COLON_expr
      (v1, v2, v3, v4, v5, v6, v7) ->
      let quant =
        match v1 with
        | `Forall tok -> (* "forall" *) token env tok
        | `Exists tok -> (* "exists" *) token env tok
      in
      let bind_first = G.P (map_quantifier_bind env v2) in
      let bind_rest =
        List_.map
          (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_quantifier_bind env v2 in
            G.P v2)
          v3
      in

      let binds = bind_first :: bind_rest in
      let triggers =
        Option.map (map_triggers env) v4
        |> Option.value ~default:[]
        |> List_.map (fun x -> G.S x)
      in
      let constraint_ =
        Option.map
          (fun (v1, v2) ->
            let v1 = (* "where" *) token env v1 in
            let v2 = map_expr env v2 in
            G.E v2)
          v5
        |> Option.to_list
      in
      let v6 = (* ":" *) token env v6 in
      let invariant = [ E (map_expr env v7) ] in

      G.OtherPat
        (("Quantifier", quant), binds @ triggers @ constraint_ @ invariant)
      |> H2.pattern_to_expr
  | `Choose_opt_min_quan_bind_where_expr (v1, v2, v3, v4, v5) ->
      let choose = (* "choose" *) token env v1 in
      let min_opt =
        Option.map (fun x -> G.Tk (token env x)) v2 |> Option.to_list
      in
      let v3 = G.P (map_quantifier_bind env v3) in
      let v4 = (* "where" *) G.Tk (token env v4) in
      let v5 = G.E (map_expr env v5) in
      G.OtherPat (("QuantifierChoose", choose), min_opt @ [ v4; v5 ])
      |> H2.pattern_to_expr

and map_quantifier_bind (env : env) (x : CST.quantifier_bind) =
  match x with
  | `Id_COLON_type_ (v1, v2, v3) ->
      let ident = (* identifier *) str env v1 in
      let _colon = (* ":" *) token env v2 in
      let bind_type = map_type__ env v3 in
      G.PatTyped (G.PatId (ident, G.empty_id_info ()), bind_type)
  | `Id_in_expr (v1, v2, v3) ->
      let ident = (* identifier *) str env v1 in
      let v2 = (* "in" *) token env v2 in
      let expr = map_expr env v3 in
      (* TODO: find a better way to place it *)
      G.PatWhen (G.PatId (ident, G.empty_id_info ()), expr)

and map_return_expr (env : env) (x : CST.return_expr) =
  let st =
    match x with
    | `Ret tok -> (* "return" *) G.Return (token env tok, None, sc) |> G.s
    | `Ret_expr (v1, v2) ->
        let v1 = (* "return" *) token env v1 in
        let v2 = map_expr env v2 in
        G.Return (v1, Some v2, sc) |> G.s
  in
  G.StmtExpr st

and map_sequence_item (env : env) (x : CST.sequence_item) =
  match x with
  | `Choice_expr_SEMI (v1, v2) -> (
      let expr =
        match v1 with
        | `Expr x -> map_expr env x
        | `Let_expr x -> map_let_expr env x
      in
      let sc = (* ";" *) token env v2 in

      (* Simplify nested statements *)
      match expr with
      | { G.e = G.StmtExpr stmt; _ } -> stmt
      | expr -> G.ExprStmt (expr, sc) |> G.s)
  | `Ellips tok -> G.exprstmt (G.Ellipsis (token env tok) |> G.e)

and map_spec_apply (env : env) ((v1, v2, v3, v4, v5, v6, v7) : CST.spec_apply) =
  let v1 = (* "apply" *) token env v1 in
  let v2 = G.E (map_expr env v2) in
  let v3 = (* "to" *) G.Tk (token env v3) in
  let v4 =
    v4
    |> Option.map (fun x ->
           map_anon_spec_apply_pat_rep_COMMA_spec_apply_pat_d9a21d6 env x)
    |> Option.value ~default:[]
    |> List_.map (fun x -> G.P x)
  in
  let v5 = Option.map (fun x -> (* "," *) token env x) v5 in
  let v6 =
    match v6 with
    | Some (v1, v2, v3) ->
        let v1 = (* "except" *) G.Tk (token env v1) in
        let patterns =
          Option.map
            (map_anon_spec_apply_pat_rep_COMMA_spec_apply_pat_d9a21d6 env)
            v2
          |> Option.value ~default:[]
          |> List_.map (fun x -> G.P x)
        in
        let v3 = Option.map (fun x -> (* "," *) token env x) v3 in
        [ G.Anys (v1 :: patterns) ]
    | None -> []
  in
  let v7 = (* ";" *) token env v7 in
  G.OtherPat (("SpecApply", v1), [ v2; v3 ] @ v4 @ v6)
  |> H2.pattern_to_expr |> G.exprstmt

and map_spec_axiom (env : env) ((v1, v2, v3, v4, v5) : CST.spec_axiom) =
  let axiom = (* "axiom" *) token env v1 in
  let type_params = Option.map (map_type_params env) v2 in
  let props =
    Option.map (map_condition_props env) v3
    |> Option.to_list
    |> List_.map (fun x -> G.At x)
  in
  let expr = G.E (map_expr env v4) in
  let v5 = (* ";" *) token env v5 in
  G.OtherPat (("SpecAxiom", axiom), expr :: props)
  |> H2.pattern_to_expr |> G.exprstmt

and map_spec_block (env : env) attrs ((v1, v2) : CST.spec_block) =
  let v1 = (* "spec" *) token env v1 in
  let body =
    match v2 with
    | `Spec_func x -> map_spec_func env x
    | `Opt_spec_blk_target_LCURL_rep_use_decl_rep_spec_blk_member_RCURL
        (v1, v2, v3, v4, v5) ->
        let v1 =
          match v1 with
          | Some x -> Some (map_spec_block_target env x)
          | None -> None
        in
        let v2 = (* "{" *) token env v2 in
        let uses =
          List_.map (map_use_decl env attrs) v3
          |> List_.flatten
          |> List_.map (fun x -> G.DirectiveStmt x |> G.s)
        in
        let members = List_.map (map_spec_block_member env attrs) v4 in
        let v5 = (* "}" *) token env v5 in
        G.Block (v2, uses @ members, v5) |> G.s
  in
  body

and map_spec_block_member (env : env) attrs (x : CST.spec_block_member) =
  (* TODO: Spec *)
  (* let ret =
       match x with
       | `Choice_spec_inva x -> (
           match x with
           | `Spec_inva x -> map_spec_invariant env x
           | `Spec_cond x -> map_spec_condition env x
           | `Spec_func x -> map_spec_func env x
           | `Spec_var x -> map_spec_variable env x
           | `Spec_incl x -> map_spec_include env x
           | `Spec_apply x -> map_spec_apply env x
           | `Spec_pragma x -> map_spec_pragma env x
           | `Spec_let x -> map_spec_let env x
           | `Spec_update x -> map_spec_update env x
           | `Spec_axiom x -> map_spec_axiom env x)
       | `Ellips tok -> (* "..." *) G.Ellipsis (token env tok) |> G.e |> G.exprstmt
     in *)
  G.emptystmt sc

and map_spec_condition (env : env) ((v1, v2) : CST.spec_condition) =
  let v1 =
    match v1 with
    | `Asserts x -> map_asserts env x
    | `Aborts_if x -> map_aborts_if env x
    | `Aborts_with_or_modifs x -> map_aborts_with_or_modifies env x
    | `Emits x -> map_emits env x
  in
  let v2 = (* ";" *) token env v2 in
  v1 |> G.s

and map_spec_func (env : env) (x : CST.spec_func) =
  match x with
  | `Fun_spec_func_signas_choice_blk (v1, v2, v3) ->
      let v1 = (* "fun" *) token env v1 in
      let entity, params, ret_type = map_spec_func_signatures env v2 in
      let body = map_anon_choice_blk_f78fea4 env v3 in

      let func_def =
        {
          fkind = (G.Function, v1);
          fparams = params;
          frettype = Some ret_type;
          fbody =
            (match body with
            | None -> G.FBNothing
            | Some x -> G.FBStmt x);
        }
      in
      G.DefStmt (entity, G.FuncDef func_def) |> G.s
  | `Native_fun_spec_func_signas_SEMI (v1, v2, v3, v4) ->
      let native = (* "native" *) token env v1 in
      let attr = G.KeywordAttr (G.Extern, native) in
      let fun_ = (* "fun" *) token env v2 in
      let entity, params, ret_type = map_spec_func_signatures env v3 in
      let func_def =
        {
          fkind = (G.Function, fun_);
          fparams = params;
          frettype = Some ret_type;
          fbody = G.FBNothing;
        }
      in
      let v4 = (* ";" *) token env v4 in
      G.DefStmt (entity, G.FuncDef func_def) |> G.s

and map_spec_include (env : env) ((v1, v2, v3, v4) : CST.spec_include) =
  let v1 = (* "include" *) G.I (str env v1) in
  let v2 =
    Option.map (map_condition_props env) v2
    |> Option.to_list
    |> List_.map (fun x -> G.At x)
  in
  let v3 = G.E (map_expr env v3) in
  let v4 = (* ";" *) token env v4 in
  G.OtherStmt (G.OS_Todo, (v1 :: v2) @ [ v3 ]) |> G.s

and map_spec_invariant (env : env)
    ((v1, v2, v3, v4, v5, v6) : CST.spec_invariant) =
  let v1 = (* "invariant" *) G.I (str env v1) in
  let type_params =
    Option.map (map_type_params env) v2
    |> Option.map (fun x ->
           let _, y, _ = x in
           y)
    |> Option.value ~default:[]
    |> List_.map (fun x -> G.Tp x)
  in
  let update =
    Option.map (token env) v3 |> Option.to_list |> List_.map (fun x -> G.Tk x)
  in
  let props =
    Option.map (map_condition_props env) v4
    |> Option.to_list
    |> List_.map (fun x -> G.At x)
  in
  let v5 = G.E (map_expr env v5) in
  let v6 = (* ";" *) token env v6 in
  G.OtherStmt (G.OS_Todo, [ v1 ] @ type_params @ update @ props @ [ v5 ]) |> G.s

and map_spec_let (env : env) ((v1, v2, v3, v4, v5, v6) : CST.spec_let) =
  let let_ = (* "let" *) token env v1 in
  let post_TODO = Option.map ((* "post" *) token env) v2 in
  let var_name = map_var_name env v3 in
  let v4 = (* "=" *) token env v4 in
  let value = map_expr env v5 in
  let v6 = (* ";" *) token env v6 in
  let def = G.VarDef { vinit = Some value; vtype = None; vtok = Some let_ } in
  let entity =
    { name = G.EN (H2.name_of_id var_name); attrs = []; tparams = None }
  in
  G.DefStmt (entity, def) |> G.s

and map_spec_loop_invariant (env : env) attrs (x : CST.spec_loop_invariant) =
  map_spec_block env attrs x

and map_spec_update (env : env) ((v1, v2, v3) : CST.spec_update) =
  let v1 = (* "update" *) G.I (str env v1) in
  let v2 = G.E (map_assignment env v2) in
  let v3 = (* ";" *) token env v3 in
  G.OtherStmt (G.OS_Todo, [ v1; v2 ]) |> G.s

and map_spec_variable (env : env)
    ((v1, v2, v3, v4, v5, v6, v7) : CST.spec_variable) =
  let attr =
    match v1 with
    | Some x -> (
        match x with
        | `Global tok -> [ G.KeywordAttr (G.Public, token env tok) ]
        | `Local tok -> [ G.KeywordAttr (G.Private, token env tok) ])
    | None -> []
  in
  let ident = (* identifier *) str env v2 in
  let type_params = Option.map (map_type_params env) v3 in
  let v4 = (* ":" *) token env v4 in
  let type_ = map_type__ env v5 in

  let entity = G.basic_entity ~attrs:attr ?tparams:type_params ident in
  let value =
    Option.map
      (fun (v1, v2) ->
        let v1 = (* "=" *) token env v1 in
        let v2 = map_expr env v2 in
        v2)
      v6
  in
  let v7 = (* ";" *) token env v7 in
  G.DefStmt (entity, G.VarDef { vinit = value; vtype = Some type_; vtok = None })
  |> G.s

and map_match_arm (env : env) (x : CST.match_arm) : G.case_and_body =
  match x with
  | `Bind_list_opt_if_expr_EQGT_cont_body (v1, v2, v3, v4) -> (
      let pat = map_bind_list env v1 in
      let cond =
        v2
        |> Option.map (fun (v1, v2) ->
               let v1 = (* "if" *) token env v1 in
               G.PatWhen (pat, map_expr env v2))
        |> Option.value ~default:pat
      in

      let v3 = (* "=>" *) token env v3 in
      let body = map_control_body env v4 in

      match pat with
      | G.PatType _
      | G.PatId _
      | G.PatEllipsis _
      | G.PatWildcard _ ->
          G.CasesAndBody ([ G.Case (v3, cond) ], body)
      | _ ->
          let transpiled =
            transpile_let_bind env cond (G.L (G.Null sc) |> G.e)
          in
          G.CasesAndBody
            ( [ G.CaseEqualExpr (v3, G.Record (sc, transpiled, sc) |> G.e) ],
              body ))
  | `Ellips tok -> (* "..." *) G.CaseEllipsis (token env tok)

and map_for_loop_expr (env : env) (x : CST.for_loop_expr) =
  match x with
  | `For_LPAR_var_name_in_un_expr_DOTDOT_un_expr_opt_spec_loop_inva_RPAR_blk
      (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10) ->
      let for_ = (* "for" *) token env v1 in
      let v2 = (* "(" *) token env v2 in
      let index = G.PatId (map_var_name env v3, G.empty_id_info ()) in
      let in_ = (* "in" *) token env v4 in
      let first = map_unary_expr env v5 in
      let v6 = (* ".." *) token env v6 in
      let last = map_unary_expr env v7 in

      let range = G.opcall (G.Range, v6) [ first; last ] in

      let _loop_invariant_TODO =
        Option.map (map_spec_loop_invariant env []) v8
      in
      let v9 = (* ")" *) token env v9 in
      let body = map_block env v10 in

      let for_header = G.ForEach (index, in_, range) in
      let for_stmt = G.For (for_, for_header, body) |> G.s in
      G.stmt_to_expr for_stmt
  | `For_LPAR_ellips_RPAR_blk (v1, v2, v3, v4, v5) ->
      let v1 = (* "for" *) token env v1 in
      let v2 = (* "(" *) token env v2 in
      let v3 = (* "..." *) token env v3 in
      let v4 = (* ")" *) token env v4 in
      let v5 = map_block env v5 in

      let for_header = G.ForEllipsis v3 in
      let for_stmt = G.For (v1, for_header, v5) |> G.s in
      G.stmt_to_expr for_stmt

and map_term (env : env) (x : CST.term) : G.expr =
  match x with
  | `Brk tok ->
      (* "break" *)
      G.Break (token env tok, G.LNone, sc) |> G.s |> G.stmt_to_expr
  | `Cont tok ->
      (* "continue" *)
      G.Continue (token env tok, G.LNone, sc) |> G.s |> G.stmt_to_expr
  | `Vec_value_expr (v1, v2, v3, v4, v5, v6) ->
      let vector_ = (* "vector" *) token env v1 in
      let _type_args = v2 |> Option.map (map_type_args env) in
      let lb = (* "[" *) token env v3 in
      let values =
        Option.map (map_expr_list env) v4 |> Option.value ~default:[]
      in
      let _comma = v5 |> Option.map (fun x -> (* "," *) token env x) in
      let rb = (* "]" *) token env v6 in
      G.Container (G.Array, (lb, values, rb)) |> G.e
  | `Value x -> map_value env x |> G.e
  | `Tuple_expr (v1, v2, v3, v4) -> (
      let lp = (* "(" *) token env v1 in
      let tuple_list =
        Option.map (map_expr_list env) v2 |> Option.value ~default:[]
      in
      let _comma = v3 |> Option.map (fun x -> (* "," *) token env x) in
      let rp = (* ")" *) token env v4 in

      match tuple_list with
      | [] -> G.L (G.Unit lp) |> G.e
      | _ -> G.Container (G.Tuple, (lp, tuple_list, rp)) |> G.e)
  | `Type_hint_expr (v1, v2, v3, v4, v5) ->
      let v1 = (* "(" *) token env v1 in
      let v2 = map_expr env v2 in
      let v3 = (* ":" *) token env v3 in
      let v4 = map_type__ env v4 in
      let v5 = (* ")" *) token env v5 in
      G.Cast (v4, v3, v2) |> G.e
  | `Cast_expr (v1, v2, v3, v4, v5) ->
      let v1 = (* "(" *) token env v1 in
      let v2 = map_expr env v2 in
      let v3 = (* "as" *) token env v3 in
      let v4 = map_type__ env v4 in
      let v5 = (* ")" *) token env v5 in
      G.Cast (v4, v3, v2) |> G.e
  | `Blk x -> map_block env x |> G.stmt_to_expr
  | `Choice_var x -> map_name_expr env x
  (* TODO: Spec *)
  (* | `Spec_blk x -> map_spec_loop_invariant env [] x |> G.stmt_to_expr *)
  | `Spec_blk x -> G.emptystmt sc |> G.stmt_to_expr
  | `If_expr (v1, v2, v3, v4) ->
      let _if = (* "if" *) token env v1 in
      let cond = map_parenthesized_expr env v2 in
      let then_ = map_control_body env v3 in
      let else_ =
        Option.map
          (fun (v1, v2) ->
            let _else_ = (* "else" *) token env v1 in
            let v2 = map_control_body env v2 in
            v2)
          v4
      in
      let if_stmt = G.If (_if, G.Cond cond, then_, else_) |> G.s in
      G.stmt_to_expr if_stmt
  | `While_expr (v1, v2, v3, v4) ->
      let while_ = (* "while" *) token env v1 in
      let cond = map_parenthesized_expr env v2 in
      let body = map_control_body env v3 in
      let _loop_invariant_TODO =
        Option.map (map_spec_loop_invariant env []) v4
      in
      let while_stmt = G.While (while_, G.Cond cond, body) |> G.s in
      G.stmt_to_expr while_stmt
  | `Loop_expr (v1, v2) ->
      let loop_ = (* "loop" *) token env v1 in
      let body = map_control_body env v2 in
      (* fake condition *)
      let cond = G.L (G.Bool (true, G.fake "true")) |> G.e in
      G.stmt_to_expr (G.While (loop_, G.Cond cond, body) |> G.s)
  | `Match_expr (v1, v2, v3, v4, v5, v6, v7) ->
      let match_ = (* "match" *) token env v1 in
      let v2 = (* "(" *) token env v2 in
      let cond = G.Cond (map_expr env v3) in
      let v4 = (* ")" *) token env v4 in
      let v5 = (* "{" *) token env v5 in
      let arms =
        List_.map
          (fun (v1, v2) ->
            let arm = map_match_arm env v1 in
            let v2 = v2 |> Option.map (fun x -> (* "," *) token env x) in
            arm)
          v6
      in
      let v7 = (* "}" *) token env v7 in
      G.Switch (match_, Some cond, arms) |> G.s |> G.stmt_to_expr
  | `Ret_expr x ->
      let expr_stmt = G.ExprStmt (map_return_expr env x |> G.e, sc) |> G.s in
      G.stmt_to_expr expr_stmt
  | `Abort_expr (v1, v2) ->
      let v1 = (* "abort" *) str env v1 in
      let v2 = map_expr env v2 in
      (* translate into a call *)
      G.Call (G.N (H2.name_of_id v1) |> G.e, fb [ G.Arg v2 ]) |> G.e
  | `For_loop_expr x -> map_for_loop_expr env x
  | `Ellips x -> G.Ellipsis (token env x) |> G.e
  | `Deep_ellips x -> map_deep_ellipsis env x

and map_triggers (env : env) (xs : CST.triggers) =
  (* TODO: Spec *)
  List_.map
    (fun (v1, v2, v3, v4) ->
      let v1 = (* "{" *) token env v1 in
      let v2 =
        Option.map (map_expr_list env) v2
        |> Option.value ~default:[] |> List_.map G.exprstmt
      in
      let v3 = Option.map (fun x -> (* "," *) token env x) v3 in
      let v4 = (* "}" *) token env v4 in
      G.Block (v1, v2, v4) |> G.s)
    xs

and map_unary_expr (env : env) (x : CST.unary_expr) : G.expr =
  match x with
  | `Not_expr (v1, v2) ->
      let v1 = (* "!" *) token env v1 in
      let v2 = map_unary_expr env v2 in
      G.opcall (G.Not, v1) [ v2 ]
  | `Ref_expr (v1, v2) ->
      let v1 = (* "&" *) token env v1 in
      let v2 = map_unary_expr env v2 in
      G.Ref (v1, v2) |> G.e
  | `Ref_mut_expr (v1, v2) ->
      (* TODO: mutability *)
      let v1 = (* "&mut" *) token env v1 in
      let v2 = map_unary_expr env v2 in
      G.Ref (v1, v2) |> G.e
  | `Deref_expr (v1, v2) ->
      let v1 = (* "*" *) token env v1 in
      let v2 = map_unary_expr env v2 in
      G.DeRef (v1, v2) |> G.e
  | `Move_expr (v1, v2) ->
      let v1 = (* "move" *) str env v1 in
      let v2 = (* identifier *) str env v2 in
      G.Call
        ( G.N (H2.name_of_id v1) |> G.e,
          fb [ G.Arg (G.N (H2.name_of_id v2) |> G.e) ] )
      |> G.e
  | `Copy_expr (v1, v2) ->
      let v1 = (* "copy" *) str env v1 in
      let v2 = (* identifier *) str env v2 in
      G.Call
        ( G.N (H2.name_of_id v1) |> G.e,
          fb [ G.Arg (G.N (H2.name_of_id v2) |> G.e) ] )
      |> G.e
  | `Choice_access_field x -> map_dot_or_index_chain env x
  | `Ellips tok -> (* "..." *) G.Ellipsis (token env tok) |> G.e
  | `Deep_ellips x -> map_deep_ellipsis env x
  | `Field_access_ellips_expr x -> map_field_access_ellipsis_expr env x
  | `Typed_meta (v1, v2, v3, v4, v5) ->
      let v1 = (* "(" *) token env v1 in
      let ident = (* identifier *) str env v2 in
      let _colon = (* ":" *) token env v3 in
      let var_type = map_type__ env v4 in
      let v5 = (* ")" *) token env v5 in

      (* Typed metavariables and cast expressions are ambiguous in the grammar *)
      let expr =
        if AST_generic.is_metavar_name (H2.str_of_ident ident) then
          G.TypedMetavar (ident, _colon, var_type)
        else G.Cast (var_type, _colon, G.N (H2.name_of_id ident) |> G.e)
      in
      expr |> G.e

let map_constant_decl (env : env) attrs
    ((v1, v2, v3, v4, v5, v6, v7) : CST.constant_decl) =
  let tconst = (* "const" *) token env v1 in
  let ident = (* identifier *) str env v2 in
  let v3 = (* ":" *) token env v3 in
  let const_type = map_type__ env v4 in
  let v5 = (* "=" *) token env v5 in
  let value = map_expr env v6 in
  let sc = (* ";" *) token env v7 in

  let var_def =
    { G.vinit = Some value; G.vtype = Some const_type; vtok = Some sc }
  in
  let attrs = attrs @ [ G.attr G.Const tconst ] in
  let ent = G.basic_entity ~attrs ident in
  G.DefStmt (ent, G.VarDef var_def) |> G.s

let map_script_spec_block (env : env) ext_attrs (x : CST.script_spec_block) =
  match x with
  | `Opt_attris_spec_blk (v1, v2) ->
      (* TODO: Spec *)
      (* let attrs =
           Option.map (map_attributes env) v1 |> Option.value ~default:[]
         in
         let invariants = map_spec_loop_invariant env attrs v2 in
         invariants *)
      G.emptystmt sc
  | `Ellips tok -> G.Ellipsis (token env tok) |> G.e |> G.exprstmt

let map_function_decl (env : env) attrs ((v1, v2) : CST.function_decl) =
  let fn_def, fn_ent = map_function_signature env attrs v1 in
  let body = map_anon_choice_blk_f78fea4 env v2 in

  let fn_def =
    match body with
    | Some body -> { fn_def with G.fbody = G.FBStmt body }
    | _ -> fn_def
  in
  G.DefStmt (fn_ent, G.FuncDef fn_def) |> G.s

let map_script_constant_decl (env : env) ext_attrs
    (x : CST.script_constant_decl) =
  match x with
  | `Opt_attris_cst_decl (v1, v2) ->
      let attrs =
        Option.map (map_attributes env) v1 |> Option.value ~default:[]
      in
      map_constant_decl env (ext_attrs @ attrs) v2
  | `Ellips tok -> G.Ellipsis (token env tok) |> G.e |> G.exprstmt

let map_declaration (env : env) (x : CST.declaration) : G.stmt list =
  match x with
  | `Opt_attris_choice_use_decl (v1, v2) -> (
      let attrs =
        Option.map (map_attributes env) v1 |> Option.value ~default:[]
      in
      (* TODO: attach attributes to the declerations *)
      match v2 with
      | `Use_decl x ->
          map_use_decl env attrs x
          |> List_.map (fun x -> G.DirectiveStmt x |> G.s)
      | `Friend_decl x ->
          [ map_friend_decl env x |> (fun x -> G.DirectiveStmt x) |> G.s ]
      | `Spec_spec_func (v1, v2) ->
          (* let _ = (* "spec" *) token env v1 in
             let v2 = map_spec_func env v2 in *)
          (* TODO: Spec *)
          []
      | `Spec_blk x -> [ (* map_spec_loop_invariant env attrs x *) ]
      | `Spec_inva x -> [ (* map_spec_invariant env x *) ]
      | `Rep_module_member_modi_choice_cst_decl (v1, v2) ->
          let modifiers = List_.map (map_module_member_modifier env) v1 in
          let attrs = modifiers @ attrs in
          let def =
            match v2 with
            | `Cst_decl x -> map_constant_decl env attrs x
            | `Struct_or_enum_decl x -> (
                match x with
                | `Struct_decl x -> map_struct_decl env attrs x
                | `Enum_decl x -> map_enum_decl env attrs x)
            | `Func_decl x -> map_function_decl env attrs x
          in
          [ def ])
  | `Ellips tok -> (* "..." *) [ G.exprstmt (G.e (G.Ellipsis (token env tok))) ]

let map_script_func_decl (env : env) ext_attrs (x : CST.script_func_decl) =
  match x with
  | `Opt_attris_rep_module_member_modi_func_decl (v1, v2, v3) ->
      let attrs =
        Option.map (map_attributes env) v1 |> Option.value ~default:[]
      in
      let modifiers = List_.map (map_module_member_modifier env) v2 in
      map_function_decl env (ext_attrs @ attrs @ modifiers) v3
  | `Ellips tok -> G.Ellipsis (token env tok) |> G.e |> G.exprstmt

let map_module_path (env : env) ((v1, v2) : CST.module_path) =
  let v1 =
    v1
    |> Option.map (fun (v1, v2) ->
           let v1 = map_leading_name_access env v1 in
           let v2 = (* "::" *) token env v2 in
           v1)
  in
  let v2 = (* identifier *) str env v2 in
  (v1, v2)

let map_module_ (env : env) attrs (addr : ident option)
    ((v1, v2, v3, v4, v5) : CST.module_) =
  let v1 =
    match v1 with
    | `Spec tok -> (* "spec" *) token env tok
    | `Module_kw tok -> (* "module" *) token env tok
  in
  let prefix, name = map_module_path env v2 in
  let prefix = [ addr; prefix ] |> List_.filter_map Fun.id in
  let _ = (* "{" *) token env v3 in
  let body = List_.map (map_declaration env) v4 |> List_.flatten in
  let _ = (* "}" *) token env v5 in

  (* let module_entity = G.basic_entity ~attrs name in *)
  let module_entity =
    match prefix with
    | [] -> G.basic_entity ~attrs name
    | _ ->
        {
          name = G.EN (H2.name_of_ids (prefix @ [ name ]));
          attrs;
          tparams = None;
        }
  in
  let def = G.ModuleDef { mbody = G.ModuleStruct (None, body) } in
  G.DefStmt (module_entity, def) |> G.s

let map_script (env : env) attrs ((v1, v2, v3, v4, v5, v6, v7) : CST.script) =
  let v1 = (* "script" *) str env v1 in
  let v2 = (* "{" *) token env v2 in
  let uses = v3 |> List_.map (map_script_use_decl env attrs) |> List_.flatten in
  let consts = List_.map (map_script_constant_decl env attrs) v4 in
  let func = map_script_func_decl env attrs v5 in
  let spec = List_.map (map_script_spec_block env attrs) v6 in
  let body = uses @ consts @ [ func ] @ spec in
  let v7 = (* "}" *) token env v7 in

  (* let def = G.ModuleDef { mbody = G.ModuleStruct (Some [ v1 ], body) } in
     G.DefStmt (G.basic_entity ~attrs v1, def) |> G.s *)
  G.OtherStmtWithStmt (G.OSWS_Block v1, [], G.Block (v2, body, v7) |> G.s)
  |> G.s

let map_address_member (env : env) addr (x : CST.address_member) =
  match x with
  | `Opt_attris_module (v1, v2) ->
      let attrs =
        v1 |> Option.map (map_attributes env) |> Option.value ~default:[]
      in
      map_module_ env attrs (Some addr) v2
  | `Ellips tok -> (* "..." *) G.Ellipsis (token env tok) |> G.e |> G.exprstmt

let map_address_block (env : env) ((v1, v2, v3, v4, v5) : CST.address_block) =
  let address_ = (* "address" *) str env v1 in
  let addr = map_leading_name_access env v2 in
  let lb = (* "{" *) token env v3 in

  let modules = v4 |> List_.map (map_address_member env addr) in
  let rb = (* "}" *) token env v5 in
  G.OtherStmtWithStmt
    ( G.OSWS_Block address_,
      [ G.Name (H2.name_of_id addr) ],
      G.Block (lb, modules, rb) |> G.s )
  |> G.s

let map_semgrep_expression (env : env) (x : CST.semgrep_expression) =
  G.E
    (match x with
    | `Expr x -> map_expr env x
    | `Let_expr (v1, v2, v3, v4) -> map_let_expr env (v1, v2, v3, v4))

let map_semgrep_statement (env : env) (xs : CST.semgrep_statement) =
  let stmts =
    xs
    |> List_.map (fun x ->
           match x with
           | `Seq_item x -> [ map_sequence_item env x ]
           | `Decl x -> map_declaration env x)
    |> List_.flatten
  in
  G.Ss stmts

let map_source_file (env : env) (x : CST.source_file) =
  match x with
  | `Rep_opt_attris_choice_module xs ->
      G.Pr
        (xs
        |> List_.map (fun (v1, v2) ->
               let attrs =
                 Option.map (map_attributes env) v1 |> Option.value ~default:[]
               in
               match v2 with
               | `Module x -> map_module_ env attrs None x
               | `Script x -> map_script env attrs x
               | `Addr_blk x -> map_address_block env x))
  | `Semg_exp x -> map_semgrep_expression env x
  | `Semg_stmt x -> map_semgrep_statement env x
  | `Semg_part (v1, v2, v3) -> (
      let modifiers = List_.map (map_module_member_modifier env) v2 in
      let attrs =
        v1
        |> Option.map (map_attributes env)
        |> Option.value ~default:[] |> List.append modifiers
      in
      match v3 with
      | `Func_sign x ->
          let fn_def, ent = map_function_signature env attrs x in
          G.Partial (G.PartialDef (ent, G.FuncDef fn_def))
      | `Struct_sign x ->
          let struct_, abilities, ent = map_struct_signature env attrs x in
          let struct_def =
            {
              ckind = (G.Class, struct_);
              cextends = [];
              cimplements = abilities;
              cmixins = [];
              cparams = fb [];
              cbody = (sc, [], sc);
            }
          in
          G.Partial (G.PartialDef (ent, G.ClassDef struct_def))
      | `Enum_sign x ->
          let enum_, abilities, ent = map_enum_signature env attrs x in
          let enum_def = { tbody = G.OrType [ G.OrEllipsis sc ] } in
          (* inject abilities into entity *)
          let ent = inject_enum_abilities env abilities ent in
          (* Workaround: Def, Partial Def don't work *)
          G.S (G.DefStmt (ent, G.TypeDef enum_def) |> G.s))

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)
let parse file =
  H.wrap_parser
    (fun () -> Tree_sitter_move_on_aptos.Parse.file !!file)
    (fun cst _extras ->
      let env = { H.file; conv = H.line_col_to_pos file; extra = Target } in
      match map_source_file env cst with
      | G.Pr xs -> xs
      | _ -> failwith "not a program")

let parse_expression_or_source_file str =
  Tree_sitter_move_on_aptos.Parse.string str

(* todo: special mode to convert Ellipsis in the right construct! *)
let parse_pattern str =
  H.wrap_parser
    (fun () -> parse_expression_or_source_file str)
    (fun cst _extras ->
      let file = Fpath.v "<pattern>" in
      let env =
        { H.file; conv = H.line_col_to_pos_pattern str; extra = Pattern }
      in
      map_source_file env cst)
