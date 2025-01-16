open Fpath_.Operators
(**
   Boilerplate to be used as a template when mapping the move CST
   to another type of tree.
*)

module CST = Tree_sitter_move_on_sui.CST
module H = Parse_tree_sitter_helpers
open AST_generic
module G = AST_generic
module H2 = AST_generic_helpers

let src = Logs.Src.create "parser_move_on_sui"

module Log = (val Logs.src_log src : Logs.LOG)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
type mode = Pattern | Target
type env = mode H.env

let token = H.token
let str = H.str
let fb = Tok.unsafe_fake_bracket
let fake s = Tok.unsafe_fake_tok s
let fake_id s = (s, G.fake s)

let in_pattern env =
  match env.H.extra with
  | Target -> false
  | Pattern -> true

(*****************************************************************************)
(* Boilerplate converter *)
(*****************************************************************************)
(* This was started by copying tree-sitter-lang/semgrep-move/Boilerplate.ml *)

(**
   Boilerplate to be used as a template when mapping the move CST
   to another type of tree.
*)

(* Disable warnings against unused variables *)
[@@@warning "-26-27-32"]

(* Disable warning against unused 'rec' *)
[@@@warning "-39"]

let remove_char str ch = String.split_on_char ch str |> String.concat ""

let map_bool_literal (env : env) (x : CST.bool_literal) : G.literal =
  match x with
  | `True tok -> (* "true" *) G.Bool (true, token env tok)
  | `False tok -> (* "false" *) G.Bool (false, token env tok)

let map_byte_string_literal (env : env) (tok : CST.byte_string_literal) =
  (* pattern "b\"(\\\\.|[^\\\\\"])*\"" *) str env tok

let map_spec_apply_name_pattern (env : env) (tok : CST.spec_apply_name_pattern)
    =
  (* pattern [0-9a-zA-Z_*]+ *) G.PatWildcard (token env tok)

let map_address_literal (env : env) (tok : CST.address_literal) =
  (* pattern @0x[a-fA-F0-9]+ *) str env tok

let map_untyped_num_literal (env : env) (tok : CST.untyped_num_literal) =
  (* pattern 0x[a-fA-F0-9_]+ *) str env tok

let map_address_literal (env : env) (tok : CST.address_literal) =
  (* pattern 0x[a-fA-F0-9_]+ *) str env tok

let map_spec_condition_kind (env : env) (x : CST.spec_condition_kind) =
  match x with
  | `Assert tok -> (* "assert" *) token env tok
  | `Assume tok -> (* "assume" *) token env tok
  | `Decres tok -> (* "decreases" *) token env tok
  | `Ensures tok -> (* "ensures" *) token env tok
  | `Succes_if tok -> (* "succeeds_if" *) token env tok

let map_typed_num_literal (env : env) (tok : CST.typed_num_literal) =
  (* pattern [0-9][0-9_]*(?:u8|u16|u32|u64|u128|u256)? *) str env tok

let map_hex_string (env : env) (x : CST.hex_string_literal) =
  let content, tok = (* pattern "x\\\"[\\da-fA-F]*\\\"" *) str env x in
  let new_len = String.length content - 3 in
  let content = String.sub content 2 new_len in
  (G.fake "x\"", (content, tok), G.fake "\"")

let map_byte_string (env : env) (x : CST.byte_string_literal) =
  let content, tok = (* pattern "b\"(\\\\.|[^\\\\\"])*\"" *) str env x in
  let new_len = String.length content - 3 in
  let content = String.sub content 2 new_len in
  (G.fake "b\"", (content, tok), G.fake "\"")

let map_modifier (env : env) (x : CST.modifier) : G.attribute =
  match x with
  | `Public tok -> (* "public" *) G.attr G.Public (token env tok)
  | `Publ_83d19bc tok ->
      (* "public(package)" *) G.attr G.Protected (token env tok)
  | `Publ_7c2e49a tok ->
      (* "public(friend)" *) G.attr G.Protected (token env tok)
  | `Entry tok -> (* "entry" *) G.OtherAttribute (str env tok, [])
  | `Native tok -> (* "native" *) G.attr G.Extern (token env tok)

let map_unary_op (env : env) (x : CST.unary_op) =
  match x with
  | `BANG tok -> (* "!" *) token env tok

let map_primitive_type (env : env) (x : CST.primitive_type) : G.ident =
  match x with
  | `U8 tok -> (* "u8" *) str env tok
  | `U16 tok -> (* "u16" *) str env tok
  | `U32 tok -> (* "u32" *) str env tok
  | `U64 tok -> (* "u64" *) str env tok
  | `U128 tok -> (* "u128" *) str env tok
  | `U256 tok -> (* "u256" *) str env tok
  | `Bool tok -> (* "bool" *) str env tok
  | `Addr tok -> (* "address" *) str env tok
  | `Signer tok -> (* "signer" *) str env tok
  | `Byte tok -> (* "bytearray" *) str env tok

(* todo fix mut*)

let map_mutable (env : env) (x : CST.anon_choice_AMP_c6caa5d) =
  match x with
  | `AMP tok -> (* "&" *) (token env tok, [])
  | `AMPmut tok ->
      (* "&mut" *) (token env tok, [ G.KeywordAttr (G.Mutable, token env tok) ])

let map_untyped_num_literal (env : env) (tok : CST.untyped_num_literal) =
  (* pattern 0x[a-fA-F0-9_]+ *) str env tok

let map_typed_num_literal (env : env) (tok : CST.typed_num_literal) =
  (* pattern 0x[a-fA-F0-9_]+ *) str env tok

let map_num_literal (env : env) (x : CST.num_literal) =
  match x with
  | `Typed_num_lit tok ->
      (* pattern [0-9][0-9_]*(?:u8|u16|u32|u64|u128|u256)? *)
      map_typed_num_literal env tok
  | `Unty_num_lit tok ->
      (* pattern 0x[a-fA-F0-9_]+ *) map_untyped_num_literal env tok

let map_reserved_identifier (env : env) (x : CST.reserved_identifier) =
  match x with
  | `Exists tok -> (* "exists" *) token env tok
  | `Forall tok -> (* "forall" *) token env tok

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

let map_num (num : G.ident) : G.literal =
  let num_str, _ = num in
  let num_val = Parsed_int.parse num in
  (* Numbers in move can be up to 256 bits, so we use rational numbers to represent them *)
  match num_val with
  | None, _ -> G.Ratio (num_str, G.fake "")
  | _ -> G.Int num_val

(* G.Int num_val *)
let map_literal_value (env : env) (x : CST.literal_value) : G.literal =
  match x with
  | `Addr_lit tok ->
      map_num (map_address_literal env tok) (* pattern @0x[a-fA-F0-9]+ *)
  | `Bool_lit tok -> map_bool_literal env tok
  | `Num_lit tok -> map_num (map_num_literal env tok)
  | `Hex_str_lit tok ->
      G.String (map_hex_string env tok) (* pattern "x\"[0-9a-fA-F]*\"" *)
  | `Byte_str_lit tok -> G.String (map_byte_string env tok)
(* pattern "b\"(\\\\.|[^\\\\\"])*\"" *)

let map_module_id (env : env) (x : CST.anon_choice_num_lit_a33e50c) : G.ident =
  match x with
  | `Num_lit tok -> map_num_literal env tok
  | `Module_id tok -> str env tok

type member_list =
  | Member_list_mem_only of (G.ident * G.ident option) list
  | Member_list_no_addr_mod of G.ident * (G.ident * G.ident option) list
  | Member_list_addr of G.ident * G.ident * (G.ident * G.ident option) list
  | Member_mod_only of G.ident * G.ident option
  | Member_list_addr_members of G.ident * member_list list

let get_members_from_member_list member_list =
  match member_list with
  | Member_list_mem_only mems -> mems
  | Member_list_no_addr_mod (mod_name, mems) -> mems
  | Member_list_addr (address, mod_name, mems) -> mems
  | Member_mod_only (mod_name, alias) ->
      failwith "Can't exctract a member from single module name"
  | Member_list_addr_members (address, mems) ->
      failwith
        "You need to be careful here because some are mods and some are members"

let map_inner_directive (address, mod_name, name, label) : G.directive_kind =
  let use = G.fake "use" in
  match name with
  | "Self", _ ->
      G.ImportFrom
        ( use,
          G.DottedName [ address ],
          [ H2.mk_import_from_kind mod_name label ] )
  | _ ->
      G.ImportFrom
        ( use,
          G.DottedName [ address; mod_name ],
          [ H2.mk_import_from_kind name label ] )

let map_inner_directive_mod (address, mod_name, label) : G.directive_kind =
  let use = G.fake "use" in
  let alias =
    match label with
    | Some x -> Some (x, G.empty_id_info ())
    | None -> None
  in
  G.ImportFrom
    (use, G.DottedName [ address ], [ H2.mk_import_from_kind mod_name label ])

let map_member_to_directive member_list : G.directive_kind list =
  let out =
    match member_list with
    | Member_list_addr (address, mod_name, mems) ->
        mems
        |> List_.map (fun (name, label) ->
               map_inner_directive (address, mod_name, name, label))
    | Member_list_addr_members (address, mems) ->
        let out_list =
          mems
          |> List_.map (fun x ->
                 match x with
                 | Member_mod_only (mod_name, label) ->
                     [ map_inner_directive_mod (address, mod_name, label) ]
                 | Member_list_no_addr_mod (mod_name, mems) ->
                     mems
                     |> List_.map (fun (name, label) ->
                            map_inner_directive (address, mod_name, name, label))
                 | Member_list_mem_only mems ->
                     failwith "We should have converted members to modules"
                 | Member_list_addr (address, mod_name, mems) ->
                     failwith "Shouldn't have address in address"
                 | Member_list_addr_members (address, mems) ->
                     failwith "Shouldn't have address in address")
        in
        out_list |> List_.flatten
    | Member_list_no_addr_mod (mod_name, mems) -> failwith "Should have address"
    | Member_mod_only (mod_name, alias) -> failwith "Should have address"
    | Member_list_mem_only mems -> failwith "Should have address"
  in
  out

let map_parameter (is_mutable, is_dollar, ident, func_type) : G.parameter =
  let func_type =
    match is_mutable with
    | true ->
        {
          t = G.TyRef (fake "&mut", func_type);
          t_attrs = [ G.KeywordAttr (G.Mutable, fake "&mut") ];
        }
    | false -> func_type
  in
  G.Param
    {
      G.pname = Some ident;
      G.ptype = Some func_type;
      G.pdefault = None;
      G.pattrs = [];
      G.pinfo = G.empty_id_info ();
    }

let rec map_use_member (env : env) (x : CST.use_member) : member_list =
  match x with
  | `Choice_id_COLONCOLON_LCURL_use_member_rep_COMMA_use_member_opt_COMMA_RCURL
      x -> (
      match x with
      | `Id_COLONCOLON_LCURL_use_member_rep_COMMA_use_member_opt_COMMA_RCURL
          (v1, v2, v3, v4, v5, v6, v7) ->
          let mod_name = (* identifier *) str env v1 in
          let mod_name_ident : G.ident = (* identifier *) str env v1 in
          let v2 = (* "::" *) token env v2 in
          let lb = (* "{" *) token env v3 in
          let mem_list = map_use_member env v4 in
          let use_mem = get_members_from_member_list mem_list in
          let use_mems =
            v5
            |> List_.map (fun (v1, v2) ->
                   (*TODO*)
                   let v1 = (* "," *) token env v1 in
                   let mem_list_in = map_use_member env v2 in
                   let use_mem_in = get_members_from_member_list mem_list_in in
                   use_mem_in)
            |> List_.flatten
          in
          let _comma =
            match v6 with
            | Some tok -> Some ((* "," *) token env tok)
            | None -> None
          in
          let all_mems = use_mem @ use_mems in
          let rb = (* "}" *) token env v7 in
          let all_mem_idents =
            all_mems
            |> List_.map (fun (v1, v2) ->
                   let v2 : G.ident = v1 in
                   (v2, None))
          in

          Member_list_no_addr_mod (mod_name_ident, all_mem_idents)
      | `Id_COLONCOLON_id_opt_as_id (v1, v2, v3, v4) ->
          let mod_name = (* identifier *) str env v1 in
          let mod_name_ident : G.ident = (* identifier *) str env v1 in
          let v2 = (* "::" *) token env v2 in
          let memeber = (* identifier *) str env v3 in
          let memeber_ident : G.ident = (* identifier *) str env v3 in
          let mems =
            match v4 with
            | Some (v1, v2) ->
                let v1 = (* "as" *) token env v1 in
                let alias = (* identifier *) str env v2 in
                let alias_ident : G.ident = (* identifier *) str env v2 in
                Member_list_no_addr_mod
                  (mod_name_ident, [ (memeber_ident, Some alias_ident) ])
            | None ->
                Member_list_no_addr_mod
                  (mod_name_ident, [ (memeber_ident, None) ])
          in
          mems
      | `Id_opt_as_id (v1, v2) ->
          let memeber = (* identifier *) str env v1 in
          let memeber_ident : G.ident = (* identifier *) str env v1 in
          let mems =
            match v2 with
            | Some (v1, v2) ->
                let v1 = (* "as" *) token env v1 in
                let alias = (* identifier *) token env v2 in
                let alias_ident : G.ident = (* identifier *) str env v2 in
                Member_list_mem_only [ (memeber_ident, Some alias_ident) ]
            | None -> Member_list_mem_only [ (memeber_ident, None) ]
          in
          mems)
  | `Ellips tok ->
      let elipse_ident : G.ident = (* "..." *) str env tok in
      (* "..." *) Member_list_mem_only [ (elipse_ident, None) ]

let map_label (env : env) ((v1, v2) : CST.label) : G.ident =
  let v1 = token env v1 in
  (* "'" *)
  let v2 = str env v2 in
  (* identifier *)
  v2

let map_ability_decls (env : env) ((v1, v2, v3) : CST.ability_decls) :
    G.type_ list =
  let v1 = (* "has" *) token env v1 in
  let ability_list =
    List_.map
      (fun (v1, v2) ->
        let v1 = map_ability env v1 in
        let v2 = (* "," *) token env v2 in
        v1)
      v2
  in
  let abilites =
    match v3 with
    | Some x -> ability_list @ [ map_ability env x ]
    | None -> ability_list
  in
  abilites

let map_postfix_ability_decls (env : env)
    ((v1, v2, v3, v4) : CST.postfix_ability_decls) : G.type_ list =
  let v1 = (* "has" *) token env v1 in
  let ability_list =
    List_.map
      (fun (v1, v2) ->
        let v1 = map_ability env v1 in
        let v2 = (* "," *) token env v2 in
        v1)
      v2
  in
  let abilites =
    match v3 with
    | Some x -> ability_list @ [ map_ability env x ]
    | None -> ability_list
  in
  let v4 = (* ";" *) token env v4 in
  abilites

let map_identifier_or_metavariable (env : env)
    (x : CST.identifier_or_metavariable) =
  match x with
  | `Choice_macro_id_dollar x ->
      let ident, attribs =
        match x with
        | `Macro_id_dollar tok ->
            let type_name = (* pattern \$[a-zA-Z][0-9a-zA-Z_]* *) str env tok in
            let macro_attr =
              G.NamedAttr (fake "$", H2.name_of_id (fake_id "$"), (sc, [], sc))
            in
            (type_name, [ macro_attr ])
        | `Semg_meta_ellips tok ->
            let ident = (* pattern \$\.\.\.[A-Z_][A-Z_0-9]* *) str env tok in
            (ident, [])
        | `Semg_meta_var tok ->
            let ident = (* pattern \$[A-Z_][A-Z_0-9]* *) str env tok in
            (ident, [])
        | `X__ tok ->
            let ident = (* "_" *) str env tok in
            (ident, [])
      in
      (ident, attribs)
  | `Opt_phan_clean_id (v1, v2) ->
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
      let ident = (* pattern (`)?[a-zA-Z_][0-9a-zA-Z_]*(`)? *) str env v2 in
      (ident, phatom)

let map_type_parameter (env : env) attrs (x : CST.type_parameter) :
    G.type_parameter =
  match x with
  | `Id_or_meta_opt_COLON_abil_rep_PLUS_abil_opt_PLUS (v1, v2) ->
      let type_name, attribs = map_identifier_or_metavariable env v1 in
      let abilites =
        (* todo use abilities *)
        match v2 with
        | Some (v1, v2, v3, v4) ->
            let v1 = (* ":" *) token env v1 in
            let ability = map_ability env v2 in
            let ablities =
              List_.map
                (fun (v1, v2) ->
                  let v1 = token env v1 in
                  let v2 = map_ability env v2 in
                  v2)
                v3
            in
            let v4 =
              match v4 with
              | Some tok -> Some (token env tok)
              | None -> None
            in
            ability :: ablities
        | None -> []
      in
      G.tparam_of_id ~tp_attrs:(attrs @ attrs @ attribs) type_name
  | `Ellips tok -> G.TParamEllipsis (token env tok)

let map_spec_property (env : env) ((v1, v2) : CST.spec_property) : G.expr =
  let ident : G.ident = (* identifier *) str env v1 in
  let assinged_lit_expr =
    match v2 with
    | Some (v1, v2) ->
        let v1 = (* "=" *) token env v1 in
        let v2 = map_literal_value env v2 in
        let name = G.N (H2.name_of_id ident) |> G.e in
        G.Assign (name, v1, G.L v2 |> G.e) |> G.e
    | None -> G.N (H2.name_of_id ident) |> G.e
  in
  assinged_lit_expr

let map_number (env : env) (x : CST.num_literal) =
  match x with
  | `Typed_num_lit tok ->
      (* pattern [0-9][0-9_]*(?:u8|u16|u32|u64|u128|u256)? *) str env tok
  | `Unty_num_lit tok -> (* pattern 0x[a-fA-F0-9_]+ *) str env tok

let map_anon_choice_num_lit_a33e50c (env : env)
    (x : CST.anon_choice_num_lit_a33e50c) =
  match x with
  | `Num_lit tok -> map_number env tok
  | `Module_id tok -> str env tok

let map_module_identity (env : env) ((v1, v2, v3) : CST.module_identity) =
  let address = map_anon_choice_num_lit_a33e50c env v1 in
  (*todo what it not addr*)
  let v2 = (* "::" *) token env v2 in
  let mod_name = (* identifier *) str env v3 in
  (address, mod_name)

let map_block_identifier (env : env) ((v1, v2) : CST.block_identifier) =
  let label = map_label env v1 in
  let v2 = token env v2 in
  (* ":" *)
  label

let map_type_parameters (env : env) ((v1, v2, v3, v4, v5) : CST.type_parameters)
    : G.type_parameters =
  let lt = (* "<" *) token env v1 in
  let first_param = map_type_parameter env [] v2 in
  let params =
    v3
    |> List_.map (fun (v1, v2) ->
           let v1 = (* "," *) token env v1 in
           let v2 = map_type_parameter env [] v2 in
           v2)
  in
  let v4 =
    match v4 with
    | Some tok -> Some ((* "," *) token env tok)
    | None -> None
  in
  let gt = (* ">" *) token env v5 in
  (lt, first_param :: params, gt)

let map_spec_pragma (env : env) ((v1, v2, v3, v4) : CST.spec_pragma) =
  let v1 = (* "pragma" *) token env v1 in
  let spec_props =
    List_.map
      (fun (v1, v2) ->
        let spec_prop = map_spec_property env v1 in
        let v2 = (* "," *) token env v2 in
        spec_prop)
      v2
  in
  let all_spec_prop =
    match v3 with
    | Some x -> spec_props @ [ map_spec_property env x ]
    | None -> spec_props
  in
  let v4 = (* ";" *) token env v4 in
  let all_spec_prop = all_spec_prop |> List_.map (fun x -> G.E x) in
  G.OtherStmt (G.OS_Todo, all_spec_prop)

let map_condition_properties (env : env)
    ((v1, v2, v3, v4) : CST.condition_properties) =
  let v1 = (* "[" *) token env v1 in
  let props =
    List_.map
      (fun (v1, v2) ->
        let v1 = map_spec_property env v1 in
        let v2 = (* "," *) token env v2 in
        v1)
      v2
  in
  let all_spec_props =
    match v3 with
    | Some x -> props @ [ map_spec_property env x ]
    | None -> props
  in
  let any_props = all_spec_props |> List_.map (fun x -> G.E x) in
  let v4 = (* "]" *) token env v4 in
  G.OtherAttribute (("ConditionProps", v1), any_props)

let map_module_access_ (env : env) (x : CST.module_access) =
  match x with
  | `Choice_semg_meta_ellips x -> (
      match x with
      | `Semg_meta_ellips tok ->
          let member = (* pattern \$\.\.\.[A-Z_][A-Z_0-9]* *) str env tok in
          (H2.name_of_id member, (None, None, Some member))
      | `Macro_id_dollar tok ->
          let member = (* pattern \$[a-zA-Z][0-9a-zA-Z_]* *) str env tok in
          (H2.name_of_id member, (None, None, Some member))
      | `Id tok ->
          let member =
            (* pattern (`)?[a-zA-Z_][0-9a-zA-Z_]*(`)?|\$\.\.\.[A-Z_][A-Z_0-9]*]|\$[A-Z_][A-Z_0-9]* *)
            str env tok
          in
          (H2.name_of_id member, (None, None, Some member)))
  | `AT_id (v1, v2) ->
      let v1 = (* "@" *) str env v1 in
      let member = (* identifier *) str env v2 in
      (H2.name_of_id member, (None, None, Some member))
  | `Rese_id tok ->
      let quant = map_reserved_identifier env tok in
      (H2.name_of_id (fake_id ""), (None, None, None))
      (* Not sure this ever happens in a module_access*)
  | `Module_id_COLONCOLON_id (v1, v2, v3) ->
      let mod_name = (* identifier *) str env v1 in
      let v2 = (* "::" *) token env v2 in
      let member = (* identifier *) str env v3 in
      (H2.name_of_ids [ mod_name; member ], (None, Some mod_name, Some member))
  | `Module_iden_COLONCOLON_id (v1, v2, v3) ->
      let address, mod_name = map_module_identity env v1 in
      let v2 = (* "::" *) token env v2 in
      let member = (* identifier *) str env v3 in
      ( H2.name_of_ids [ address; mod_name; member ],
        (Some address, Some mod_name, Some member) )
  | `Module_iden_COLONCOLON_id_COLONCOLON_id (v1, v2, v3, v4, v5) ->
      let address, mod_name = map_module_identity env v1 in
      let v2 = (* "::" *) token env v2 in
      let enum_name = (* identifier *) str env v3 in
      let v4 = (* "::" *) token env v4 in
      let variant = (* "::" *) str env v5 in
      let enum_name_str, _ = enum_name in
      let variant_str, _ = variant in
      (*todo is treating enum as a member correct*)
      ( H2.name_of_ids [ address; mod_name; enum_name; variant ],
        ( Some address,
          Some mod_name,
          Some (enum_name_str ^ "::" ^ variant_str, sc) ) )
  | `Ellips (* "..." *) x -> (H2.name_of_id (str env x), (None, None, None))

let map_module_access (env : env) (x : CST.module_access) : G.name =
  let name, _ = map_module_access_ env x in
  name

let map_friend_access (env : env) (x : CST.friend_access) : G.dotted_ident =
  match x with
  | `Id tok ->
      let ident : G.ident = str env tok in
      [ ident ]
  | `Module_iden tok ->
      let addr, mod_name = map_module_identity env tok in
      [ addr; mod_name ]

let map_use_module_members (env : env) (x : CST.use_module_members) =
  match x with
  | `Choice_num_lit_COLONCOLON_LCURL_use_member_rep_COMMA_use_member_opt_COMMA_RCURL
      (v1, v2, v3, v4, v5, v6, v7) ->
      (* Here we have a list of multiple *)
      let address = map_anon_choice_num_lit_a33e50c env v1 in
      let v2 = (* "::" *) token env v2 in
      let v3 = (* "{" *) token env v3 in
      let member_list = map_use_member env v4 in
      let member_list_list =
        List_.map
          (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_use_member env v2 in
            v2)
          v5
      in
      let v6 =
        match v6 with
        | Some tok -> Some ((* "," *) token env tok)
        | None -> None
      in
      let v7 = (* "}" *) token env v7 in
      let all_mems = member_list :: member_list_list in
      let clean_memes =
        all_mems
        |> List_.map (fun x ->
               match x with
               | Member_list_mem_only mems ->
                   let mods =
                     mems
                     |> List_.map (fun (v1, v2) -> Member_mod_only (v1, v2))
                   in
                   mods
               | Member_list_no_addr_mod (mod_name, mems) ->
                   [ Member_list_no_addr_mod (mod_name, mems) ]
               | Member_list_addr (address, mod_name, mems) ->
                   failwith "Shouldn't have an address"
               | Member_mod_only (mod_name, alias) ->
                   failwith "Can't exctract a member from single module name"
               | Member_list_addr_members (address, mems) ->
                   failwith "Should not already be a address list")
      in
      let flat_members = clean_memes |> List_.flatten in
      Member_list_addr_members (address, flat_members)
  | `Module_iden_COLONCOLON_LCURL_use_member_rep_COMMA_use_member_opt_COMMA_RCURL
      (v1, v2, v3, v4, v5, v6, v7) ->
      let address, mod_name = map_module_identity env v1 in
      let v2 = (* "::" *) token env v2 in
      let v3 = (* "{" *) token env v3 in
      let member_list = map_use_member env v4 in
      let mems = get_members_from_member_list member_list in
      let member_list_list =
        List_.map
          (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_use_member env v2 in
            v2)
          v5
      in
      let member_list_sing =
        member_list_list
        |> List_.map (fun v1 ->
               let mems = get_members_from_member_list v1 in
               mems)
        |> List_.flatten
      in
      let v6 =
        match v6 with
        | Some tok -> Some ((* "," *) token env tok)
        | None -> None
      in
      let v7 = (* "}" *) token env v7 in
      let all_mems = mems @ member_list_sing in
      Member_list_addr (address, mod_name, all_mems)

let map_use_module (env : env) ((v1, v2) : CST.use_module) : directive_kind list
    =
  let use = fake "use" in
  let address, mod_name = map_module_identity env v1 in
  let adderss_ident : G.ident = address in
  let mod_name_ident : G.ident = mod_name in
  let directive =
    match v2 with
    | Some (v1, v2) ->
        let v1 = (* "as" *) token env v1 in
        let alias = (* identifier *) str env v2 in
        let alias_ident : G.ident = alias in
        [
          G.ImportFrom
            ( use,
              G.DottedName [ adderss_ident ],
              [ H2.mk_import_from_kind mod_name_ident (Some alias_ident) ] );
        ]
    | None ->
        [
          G.ImportFrom
            ( use,
              G.DottedName [ adderss_ident ],
              [ H2.mk_import_from_kind mod_name_ident None ] );
        ]
  in
  directive

let map_use_module_member (env : env) ((v1, v2, v3) : CST.use_module_member) :
    member_list =
  let address, mod_name = map_module_identity env v1 in
  let v2 = (* "::" *) token env v2 in
  let member_list = map_use_member env v3 in
  let mems = get_members_from_member_list member_list in
  Member_list_addr (address, mod_name, mems)

let map_enum_signature (env : env) ((v1, v2, v3, v4) : CST.enum_signature) =
  let v1 = (* "enum" *) token env v1 in
  let name = (* identifier *) str env v2 in
  let type_params =
    match v3 with
    | Some x -> Some (map_type_parameters env x)
    | None -> None
  in
  let abilites =
    match v4 with
    | Some x -> map_ability_decls env x
    | None -> []
  in
  (name, type_params, abilites)

let map_struct_signature (env : env) attrs
    ((v1, v2, v3, v4) : CST.struct_signature) =
  let struct_ = (* "struct" *) token env v1 in
  let name = (* identifier *) str env v2 in
  let type_params =
    match v3 with
    | Some x -> Some (map_type_parameters env x)
    | None -> None
  in
  let abilities =
    match v4 with
    | Some x -> map_ability_decls env x
    | None -> []
  in
  let struct_ent =
    let type_params =
      match type_params with
      | Some params -> params
      | None -> (G.fake "", [], G.fake "")
    in
    G.basic_entity ~tparams:type_params ~attrs name
  in
  (struct_, abilities, struct_ent)

let map_spec_apply_pattern (env : env) ((v1, v2, v3) : CST.spec_apply_pattern) =
  let pub_int =
    v1
    |> Option.map (fun x ->
           match x with
           | `Public tok -> (* "public" *) str env tok
           | `Inte tok -> (* "internal" *) str env tok)
    |> Option.to_list
    |> List_.map (fun x -> G.I x)
  in
  let pattern =
    (* pattern [0-9a-zA-Z_*]+ *) map_spec_apply_name_pattern env v2
  in
  let pattern = G.P pattern in
  let type_params =
    Option.map (map_type_parameters env) v3
    |> Option.map (fun x ->
           let _, types, _ = x in
           types)
    |> Option.value ~default:[]
    |> List_.map (fun x -> G.Tp x)
  in
  G.OtherPat
    (("SpecApplyPattern", G.fake ""), pub_int @ [ pattern ] @ type_params)

let map_use_fun (env : env) ((v1, v2, v3, v4, v5, v6) : CST.use_fun) :
    G.directive_kind list =
  let fun_ = (* "fun" *) token env v1 in
  let _, (address, mod_name, member) = map_module_access_ env v2 in
  let v3, tok = (* "as" *) str env v3 in
  let use = fake "use" in
  let _, (alias_address, alias_mod_name, alais_member) =
    map_module_access_ env v4
  in
  let v5 = (* "." *) str env v5 in
  let function_ident = (* unknown token description *) str env v6 in
  let directive =
    match (address, mod_name) with
    | Some address, Some mod_name -> (
        let address_ident : G.ident = address in
        let mod_name_ident : G.ident = mod_name in
        match alias_mod_name with
        | Some alias_mod_name ->
            let alias_mod_name_ident : G.ident = alias_mod_name in
            [
              G.ImportFrom
                ( use,
                  G.DottedName [ address_ident ],
                  [
                    H2.mk_import_from_kind mod_name_ident
                      (Some alias_mod_name_ident);
                  ] );
            ]
        | None ->
            [
              G.ImportFrom
                ( use,
                  G.DottedName [ address_ident; mod_name_ident ],
                  [ H2.mk_import_from_kind function_ident None ] );
            ])
    | _ -> []
  in
  directive

let map_type_to_param (env : env) (ty : G.type_) : G.parameter =
  match ty.t with
  (* If this type is a singular identifier that is a metavariable,
     * then the user probably meant to write a metavariable parameter.
     * So let's translate it to one.
  *)
  | G.TyN (Id (((s, _) as id), _))
    when AST_generic.is_metavar_name s && in_pattern env ->
      let param =
        {
          G.pname = Some id;
          G.ptype = None;
          G.pdefault = None;
          G.pattrs = [];
          G.pinfo = G.empty_id_info ();
        }
      in
      G.Param param
  | _ ->
      let param =
        {
          G.pname = None;
          G.ptype = Some ty;
          G.pdefault = None;
          G.pattrs = [];
          G.pinfo = G.empty_id_info ();
        }
      in
      G.Param param

let rec map_function_type_parameters (env : env)
    ((v1, v2, v3, v4) : CST.function_type_parameters) : G.parameter list =
  let v1 = (* "|" *) token env v1 in
  let type_list =
    List_.map
      (fun (v1, v2) ->
        let v1 = map_type_ env v1 in
        let v2 = (* "," *) token env v2 in
        v1)
      v2
  in
  let all_types =
    match v3 with
    | Some x -> type_list @ [ map_type_ env x ]
    | None -> type_list
  in
  let v4 = (* "|" *) token env v4 in
  let all_params = List_.map (fun x -> map_type_to_param env x) all_types in
  all_params

and map_type_ (env : env) (x : CST.type_) : G.type_ =
  match x with
  | `Choice_apply_type x -> (
      match x with
      | `Apply_type (v1, v2) ->
          let mod_access = map_module_access env v1 in
          let out_type =
            match v2 with
            | Some x ->
                G.TyApply (G.TyN mod_access |> G.t, map_type_arguments env x)
                |> G.t
            | None -> G.TyN mod_access |> G.t
          in
          out_type
      | `Ref_type (v1, v2) ->
          let ref_tok, atrrs = map_mutable env v1 in
          let sub_type = map_type_ env v2 in
          { t = G.TyRef (ref_tok, sub_type); t_attrs = atrrs }
      | `Tuple_type (v1, v2, v3, v4) ->
          let v1 = (* "(" *) token env v1 in
          let type_list =
            List_.map
              (fun (v1, v2) ->
                let v1 = map_type_ env v1 in
                let v2 = (* "," *) token env v2 in
                v1)
              v2
          in
          let full_type_list =
            match v3 with
            | Some x -> type_list @ [ map_type_ env x ]
            | None -> type_list
          in
          let v4 = (* ")" *) token env v4 in
          G.TyTuple (v1, full_type_list, v4) |> G.t
      | `Func_type (v1, v2) ->
          let type_params = map_function_type_parameters env v1 in
          let ret_type =
            match v2 with
            | Some (v1, v2) ->
                let v1 = (* "->" *) token env v1 in
                let v2 = map_type_ env v2 in
                v2
            | None -> G.ty_builtin (fake_id "()")
          in
          G.TyFun (type_params, ret_type) |> G.t
      | `Prim_type x ->
          let prim_type = map_primitive_type env x in
          G.ty_builtin prim_type)
  | `Ellips tok -> (* "..." *) G.TyEllipsis (token env tok) |> G.t

and map_datatype_fields (env : env) (x : CST.datatype_fields) =
  match x with
  | `Posi_fields (v1, v2, v3, v4) ->
      let lp = (* "(" *) token env v1 in
      let field_types =
        List_.map
          (fun (v1, v2) ->
            let field_type = map_type_ env v1 in
            let v2 = (* "," *) token env v2 in
            field_type)
          v2
      in
      let all_types =
        match v3 with
        | Some x -> field_types @ [ map_type_ env x ]
        | None -> field_types
      in
      let rp = (* ")" *) token env v4 in
      let all_fileds =
        all_types
        |> List_.mapi (fun idx field_type ->
               let var_def =
                 { G.vinit = None; G.vtype = Some field_type; vtok = G.no_sc }
               in
               let ent =
                 {
                   G.name =
                     G.EDynamic (G.L (G.Int (Parsed_int.of_int idx)) |> G.e);
                   G.attrs = [];
                   G.tparams = None;
                 }
               in
               G.fld (ent, G.FieldDefColon var_def))
      in
      (lp, all_fileds, rp)
  | `Named_fields (v1, v2, v3, v4) ->
      let lb = (* "{" *) token env v1 in
      let field_annots =
        List_.map
          (fun (v1, v2) ->
            let field_annots = map_field_annotation env v1 in
            let v2 = (* "," *) token env v2 in
            field_annots)
          v2
      in
      let all_fileds =
        match v3 with
        | Some x -> field_annots @ [ map_field_annotation env x ]
        | None -> field_annots
      in
      let rb = (* "}" *) token env v4 in
      (lb, all_fileds, rb)

and map_datatype_fields_type (env : env) (x : CST.datatype_fields) =
  match x with
  | `Posi_fields (v1, v2, v3, v4) ->
      let lp = (* "(" *) token env v1 in
      let field_types =
        List_.map
          (fun (v1, v2) ->
            let field_type = map_type_ env v1 in
            let v2 = (* "," *) token env v2 in
            field_type)
          v2
      in
      let all_types =
        match v3 with
        | Some x -> field_types @ [ map_type_ env x ]
        | None -> field_types
      in
      let rp = (* ")" *) token env v4 in
      all_types
  | `Named_fields (v1, v2, v3, v4) ->
      let lb = (* "{" *) token env v1 in
      let types =
        List_.map
          (fun (v1, v2) ->
            let types = map_field_annotation_type env v1 in
            let v2 = (* "," *) token env v2 in
            types)
          v2
      in
      let all_types =
        match v3 with
        | Some x -> types @ [ map_field_annotation_type env x ]
        | None -> types
      in
      let rb = (* "}" *) token env v4 in
      all_types

and map_type_arguments (env : env) (x : CST.type_arguments) : G.type_arguments =
  match x with
  | `LT_type_rep_COMMA_type_opt_COMMA_GT (v1, v2, v3, v4, v5) ->
      let lab = (* "<" *) token env v1 in
      let first_type = map_type_ env v2 in
      let types =
        List_.map
          (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_type_ env v2 in
            v2)
          v3
      in
      let all_types_args = first_type :: types |> List_.map (fun x -> G.TA x) in

      let v4 =
        match v4 with
        | Some tok -> Some ((* "," *) token env tok)
        | None -> None
      in
      let rab = (* ">" *) token env v5 in
      (lab, all_types_args, rab)
  | `Ellips tok ->
      (* "..." *)
      let ellips_tk = G.TyEllipsis (token env tok) |> G.t in
      let ellipse_ta = G.TA ellips_tk in
      (G.fake "<", [ ellipse_ta ], G.fake ">")
  | `LT_ellips_GT (v1, v2, v3) ->
      (* "..." *)
      let v1 = (* "<" *) token env v1 in
      let ellips_tk = G.TyEllipsis (token env v2) |> G.t in
      let ellipse_ta = G.TA ellips_tk in
      let v3 = (* ">" *) token env v3 in
      (v1, [ ellipse_ta ], v3)

and map_field_annotation (env : env) (x : CST.field_annotation) : G.field =
  match x with
  | `Field_id_COLON_type (v1, v2, v3) ->
      let ident = (* identifier *) str env v1 in
      let v2 = (* ":" *) token env v2 in
      let field_type = map_type_ env v3 in

      let var_def =
        { G.vinit = None; G.vtype = Some field_type; vtok = G.no_sc }
      in
      let ent = G.basic_entity ident in
      G.fld (ent, G.FieldDefColon var_def)
  | `Ellips tok -> (* "..." *) G.field_ellipsis (token env tok)

and map_field_annotation_type (env : env) (x : CST.field_annotation) : G.type_ =
  match x with
  | `Field_id_COLON_type (v1, v2, v3) ->
      let ident = (* identifier *) str env v1 in
      let v2 = (* ":" *) token env v2 in
      let field_type = map_type_ env v3 in
      field_type
  | `Ellips tok -> (* "..." *) G.TyEllipsis (token env tok) |> G.t

(*let map_anon_choice_type_param_id_bd8a33f (env : env) (x : CST.anon_choice_type_param_id_bd8a33f) =
  match x with
  | `Id tok ->
    let name: G.ident = (* identifier *) str env tok in
    let name_name =H2.name_of_id name in
    G.N name_name |> G.e
  | `Ellips tok -> (* "..." *) G.Ellipsis (token env tok) |> G.e*)

let map_annotation_expr (env : env) (x : CST.annotation_expr) : G.expr =
  match x with
  | `Id tok ->
      let name = str env tok in
      let name_name = H2.name_of_id name in
      G.N name_name |> G.e
  | `Id_EQ_choice_COLONCOLON_module_access (v1, v2, v3) ->
      let name = str env v1 in
      let name_name = H2.name_of_id name in
      let v2 = (* "=" *) token env v2 in
      let lh_exp =
        match v3 with
        | `COLONCOLON_module_access (v1, v2) ->
            let v1 = (* "::" *) token env v1 in
            let mod_name = map_module_access env v2 in
            G.N mod_name |> G.e
        | `Module_access x ->
            let mod_name = map_module_access env x in
            G.N mod_name |> G.e
        | `Lit_value x -> G.L (map_literal_value env x) |> G.e
      in
      G.Assign (G.N name_name |> G.e, v2, lh_exp) |> G.e
(* |  `Ellips x -> (* "..." *) G.Ellipsis (token env x) |> G.e
   | `Ellips_or_ellips (v1, v2, v3) ->
       let rhs =  map_anon_choice_type_param_id_bd8a33f  (* "=" *) env v1 in
       let v2 = (* "=" *) token env v2 in
       let lhs  =map_anon_choice_type_param_id_bd8a33f (* "=" *) env v3 in
       G.Assign (rhs, v2, lhs) |> G.e*)

let map_struct_item (env : env) (x : CST.struct_item) : G.stmt =
  let struct_, all_abilities, struct_ent, fields =
    match x with
    | `Native_struct_defi (v1, v2, v3, v4) ->
        let public_attrs =
          match v1 with
          | Some tok -> (* "public" *) [ G.attr G.Public (token env tok) ]
          | None -> []
        in
        let native_attrs = (* "native" *) [ G.attr G.Extern (token env v2) ] in
        let struct_, abilities, struct_ent =
          map_struct_signature env (native_attrs @ public_attrs) v3
        in
        let v4 = (* ";" *) token env v4 in
        (struct_, abilities, struct_ent, (sc, [], sc))
    | `Struct_defi (v1, v2, v3, v4) ->
        let public_attrs =
          match v1 with
          | Some tok -> (* "public" *) [ G.attr G.Public (token env tok) ]
          | None -> []
        in
        let struct_, abilities, struct_ent =
          map_struct_signature env public_attrs v2
        in
        let fields = map_datatype_fields env v3 in
        let all_abilities =
          match v4 with
          | Some x -> abilities @ map_postfix_ability_decls env x
          | None -> abilities
        in
        (struct_, all_abilities, struct_ent, fields)
  in
  let struct_def =
    {
      ckind = (G.Class, struct_);
      cextends = [];
      cimplements = all_abilities;
      cmixins = [];
      cparams = fb [];
      cbody = fields;
    }
  in
  G.DefStmt (struct_ent, G.ClassDef struct_def) |> G.s

let map_macro_module_access (env : env) ((v1, v2) : CST.macro_module_access) =
  let name = map_module_access env v1 in
  let bang = (* "!" *) token env v2 in
  (name, bang)

let map_friend_declaration (env : env) ((v1, v2, v3) : CST.friend_declaration) =
  let friend = (* "friend" *) str env v1 in
  let member = map_friend_access env v2 in
  let member_name = H2.name_of_ids member in
  let v3 = (* ";" *) token env v3 in

  G.OtherDirective (friend, [ G.Name member_name ]) |> G.d

let map_spec_block_target (env : env) (x : CST.spec_block_target) : G.any =
  match x with
  | `Id tok -> (* identifier *) G.I (str env tok)
  | `Module tok -> (* "module" *) G.I (str env tok)
  | `Spec_blk_target_schema (v1, v2, v3) ->
      let schema = G.I (str env v1) in
      (* "schema" *)
      let identifier : G.ident = str env v2 in
      (* identifier *)
      let type_params =
        match v3 with
        | Some x -> Some (map_type_parameters env x)
        | None -> None
      in
      let type_params =
        match type_params with
        | Some params -> params
        | None -> (G.fake "", [], G.fake "")
      in
      let entity = G.basic_entity ~tparams:type_params identifier in
      G.Anys [ schema; G.En entity ]

let rec transpile_let_bind (env : env) (left : G.pattern) (right : G.expr) :
    G.field list =
  match left with
  | G.PatId (var, _) -> [ G.basic_field var (Some right) None ]
  | G.PatEllipsis tok -> [ G.F (G.Ellipsis tok |> G.e |> G.exprstmt) ]
  | G.PatDisj (G.PatType inner_type, inner) ->
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
             let element = G.ArrayAccess (right, (sc, idx, sc)) |> G.e in
             transpile_let_bind env pat element)
      |> List_.flatten
  | G.OtherPat (("ExprToPattern", _), [ E e ]) -> (
      match e.e with
      | G.IdSpecial (G.Spread, tok) ->
          [ G.F (G.IdSpecial (G.Spread, tok) |> G.e |> G.exprstmt) ]
      | _ ->
          Log.err (fun m ->
              m "Unsupported pattern in let binding %s \n\n \n %s "
                (G.show_pattern left) (G.show_expr right));
          failwith "Unsupported pattern in let binding")
  | _ ->
      Log.err (fun m ->
          m "Unsupported pattern in let binding %s \n\n \n %s "
            (G.show_pattern left) (G.show_expr right));
      failwith "Unsupported pattern in let binding"

let map_use_declaration (env : env) ((v1, v2, v3, v4) : CST.use_declaration) :
    G.directive list =
  let attrs =
    match v1 with
    | Some tok -> (* "public" *) [ G.KeywordAttr (G.Public, token env tok) ]
    | None -> []
  in
  let use_ = (* "use" *) token env v2 in
  let directives =
    match v3 with
    | `Use_fun x -> map_use_fun env x
    | `Use_module x -> map_use_module env x
    | `Use_module_member x ->
        let member_list = map_use_module_member env x in
        map_member_to_directive member_list
    | `Use_module_members x ->
        let member_list = map_use_module_members env x in
        map_member_to_directive member_list
  in

  let v4 = (* ";" *) token env v4 in
  directives |> List_.map (fun x -> { d = x; d_attrs = attrs })

let map_spec_variable (env : env) ((v1, v2, v3, v4, v5, v6) : CST.spec_variable)
    =
  let attr =
    match v1 with
    | Some x -> (
        match x with
        | `Global tok -> [ G.KeywordAttr (G.Public, token env tok) ]
        | `Local tok -> [ G.KeywordAttr (G.Private, token env tok) ])
    | None -> []
  in
  let ident = (* identifier *) str env v2 in
  let type_params =
    match v3 with
    | Some x -> Some (map_type_parameters env x)
    | None -> None
  in
  let v4 = (* ":" *) token env v4 in
  let type_ = map_type_ env v5 in
  let v6 = (* ";" *) token env v6 in
  let type_params =
    match type_params with
    | Some params -> params
    | None -> (G.fake "", [], G.fake "")
  in
  let entity = G.basic_entity ~attrs:attr ~tparams:type_params ident in
  G.DefStmt (entity, G.VarDef { vinit = None; vtype = Some type_; vtok = None })
  |> G.s

let map_ret_type (env : env) ((v1, v2) : CST.ret_type) : G.type_ =
  let v1 = token env v1 in
  (* ":" *)
  let v2 = map_type_ env v2 in
  v2

let map_function_parameter (env : env) (x : CST.function_parameter) :
    G.parameter =
  match x with
  | `Opt_mut_id_or_meta_COLON_type (v1, v2, v3, v4) ->
      let is_mutable =
        match v1 with
        | Some tok -> true
        | None -> false
      in
      let param_name, attribs = map_identifier_or_metavariable env v2 in
      let v3 = token env v3 in
      (* ":" *)
      let func_type = map_type_ env v4 in
      let param = map_parameter (is_mutable, attribs, param_name, func_type) in
      param
  | `Ellips tok -> (* "..." *) G.ParamEllipsis (token env tok)

(* Todo grammer doesn't currently map these to mods and funcs*)

let rec map_annotation_item (env : env) (x : CST.annotation_item) =
  match x with
  | `Choice_anno_expr x -> (
      match x with
      | `Anno_expr x -> map_annotation_expr env x
      | `Anno_list (v1, v2, v3, v4, v5, v6) ->
          let name = (* identifier *) str env v1 in
          let name_name = H2.name_of_id name in
          let v2 = (* "(" *) token env v2 in
          let expr = map_anon_choice_lit_value_3ef3d77 env v3 in
          let expr_list =
            List_.map
              (fun (v1, v2) ->
                let v1 = (* "," *) token env v1 in
                let expr_list = map_anon_choice_lit_value_3ef3d77 env v2 in
                expr_list)
              v4
          in
          let v5 =
            match v5 with
            | Some tok -> Some ((* "," *) token env tok)
            | None -> None
          in
          let v6 = (* ")" *) token env v6 in
          let full_expr_list = expr :: expr_list in
          G.Assign
            ( G.N name_name |> G.e,
              sc,
              G.Container (G.Dict, (v2, full_expr_list, v6)) |> G.e )
          |> G.e)
  | `Ellips tok (* "..." *) -> G.Ellipsis (token env tok) |> G.e

and map_anon_choice_lit_value_3ef3d77 (env : env)
    (x : CST.anon_choice_lit_value_3ef3d77) : G.expr =
  match x with
  | `Lit_value x -> G.L (map_literal_value env x) |> G.e
  | `Anno_item x -> map_annotation_item env x
  | `Module_access x -> G.N (map_module_access env x) |> G.e
  | `COLONCOLON_module_access (v1, v2) ->
      let v1 = (* "::" *) token env v1 in
      let mod_name = map_module_access env v2 in
      G.N mod_name |> G.e

let rec map_bind (env : env) (x : CST.bind) : G.pattern =
  match x with
  | `Opt_mut_var_id (v1, v2) ->
      let name : G.ident = (* identifier *) str env v2 in
      let name_name = H2.name_of_id name in
      let name_pattern =
        match name with
        | "_", tok -> G.PatWildcard tok
        | _ -> G.PatId (name, G.empty_id_info ())
      in
      let final_pattern =
        match v1 with
        (* mut& *)
        | Some x ->
            G.PatDisj
              ( G.PatType
                  {
                    t = G.TyN name_name;
                    t_attrs = [ G.KeywordAttr (G.Mutable, token env x) ];
                  },
                name_pattern )
        | None -> name_pattern
      in
      final_pattern
  | `Bind_unpack (v1, v2, v3) ->
      let mod_name = map_module_access env v1 in
      let type_args = Option.map (map_type_arguments env) v2 in
      let type_arg_name = H2.add_type_args_opt_to_name mod_name type_args in
      let type_arg_kind =
        match type_args with
        | Some x -> G.TyApply (G.TyN type_arg_name |> G.t, x)
        | None -> G.TyN type_arg_name
      in
      let full_pattern =
        match v3 with
        (*bind_fields *)
        | Some x ->
            let lbrace, fields, rbrace = map_bind_fields env x in
            G.PatDisj
              ( G.PatType (type_arg_kind |> G.t),
                G.PatRecord (lbrace, fields, rbrace) )
        | None -> G.PatType (type_arg_kind |> G.t)
      in
      full_pattern
  | `Var_id_AT_bind (v1, v2, v3) ->
      let name : G.ident = (* identifier *) str env v1 in
      let name_pattern =
        match name with
        | "_", tok -> G.PatWildcard tok
        | _ -> G.PatId (name, G.empty_id_info ())
      in
      let v2 = (* "@" *) token env v2 in
      let bind = map_bind env v3 in
      G.PatDisj (name_pattern, bind)
  | `Ellips tok -> G.PatEllipsis (token env tok)

and map_bind_field (env : env) (x : CST.bind_field) =
  match x with
  | `Choice_opt_mut_choice_exp_opt_COLON_bind x -> (
      match x with
      | `Opt_mut_choice_exp_opt_COLON_bind (v1, v2, v3) ->
          let v1 =
            (* Todo deal with "mut" *)
            match v1 with
            | Some tok -> Some (token env tok)
            | None -> None
          in
          let expr =
            match v2 with
            | `Exp x -> map_expression env x
          in
          let dot_id =
            match expr.e with
            | G.N name ->
                let dot_id = H2.dotted_ident_of_name name in
                dot_id
            | G.L lit ->
                let lit_ident : G.ident = (G.show_literal lit, sc) in
                [ lit_ident ]
            | _ ->
                Log.err (fun m ->
                    m "Not a lit %s \n %s" (G.show_expr expr)
                      (G.show_expr_kind expr.e));

                failwith "Not a lit"
          in
          let out =
            match v3 with
            | Some (v1, v2) -> (dot_id, map_bind env v2)
            | None ->
                let ident = List.nth dot_id 0 in
                (dot_id, G.PatId (ident, G.empty_id_info ()))
          in
          out
      | `Spread_op tok ->
          let ident : G.ident = str env tok in
          let spread_tok = token env tok in
          ( [ ident ],
            G.IdSpecial (G.Spread, spread_tok) |> G.e |> H2.expr_to_pattern ))
  | `Ellips tok ->
      let ident = str env tok in
      ([ ident ], G.PatEllipsis (token env tok))

and map_bind_fields (env : env) (x : CST.bind_fields) =
  match x with
  | `Bind_posi_fields (v1, v2, v3, v4) ->
      let lp = (* "(" *) token env v1 in
      let bind_fields =
        List_.map
          (fun (v1, v2) ->
            let bind_fields = map_bind_field env v1 in
            let v2 = (* "," *) token env v2 in
            bind_fields (* Directly return a tuple instead of R.Tuple *))
          v2
      in
      let all_bind_fields =
        match v3 with
        | Some x ->
            bind_fields @ [ map_bind_field env x ] (* Directly use option *)
        | None -> bind_fields
      in
      let rp = (* ")" *) token env v4 in
      (lp, all_bind_fields, rp)
  | `Bind_named_fields (v1, v2, v3, v4) ->
      let lb = (* "{" *) token env v1 in
      let bind_fields =
        List_.map
          (fun (v1, v2) ->
            let v1 = map_bind_field env v1 in
            let v2 = (* "," *) token env v2 in
            v1 (* Directly return a tuple instead of R.Tuple *))
          v2
      in
      let all_bind_fields =
        match v3 with
        | Some x ->
            bind_fields @ [ map_bind_field env x ] (* Directly use option *)
        | None -> bind_fields
      in
      let rb = (* "}" *) token env v4 in
      (lb, all_bind_fields, rb)

and map_bind_list (env : env) (x : CST.bind_list) =
  match x with
  | `Bind x -> map_bind env x
  | `LPAR_rep_bind_COMMA_opt_bind_RPAR (v1, v2, v3, v4) ->
      let lp = token env v1 in
      (* "(" *)
      let bind_list =
        List_.map
          (fun (v1, v2) ->
            let bind = map_bind env v1 in
            let v2 = token env v2 in
            (* "," *)
            bind)
          v2
      in
      let all_binds =
        match v3 with
        | Some x -> bind_list @ [ map_bind env x ]
        | None -> bind_list
      in
      let rp = token env v4 in
      (* ")" *)
      G.PatTuple (lp, all_binds, rp)

and map_lambda_bindings (env : env) ((v1, v2, v3, v4) : CST.lambda_bindings) =
  let lp = token env v1 in
  (* "|" *)
  let binds =
    List_.map
      (fun (v1, v2) ->
        let binds = map_bind env v1 in
        let v2 = token env v2 in
        (* "," *)
        binds)
      v2
  in
  let all_binds =
    match v3 with
    | Some x -> binds @ [ map_bind env x ]
    | None -> binds
  in
  let rp = token env v4 in
  (* "|" *)
  let bind_params = List_.map (fun x -> G.ParamPattern x) all_binds in
  let params = (lp, bind_params, rp) in
  (lp, params, rp)

and map_function_parameters (env : env)
    ((v1, v2, v3, v4) : CST.function_parameters) : G.parameters =
  let lb = (* "(" *) token env v1 in
  let function_params =
    List_.map
      (fun (v1, v2) ->
        let function_params = map_function_parameter env v1 in
        let v2 = (* "," *) token env v2 in
        function_params)
      v2
  in
  let all_function_params =
    match v3 with
    | Some x -> function_params @ [ map_function_parameter env x ]
    | None -> function_params
  in
  let rb = (* ")" *) token env v4 in
  (lb, all_function_params, rb)

and map_spec_function_signature (env : env) attrs
    ((v1, v2, v3, v4) : CST.spec_function_signature) =
  let func_name = (* identifier *) str env v1 in
  let type_parameters =
    match v2 with
    | Some x -> Some (map_type_parameters env x)
    | None -> None
  in
  let function_parameters = map_function_parameters env v3 in
  let ret_type = map_ret_type env v4 in
  let type_parameters =
    match type_parameters with
    | Some params -> params
    | None -> (G.fake "", [], G.fake "")
  in
  let entity = G.basic_entity ~attrs ~tparams:type_parameters func_name in
  (entity, function_parameters, ret_type)

and map_macro_signature (env : env)
    ((v1, v2, v3, v4, v5, v6) : CST.macro_signature) =
  let attrs =
    match v1 with
    | Some x -> [ map_modifier env x ]
    | None -> []
  in
  let v2 = (* "fun" *) token env v2 in
  let ident = (* identifier *) str env v3 in
  let type_parameters =
    match v4 with
    | Some x -> map_type_parameters env x
    | None -> (G.fake "", [], G.fake "")
  in
  let function_parameters = map_function_parameters env v5 in
  let ret_type =
    v6
    |> Option.map (fun (_, v2) -> map_type_ env v2)
    |> Option.value ~default:(G.TyTuple (sc, [], sc) |> G.t)
  in
  (attrs, ident, type_parameters, function_parameters, ret_type)

and map_function_signature (env : env) attrs body
    ((v1, v2, v3, v4, v5, v6, v7) : CST.function_signature) =
  let mod_one =
    match v1 with
    | Some x -> [ map_modifier env x ]
    | None -> []
  in
  let mod_two =
    match v2 with
    | Some x -> [ map_modifier env x ]
    | None -> []
  in
  let fun_ = (* "fun" *) token env v3 in
  let name : G.ident = (* identifier *) str env v4 in
  let type_parameters =
    match v5 with
    | Some x -> Some (map_type_parameters env x)
    | None -> None
  in
  let function_parameters = map_function_parameters env v6 in
  let ret_type =
    v7
    |> Option.map (fun (_, v2) -> map_type_ env v2)
    |> Option.value ~default:(G.TyTuple (sc, [], sc) |> G.t)
  in
  let fn_def =
    {
      fkind = (G.Function, fun_);
      fparams = function_parameters;
      frettype = Some ret_type;
      fbody =
        (match body with
        | None -> G.FBNothing
        | Some x -> G.FBStmt x);
    }
  in
  let fn_ent =
    {
      name = G.EN (H2.name_of_id name);
      attrs = attrs @ mod_one @ mod_two;
      tparams = type_parameters;
    }
  in
  (fn_def, fn_ent)

and map_enum_variant (env : env) ((v1, v2) : CST.variant) =
  let ident = str env v1 in
  (* identifier *)
  let orTypeElm =
    match v2 with
    | Some x -> G.OrConstructor (ident, map_datatype_fields_type env x)
    | None -> G.OrEnum (ident, None)
  in
  orTypeElm

and map_enum_variants (env : env) ((v1, v2, v3, v4) : CST.enum_variants) :
    G.type_definition_kind =
  let lb = (* "{" *) token env v1 in
  let orTypeElms =
    List_.map
      (fun (v1, v2) ->
        let orTypeElm = map_enum_variant env v1 in
        let v2 = (* "," *) token env v2 in
        orTypeElm)
      v2
  in
  let all_variants =
    match v3 with
    | Some x -> orTypeElms @ [ map_enum_variant env x ]
    | None -> orTypeElms
  in
  let rb = (* "}" *) token env v4 in
  G.OrType all_variants

and map_arg_list (env : env) ((v1, v2, v3, v4) : CST.arg_list) =
  let lb = (* "(" *) token env v1 in
  let expr =
    List_.map
      (fun (v1, v2) ->
        let expr = map_expression env v1 in
        let v2 = (* "," *) token env v2 in
        expr)
      v2
  in
  let all_expr =
    match v3 with
    | Some x -> expr @ [ map_expression env x ]
    | None -> expr
  in
  let all_args = all_expr |> List_.map (fun arg -> G.Arg arg) in
  let rb = (* ")" *) token env v4 in
  (lb, all_args, rb)

and map_binary_expression (env : env) (x : CST.binary_expression) : G.expr =
  match x with
  | `Bin_oper_EQEQGT_bin_oper (v1, v2, v3) ->
      let op1 = map_binary_operand env v1 in
      let v2 = (* "==>" *) token env v2 in
      let op2 = map_binary_operand env v3 in
      (* Only used in specification *)
      (* Boolean implication, v1 ==> v2: !v1 || v2 *)
      let not_op1 = G.opcall (G.Not, v2) [ op1 ] in
      G.opcall (G.Or, v2) [ not_op1; op2 ]
  | `Bin_oper_BARBAR_bin_oper (v1, v2, v3) ->
      let op1 = map_binary_operand env v1 in
      let v2 = (* "||" *) token env v2 in
      let op2 = map_binary_operand env v3 in
      G.opcall (G.Or, v2) [ op1; op2 ]
  | `Bin_oper_AMPAMP_bin_oper (v1, v2, v3) ->
      let op1 = map_binary_operand env v1 in
      let v2 = (* "&&" *) token env v2 in
      let op2 = map_binary_operand env v3 in
      G.opcall (G.And, v2) [ op1; op2 ]
  | `Bin_oper_EQEQ_bin_oper (v1, v2, v3) ->
      let op1 = map_binary_operand env v1 in
      let v2 = (* "==" *) token env v2 in
      let op2 = map_binary_operand env v3 in
      G.opcall (G.Eq, v2) [ op1; op2 ]
  | `Bin_oper_BANGEQ_bin_oper (v1, v2, v3) ->
      let op1 = map_binary_operand env v1 in
      let v2 = (* "!=" *) token env v2 in
      let op2 = map_binary_operand env v3 in
      G.opcall (G.NotEq, v2) [ op1; op2 ]
  | `Bin_oper_LT_bin_oper (v1, v2, v3) ->
      let op1 = map_binary_operand env v1 in
      let v2 = (* "<" *) token env v2 in
      let op2 = map_binary_operand env v3 in
      G.opcall (G.Lt, v2) [ op1; op2 ]
  | `Bin_oper_GT_bin_oper (v1, v2, v3) ->
      let op1 = map_binary_operand env v1 in
      let v2 = (* ">" *) token env v2 in
      let op2 = map_binary_operand env v3 in
      G.opcall (G.Gt, v2) [ op1; op2 ]
  | `Bin_oper_GTEQ_bin_oper (v1, v2, v3) ->
      let op1 = map_binary_operand env v1 in
      let v2 = (* ">=" *) token env v2 in
      let op2 = map_binary_operand env v3 in
      G.opcall (G.GtE, v2) [ op1; op2 ]
  | `Bin_oper_LTEQ_bin_oper (v1, v2, v3) ->
      let op1 = map_binary_operand env v1 in
      let v2 = (* "<=" *) token env v2 in
      let op2 = map_binary_operand env v3 in
      G.opcall (G.LtE, v2) [ op1; op2 ]
  | `Bin_oper_DOTDOT_bin_oper (v1, v2, v3) ->
      let op1 = map_binary_operand env v1 in
      let v2 = (* .. *) token env v2 in
      let op2 = map_binary_operand env v3 in
      G.opcall (G.Range, v2) [ op1; op2 ]
  | `Bin_oper_BAR_bin_oper (v1, v2, v3) ->
      let op1 = map_binary_operand env v1 in
      let v2 = (* | *) token env v2 in
      let op2 = map_binary_operand env v3 in
      G.opcall (G.BitOr, v2) [ op1; op2 ]
  | `Bin_oper_HAT_bin_oper (v1, v2, v3) ->
      let op1 = map_binary_operand env v1 in
      let v2 = (* ^ *) token env v2 in
      let op2 = map_binary_operand env v3 in
      G.opcall (G.BitXor, v2) [ op1; op2 ]
  | `Bin_oper_AMP_bin_oper (v1, v2, v3) ->
      let op1 = map_binary_operand env v1 in
      let v2 = (* & *) token env v2 in
      let op2 = map_binary_operand env v3 in
      G.opcall (G.BitAnd, v2) [ op1; op2 ]
  | `Bin_oper_LTLT_bin_oper (v1, v2, v3) ->
      let op1 = map_binary_operand env v1 in
      let v2 = (* << *) token env v2 in
      let op2 = map_binary_operand env v3 in
      G.opcall (G.LSL, v2) [ op1; op2 ]
  | `Bin_oper_GTGT_bin_oper (v1, v2, v3) ->
      let op1 = map_binary_operand env v1 in
      let v2 = (* >> *) token env v2 in
      let op2 = map_binary_operand env v3 in
      G.opcall (G.LSR, v2) [ op1; op2 ]
  | `Bin_oper_PLUS_bin_oper (v1, v2, v3) ->
      let op1 = map_binary_operand env v1 in
      let v2 = (* "+" *) token env v2 in
      let op2 = map_binary_operand env v3 in
      G.opcall (G.Plus, v2) [ op1; op2 ]
  | `Bin_oper_DASH_bin_oper (v1, v2, v3) ->
      let op1 = map_binary_operand env v1 in
      let v2 = (* "-" *) token env v2 in
      let op2 = map_binary_operand env v3 in
      G.opcall (G.Minus, v2) [ op1; op2 ]
  | `Bin_oper_STAR_bin_oper (v1, v2, v3) ->
      let op1 = map_binary_operand env v1 in
      let v2 = (* "*" *) token env v2 in
      let op2 = map_binary_operand env v3 in
      G.opcall (G.Mult, v2) [ op1; op2 ]
  | `Bin_oper_SLASH_bin_oper (v1, v2, v3) ->
      let op1 = map_binary_operand env v1 in
      let v2 = (* "/" *) token env v2 in
      let op2 = map_binary_operand env v3 in
      G.opcall (G.Div, v2) [ op1; op2 ]
  | `Bin_oper_PERC_bin_oper (v1, v2, v3) ->
      let op1 = map_binary_operand env v1 in
      let v2 = (* "%" *) token env v2 in
      let op2 = map_binary_operand env v3 in
      G.opcall (G.Mod, v2) [ op1; op2 ]

and map_binary_operand (env : env) (x : CST.binary_operand) : G.expr =
  match x with
  | `Un_exp x -> map_unary_expression env x
  | `Bin_exp x -> map_binary_expression env x
  | `Cast_exp x -> map_cast_expression env x

and map_block_item (env : env) (x : CST.block_item) =
  match x with
  | `Choice_exp_SEMI (v1, v2) -> (
      let expr =
        match v1 with
        | `Exp x -> map_expression env x
        | `Let_stmt x -> map_let_expr env x
      in
      let sc = (* ";" *) token env v2 in
      (* Simplify nested statements *)
      match expr with
      | { G.e = G.StmtExpr stmt; _ } -> stmt
      | expr -> G.ExprStmt (expr, sc) |> G.s)
  | `Ellips tok -> G.exprstmt (G.Ellipsis (token env tok) |> G.e)

and map_borrow_expression (env : env) ((v1, v2) : CST.borrow_expression) =
  let ref_tok, attrs = map_mutable env v1 in
  let expr = map_expression env v2 in
  (expr, attrs)

and map_cast_expression (env : env) ((v1, v2, v3) : CST.cast_expression) :
    G.expr =
  let v1 = map_expression env v1 in
  let v2 = (* "as" *) token env v2 in
  let v3 = map_type_ env v3 in
  G.Cast (v3, v2, v1) |> G.e

and map_deep_ellipsis (env : env) ((v1, v2, v3) : CST.deep_ellipsis) =
  let v1 = (* "<..." *) token env v1 in
  let v2 = map_expression env v2 in
  let v3 = (* "...>" *) token env v3 in
  G.DeepEllipsis (v1, v2, v3) |> G.e

and map_dereference_expression (env : env)
    ((v1, v2) : CST.dereference_expression) : G.expr =
  let v1 = (* "*" *) token env v1 in
  let expr = map_expression env v2 in
  G.DeRef (v1, expr) |> G.e

and map_field_access_ellipsis_expr (env : env)
    ((v1, v2, v3) : CST.field_access_ellipsis_expr) =
  let v1 = map_dot_or_index_chain env v1 in
  let v2 = (* "." *) token env v2 in
  let v3 = (* "..." *) token env v3 in
  G.DotAccessEllipsis (v1, v3) |> G.e

and map_access_field (env : env) ((v1, v2, v3) : CST.access_field) =
  let expr = map_dot_or_index_chain env v1 in
  let dot = (* "." *) token env v2 in
  let field_exp =
    match v3 with
    | `Exp x -> map_expression env x
  in
  let field = G.FDynamic field_exp in
  G.DotAccess (expr, dot, field) |> G.e

and map_receiver_call (env : env) ((v1, v2, v3, v4) : CST.receiver_call) =
  let expr_ = map_dot_or_index_chain env v1 in
  let dot = (* "." *) token env v2 in
  let func_name = (* identifier *) str env v3 in
  let func_expr =
    G.DotAccess (expr_, dot, G.FN (H2.name_of_id func_name)) |> G.e
  in
  let args = map_arg_list env v4 in
  G.Call (func_expr, args) |> G.e

and map_receiver_macro_call (env : env)
    ((v1, v2, v3, v4, v5, v6) : CST.receiver_macro_call) =
  let expr_ = map_dot_or_index_chain env v1 in
  let dot = (* "." *) token env v2 in
  let func_name_str, func_name_tok = (* identifier *) str env v3 in
  let bang_str, bang_tok = (* "!" *) str env v4 in
  let type_args = Option.map (map_type_arguments env) v5 in
  let func_name_full_str = func_name_str ^ "!" in
  let func_name_full_tok = Tok.combine_toks func_name_tok [ bang_tok ] in
  let func_expr =
    G.DotAccess
      ( expr_,
        dot,
        G.FN
          (H2.add_type_args_opt_to_name
             (H2.name_of_id (func_name_full_str, func_name_full_tok))
             type_args) )
    |> G.e
  in
  let args = map_arg_list env v6 in
  G.Call (func_expr, args) |> G.e

and map_dot_or_index_chain (env : env) (x : CST.dot_or_index_chain) =
  match x with
  | `Choice_access_field x -> (
      match x with
      | `Access_field x -> map_access_field env x
      | `Rece_call x -> map_receiver_call env x
      | `Rece_macro_call x -> map_receiver_macro_call env x
      | `Index_exp x -> map_index_expression env x
      | `Exp_term x -> map_expression_term env x)
  | `Field_access_ellips_expr x -> map_field_access_ellipsis_expr env x

and map_exp_field (env : env) (x : CST.exp_field) : G.argument =
  match x with
  | `Field_id_opt_COLON_exp (v1, v2) ->
      let field = (* identifier *) str env v1 in
      let argument =
        match v2 with
        | Some (v1, v2) ->
            let v1 = (* ":" *) token env v1 in
            let value = map_expression env v2 in
            G.ArgKwd (field, value)
        | None ->
            let rhs = G.N (H2.name_of_id field) |> G.e in
            G.ArgKwd (field, rhs)
      in
      argument
  | `Ellips tok -> G.Arg (G.Ellipsis (token env tok) |> G.e)

(*
and  map_call_expression (env : env) ((v1, v2, v3) : CST.call_expression) =
  let mod_name = map_module_access env v1 in
  let type_args = Option.map (map_type_arguments env) v2 in
  let call_args = map_arg_list env v3 in
  let typed_func = H2.add_type_args_opt_to_name mod_name type_args in
  G.Call (G.N typed_func |> G.e, call_args) |> G.e

and  map_macro_call_expression (env : env) ((v1, v2, v3) : CST.macro_call_expression) =
  let mod_name = map_module_access env v1 in
  let type_args = Option.map (map_type_arguments env) v2 in
  let call_args = map_arg_list env v3 in
  let typed_func = H2.add_type_args_opt_to_name mod_name type_args in
  G.Call (G.N typed_func |> G.e, call_args) |> G.e*)
and map_semgrep_metavar_ellipsis (env : env) (x : CST.semgrep_metavar_ellipsis)
    =
  let ellipsis = token env x in
  G.Ellipsis ellipsis |> G.e

and map_typed_metavariable (env : env)
    ((v1, v2, v3, v4, v5) : CST.typed_metavariable) =
  let v1 = (* "(" *) token env v1 in
  let ident : G.ident = (* identifier *) str env v2 in
  let v3 = (* ":" *) token env v3 in
  let type_ = map_type_ env v4 in
  let v5 = (* ")" *) token env v5 in
  G.TypedMetavar (ident, v3, type_) |> G.e

and map_expression (env : env) (x : CST.expression) : G.expr =
  match x with
  | `Choice_call_exp x -> (
      match x with
      | `Call_exp (v1, v2, v3) ->
          let name = map_module_access env v1 in
          let type_args =
            match v2 with
            | Some x -> Some (map_type_arguments env x)
            | None -> None
          in
          let call_args = map_arg_list env v3 in
          let typed_func = H2.add_type_args_opt_to_name name type_args in
          G.Call (G.N typed_func |> G.e, call_args) |> G.e
      | `Macro_call_exp (v1, v2, v3) ->
          let name, bang = map_macro_module_access env v1 in
          let name_name =
            match name with
            | G.Id ((s, i1), info) ->
                G.Id ((s ^ "!", Tok.combine_toks i1 [ bang ]), info)
            | G.IdQualified ({ name_last = (s, i1), topt; _ } as qualified_info)
              ->
                let s, t = (s ^ "!", Tok.combine_toks i1 [ bang ]) in
                G.IdQualified { qualified_info with name_last = ((s, t), topt) }
          in
          let _type_args =
            (*todo use*)
            match v2 with
            | Some x -> Some (map_type_arguments env x)
            | None -> None
          in
          let call_args = map_arg_list env v3 in
          G.Call (G.N name_name |> G.e, call_args) |> G.e
      | `Lambda_exp (v1, v2, v3) ->
          let lp, params, _ = map_lambda_bindings env v1 in
          let ret_type =
            match v2 with
            | Some (v1, v2) ->
                Some
                  (let v1 = (* "->" *) token env v1 in
                   let type_ = map_type_ env v2 in
                   type_)
            | None -> None
          in
          let body = map_expression env v3 in
          let func_def =
            {
              fkind = (G.LambdaKind, lp);
              fparams = params;
              frettype = ret_type;
              fbody = G.FBExpr body;
            }
          in
          G.Lambda func_def |> G.e
      | `If_exp x -> map_if_expression env x
      | `While_exp (v1, v2, v3, v4, v5) ->
          let while_ = (* "while" *) token env v1 in
          let v2 = (* "(" *) token env v2 in
          let cond = map_expression env v3 in
          let v4 = (* ")" *) token env v4 in
          let body = map_expression env v5 in
          let while_stmt =
            G.While (while_, G.Cond cond, body |> G.exprstmt) |> G.s
          in
          G.stmt_to_expr while_stmt
      | `Loop_exp (v1, v2) ->
          let loop_ = (* "loop" *) token env v1 in
          let body = map_expression env v2 in
          let cond = G.L (G.Bool (true, G.fake "true")) |> G.e in
          let loop_stmt =
            G.While (loop_, G.Cond cond, body |> G.exprstmt) |> G.s
          in
          G.stmt_to_expr loop_stmt
      | `Ret_exp (v1, v2, v3) ->
          let ret = (* "return" *) token env v1 in
          let ret_label =
            (* todo what do with label?*)
            match v2 with
            | Some x -> Some (map_label env x)
            | None -> None
          in
          let ret_exp =
            match v3 with
            | Some x ->
                Some
                  (match x with
                  | `Exp_term x -> map_expression_term env x
                  | `Exp x -> map_expression env x)
            | None -> None
          in
          let ret_st = Return (ret, ret_exp, sc) |> G.s in
          G.stmt_to_expr ret_st
      | `Abort_exp (v1, v2) ->
          let v1 = (* "abort" *) str env v1 in
          let v2 = map_expression env v2 in
          G.Call (G.N (H2.name_of_id v1) |> G.e, fb [ G.Arg v2 ]) |> G.e
      | `Assign_exp (v1, v2, v3) ->
          let lhs = map_unary_expression env v1 in
          let v2 = (* "=" *) token env v2 in
          let rhs = map_expression env v3 in
          G.Assign (lhs, v2, rhs) |> G.e
      | `Un_exp x -> map_unary_expression env x
      | `Bin_exp x -> map_binary_expression env x
      | `Cast_exp x -> map_cast_expression env x
      | `Quan_exp (v1, v2, v3, v4, v5) ->
          let quant = map_reserved_identifier env v1 in
          let binds = map_quantifier_bindings env v2 in
          let constraint_ =
            Option.map
              (fun (v1, v2) ->
                let v1 = (* "where" *) token env v1 in
                let v2 = map_expression env v2 in
                G.E v2)
              v3
            |> Option.to_list
          in
          let v4 = (* ":" *) token env v4 in
          let invariant = [ G.E (map_expression env v5) ] in
          (*G.OtherPat (("Quantifier", quant), binds @ constraint_ @ invariant)|> H2.pattern_to_expr*)
          G.OtherStmt (G.OS_Todo, binds @ constraint_ @ invariant)
          |> G.s |> G.stmt_to_expr
      | `Iden_exp (v1, v2) ->
          let v1 = map_block_identifier env v1 in
          let v2 = map_expression env v2 in
          v2 (*todo do right*)
      | `Match_exp x -> map_match_expression env x
      | `Vec_exp x -> map_vector_expression env x)
  | `Ellips tok -> (* "..." *) G.Ellipsis (token env tok) |> G.e
  | `Deep_ellips x -> map_deep_ellipsis env x

(*| `Semg_meta_ellips x -> (map_semgrep_metavar_ellipsis env x)
  | `Typed_meta x -> (map_typed_metavariable env x)*)
and map_name_expression (env : env) ((v1, v2) : CST.name_expression) =
  let name = map_module_access env v1 in
  let type_args =
    match v2 with
    | Some x -> Some (map_type_arguments env x)
    | None -> None
  in
  let typed_name = H2.add_type_args_opt_to_name name type_args in
  G.N typed_name |> G.e

and map_expression_term (env : env) (x : CST.expression_term) =
  match x with
  | `Choice_brk_exp x -> (
      match x with
      | `Brk_exp (v1, v2, v3) ->
          let v1 = (* "break" *) token env v1 in
          let label =
            match v2 with
            | Some x -> G.LId (map_label env x)
            | None -> G.LNone
          in
          let _expr =
            (*TOdo use this*)
            match v3 with
            | Some x -> Some (map_expression_term env x)
            | None -> None
          in
          G.Break (v1, label, sc) |> G.s |> G.stmt_to_expr
      | `Cont_exp (v1, v2) ->
          let v1 = (* "continue" *) token env v1 in
          let label =
            match v2 with
            | Some x -> G.LId (map_label env x)
            | None -> G.LNone
          in
          G.Continue (v1, label, sc) |> G.s |> G.stmt_to_expr
      | `Name_exp x -> map_name_expression env x
      | `Call_exp (v1, v2, v3) ->
          let name = map_module_access env v1 in
          let type_args =
            match v2 with
            | Some x -> Some (map_type_arguments env x)
            | None -> None
          in
          let call_args = map_arg_list env v3 in
          let typed_func = H2.add_type_args_opt_to_name name type_args in
          G.Call (G.N typed_func |> G.e, call_args) |> G.e
      | `Macro_call_exp (v1, v2, v3) ->
          let name, bang = map_macro_module_access env v1 in
          let name_name =
            match name with
            | G.Id ((s, i1), info) ->
                G.Id ((s ^ "!", Tok.combine_toks i1 [ bang ]), info)
            | G.IdQualified ({ name_last = (s, i1), topt; _ } as qualified_info)
              ->
                let s, t = (s ^ "!", Tok.combine_toks i1 [ bang ]) in
                G.IdQualified { qualified_info with name_last = ((s, t), topt) }
          in
          let _type_args =
            (*todo use*)
            match v2 with
            | Some x -> Some (map_type_arguments env x)
            | None -> None
          in
          let call_args = map_arg_list env v3 in
          G.Call (G.N name_name |> G.e, call_args) |> G.e
      | `Pack_exp (v1, v2, v3) ->
          let name = map_module_access env v1 in
          let type_args =
            match v2 with
            | Some x -> Some (map_type_arguments env x)
            | None -> None
          in
          let fields = map_field_initialize_list env v3 in
          let struct_type =
            G.TyN (H2.add_type_args_opt_to_name name type_args) |> G.t
          in
          G.New (G.fake "new", struct_type, G.empty_id_info (), fields) |> G.e
      | `Lit_value x -> G.L (map_literal_value env x) |> G.e
      | `Unit_exp (v1, v2) ->
          let lp = (* "(" *) token env v1 in
          let v2 = (* ")" *) token env v2 in
          G.L (G.Unit lp) |> G.e
      | `Exp_list (v1, v2, v3, v4, v5) ->
          let v1 = (* "(" *) token env v1 in
          let expr = map_expression env v2 in
          let exprs =
            List_.map
              (fun (v1, v2) ->
                let v1 = (* "," *) token env v1 in
                let expr = map_expression env v2 in
                expr)
              v3
          in
          let v4 =
            match v4 with
            | Some tok -> Some ((* "," *) token env tok)
            | None -> None
          in
          let v5 = (* ")" *) token env v5 in
          let all_exprs = expr :: exprs in
          G.Seq all_exprs |> G.e (*todo is this right?*)
      | `Anno_exp (v1, v2, v3, v4, v5) ->
          let v1 = (* "(" *) token env v1 in
          let expr = map_expression env v2 in
          let v3 = (* ":" *) token env v3 in
          let type_ = map_type_ env v4 in
          let v5 = (* ")" *) token env v5 in
          G.Cast (type_, v3, expr) |> G.e
      | `Blk x -> map_block env x |> G.stmt_to_expr
      | `Spec_blk x -> map_spec_block env x |> G.stmt_to_expr
      | `If_exp x -> map_if_expression env x
      | `Vec_exp x -> map_vector_expression env x
      | `Match_exp x -> map_match_expression env x)
  | `Typed_meta (v1, v2, v3, v4, v5) ->
      let v1 = (* "(" *) token env v1 in
      let ident = (* identifier *) str env v2 in
      let _colon = (* ":" *) token env v3 in
      let var_type = map_type_ env v4 in
      let v5 = (* ")" *) token env v5 in
      (* Typed metavariables and cast expressions are ambiguous in the grammar *)
      let expr =
        if AST_generic.is_metavar_name (H2.str_of_ident ident) then
          G.TypedMetavar (ident, _colon, var_type)
        else G.Cast (var_type, _colon, G.N (H2.name_of_id ident) |> G.e)
      in
      expr |> G.e
  | `Ellips tok -> (* "..." *) G.Ellipsis (token env tok) |> G.e
  | `Deep_ellips x -> map_deep_ellipsis env x
(*| `Typed_meta x -> map_typed_metavariable env x*)

and map_field_initialize_list (env : env)
    ((v1, v2, v3, v4) : CST.field_initialize_list) =
  let lb = (* "{" *) token env v1 in
  let arguments =
    List_.map
      (fun (v1, v2) ->
        let arguments = map_exp_field env v1 in
        let v2 = (* "," *) token env v2 in
        arguments)
      v2
  in
  let all_arguments =
    match v3 with
    | Some x -> arguments @ [ map_exp_field env x ]
    | None -> arguments
  in
  let rb = (* "}" *) token env v4 in
  (lb, all_arguments, rb)

and map_if_expression (env : env) ((v1, v2, v3, v4, v5, v6) : CST.if_expression)
    : G.expr =
  let _if = (* "if" *) token env v1 in
  let v2 = (* "(" *) token env v2 in
  let cond = map_expression env v3 in
  let v4 = (* ")" *) token env v4 in
  let then_ = map_expression env v5 in
  let then_statement = G.ExprStmt (then_, sc) |> G.s in
  let else_statement =
    match v6 with
    | Some (v1, v2) ->
        Some
          (let v1 = (* "else" *) token env v1 in
           let else_expr = map_expression env v2 in
           G.ExprStmt (else_expr, sc) |> G.s)
    | None -> None
  in
  let if_stmt =
    G.If (_if, G.Cond cond, then_statement, else_statement) |> G.s
  in
  G.stmt_to_expr if_stmt

and map_block_ (env : env) ((v1, v2, v3, v4, v5) : CST.block) : G.stmt list =
  let lb = (* "{" *) token env v1 in
  let uses =
    v2
    |> List_.map (map_use_declaration env)
    |> List_.flatten
    |> List_.map (fun x -> G.DirectiveStmt x |> G.s)
  in
  let stmts = List_.map (map_block_item env) v3 in
  (*todo map to statmet*)
  let final_expr =
    v4
    |> Option.map (map_expression env)
    |> Option.to_list
    |> List_.map (fun x -> G.ExprStmt (x, sc) |> G.s)
  in
  let rb = (* "}" *) token env v5 in
  uses @ stmts @ final_expr

and map_block (env : env) ((v1, v2, v3, v4, v5) : CST.block) : G.stmt =
  let block_stmts = map_block_ env (v1, v2, v3, v4, v5) in
  G.Block (G.fake "{", block_stmts, G.fake "}") |> G.s

and map_index_expression (env : env)
    ((v1, v2, v3, v4, v5) : CST.index_expression) =
  (*match x with
    | `Exp_term_LBRACK_rep_exp_COMMA_opt_exp_RBRACK (v1, v2, v3, v4, v5) ->
        let arr = map_expression_term env v1 in
        let v2 = (* "[" *) token env v2 in
        let index_exp =
          List.map (fun (v1, v2) ->
            let v1 = map_expression env v1 in
            let v2 = (* "," *) token env v2 in
            v1
          ) v3
        in
        let all_index_exp =
          match v4 with
          | Some x -> (map_expression env x) :: index_exp
          | None -> index_exp
        in
        let v5 = (* "]" *) token env v5 in
        let array_acceses = all_index_exp |>
          List.map (fun x -> G.ArrayAccess (arr, (v2, x, v5)) |> G.e )  in
        G.Seq array_acceses |> G.e
    | `Ellips tok -> (* "..." *) G.Ellipsis (token env tok) |> G.e*)
  let arr = map_dot_or_index_chain env v1 in
  let v2 = (* "[" *) token env v2 in
  let index_exp =
    List_.map
      (fun (v1, v2) ->
        let v1 = map_expression env v1 in
        let v2 = (* "," *) token env v2 in
        v1)
      v3
  in
  let all_index_exp =
    match v4 with
    | Some x -> index_exp @ [ map_expression env x ]
    | None -> index_exp
  in
  let v5 = (* "]" *) token env v5 in
  let array_acceses =
    all_index_exp
    |> List_.map (fun x -> G.ArrayAccess (arr, (v2, x, v5)) |> G.e)
  in
  G.Seq array_acceses |> G.e

and map_match_arm (env : env) ((v1, v2, v3, v4) : CST.match_arm) :
    G.pattern * G.expr =
  let patr = map_bind_list env v1 in
  let patr_full =
    match v2 with
    | Some (v1, v2) ->
        let v1 = (* "if" *) token env v1 in
        let v2 = map_expression env v2 in
        G.PatWhen (patr, v2)
    | None -> patr
  in
  let v3 = (* "=>" *) token env v3 in
  let expr = map_expression env v4 in
  (patr_full, expr)

and map_match_body (env : env) ((v1, v2, v3, v4) : CST.match_body) :
    (G.pattern * G.expr) list =
  let v1 = (* "{" *) token env v1 in
  let match_arms =
    List_.map
      (fun (v1, v2) ->
        let v1 = map_match_arm env v1 in
        let v2 = (* "," *) token env v2 in
        v1)
      v2
  in
  let all_match_arms =
    match v3 with
    | Some x -> match_arms @ [ map_match_arm env x ]
    | None -> match_arms
  in
  let v4 = (* "}" *) token env v4 in
  all_match_arms

and map_match_expression (env : env)
    ((v1, v2, v3, v4, v5) : CST.match_expression) : G.expr =
  let mat = (* "match" *) token env v1 in
  let v2 = (* "(" *) token env v2 in
  let expr = map_expression env v3 in
  let v4 = (* ")" *) token env v4 in
  let v5 = map_match_body env v5 in
  let actions = v5 |> List_.map G.case_of_pat_and_expr in
  let st = G.Switch (mat, Some (G.Cond expr), actions) |> G.s in
  G.stmt_to_expr st

and map_quantifier_binding (env : env) (x : CST.quantifier_binding) =
  match x with
  | `Id_COLON_type (v1, v2, v3) ->
      let ident = (* identifier *) str env v1 in
      let v2 = (* ":" *) token env v2 in
      let bind_type = map_type_ env v3 in
      G.PatTyped (G.PatId (ident, G.empty_id_info ()), bind_type)
  | `Id_in_exp (v1, v2, v3) ->
      let ident : G.ident = (* identifier *) str env v1 in
      let v2 = (* "in" *) token env v2 in
      let expr = map_expression env v3 in
      (* TODO: find a better way to place it *)
      G.PatWhen (G.PatId (ident, G.empty_id_info ()), expr)

and map_quantifier_bindings (env : env) ((v1, v2, v3) : CST.quantifier_bindings)
    =
  let bind_first = G.P (map_quantifier_binding env v1) in
  let bind_rest =
    List_.map
      (fun (v1, v2) ->
        let v1 = (* "," *) token env v1 in
        let v2 = map_quantifier_binding env v2 in
        G.P v2)
      v2
  in
  let binds = bind_first :: bind_rest in
  let v3 =
    match v3 with
    | Some tok -> Some ((* "," *) token env tok)
    | None -> None
  in
  binds

and map_spec_block (env : env) ((v1, v2) : CST.spec_block) : G.stmt =
  let v1 = (* "spec" *) token env v1 in
  let body =
    match v2 with
    | `Opt_spec_blk_target_spec_body (v1, v2) ->
        let _targets =
          (*Todo use targets*)
          match v1 with
          | Some x -> Some (map_spec_block_target env x)
          | None -> None
        in
        let block_stmt = map_spec_body env v2 in
        block_stmt
    | `Spec_func x -> map_spec_function env x
  in

  body

and map_spec_apply (env : env)
    ((v1, v2, v3, v4, v5, v6, v7, v8) : CST.spec_apply) =
  let apply_ = (* "apply" *) token env v1 in
  let apply_expr = G.E (map_expression env v2) in
  let to_ = (* "to" *) G.Tk (token env v3) in
  let pattern = map_spec_apply_pattern env v4 in
  let patterns =
    List_.map
      (fun (v1, v2) ->
        let v1 = (* "," *) token env v1 in
        let pattern = map_spec_apply_pattern env v2 in
        pattern)
      v5
  in
  let all_patterns = pattern :: patterns |> List_.map (fun x -> G.P x) in
  let v6 =
    match v6 with
    | Some tok -> Some ((* "," *) token env tok)
    | None -> None
  in
  let except_patterns =
    match v7 with
    | Some (v1, v2, v3, v4) ->
        let except_ = (* "except" *) G.Tk (token env v1) in
        let pattern = map_spec_apply_pattern env v2 in
        let patterns =
          List_.map
            (fun (v1, v2) ->
              let v1 = (* "," *) token env v1 in
              let pattern = map_spec_apply_pattern env v2 in
              pattern)
            v3
        in
        let all_patterns = pattern :: patterns |> List_.map (fun x -> G.P x) in
        let v4 =
          match v4 with
          | Some tok -> Some ((* "," *) token env tok)
          | None -> None
        in
        [ G.Anys (except_ :: all_patterns) ]
    | None -> []
  in
  let v8 = (* ";" *) token env v8 in
  G.OtherPat
    (("SpecApply", apply_), [ apply_expr; to_ ] @ all_patterns @ except_patterns)
  |> H2.pattern_to_expr |> G.exprstmt

and map_spec_block_memeber (env : env) (x : CST.spec_block_memeber) =
  let ret =
    match x with
    | `Choice_spec_inva x -> (
        match x with
        | `Spec_inva x -> map_spec_invariant env x
        | `Spec_cond x -> map_spec_condition env x
        | `Spec_func x -> map_spec_function env x
        | `Spec_var x -> map_spec_variable env x
        | `Spec_incl x -> map_spec_include env x
        | `Spec_apply x -> map_spec_apply env x
        | `Spec_pragma x -> map_spec_pragma env x |> G.s
        | `Spec_let x -> map_spec_let env x)
    | `Ellips tok -> (* "..." *) G.Ellipsis (token env tok) |> G.e |> G.exprstmt
  in
  ret

and map_spec_body (env : env) ((v1, v2, v3, v4) : CST.spec_body) : G.stmt =
  let v1 = (* "{" *) token env v1 in
  let uses =
    List_.map (map_use_declaration env) v2
    |> List_.flatten
    |> List_.map (fun x -> G.DirectiveStmt x |> G.s)
  in
  let members = List_.map (map_spec_block_memeber env) v3 in

  let v4 = (* "}" *) token env v4 in
  G.Block (v1, uses @ members, v4) |> G.s

and map_spec_condition (env : env) (x : CST.spec_condition) =
  match x with
  | `Spec_cond_ (v1, v2, v3, v4) ->
      let cond =
        match v1 with
        | `Spec_cond_kind x -> map_spec_condition_kind env x
        | `Requis_opt_module (v1, v2) ->
            let requires = (* "requires" *) str env v1 in
            let requires_str, requires_tok = requires in
            let requres_and =
              match v2 with
              | Some x ->
                  let module_str, _ = (* "module" *) str env x in
                  G.fake (requires_str ^ " " ^ module_str)
              | None -> requires_tok
            in
            requres_and
      in
      let props =
        Option.map (map_condition_properties env) v2
        |> Option.to_list
        |> List_.map (fun x -> G.OtherArg (("ConditionProps", sc), [ G.At x ]))
      in
      let arg = G.Arg (map_expression env v3) in
      let v4 = (* ";" *) token env v4 in
      G.Assert (cond, (sc, props @ [ arg ], sc), sc) |> G.s
  | `Spec_abort_if (v1, v2, v3, v4, v5) ->
      let cond = (* "aborts_if" *) G.I (str env v1) in
      let props =
        match v2 with
        | Some x -> [ G.At (map_condition_properties env x) ]
        | None -> []
      in
      let expr = G.E (map_expression env v3) in
      let with_value =
        v4
        |> Option.map (fun (v1, v2) ->
               let v1 = (* "with" *) G.Tk (token env v1) in
               let v2 = G.E (map_expression env v2) in
               G.Anys [ v1; v2 ])
        |> Option.to_list
      in
      G.OtherStmt (G.OS_Todo, (cond :: props) @ (expr :: with_value)) |> G.s
  | `Spec_abort_with_or_modifs (v1, v2, v3, v4, v5, v6) ->
      let cond =
        match v1 with
        | `Aborts_with tok -> (* "aborts_with" *) str env tok
        | `Modifs tok -> (* "modifies" *) str env tok
      in

      let props =
        Option.map (map_condition_properties env) v2
        |> Option.to_list
        |> List_.map (fun x -> G.At x)
      in
      let first = G.E (map_expression env v3) in
      let rest =
        List_.map
          (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_expression env v2 in
            G.E v2)
          v4
      in
      G.OtherStmt (G.OS_Todo, (G.I cond :: props) @ (first :: rest)) |> G.s

and map_spec_function (env : env) (x : CST.spec_function) : G.stmt =
  match x with
  | `Native_spec_func (v1, v2, v3, v4) ->
      let native = (* "native" *) token env v1 in
      let attr = G.KeywordAttr (G.Extern, native) in
      let fun_lit = (* "fun" *) token env v2 in
      let entity, params, ret_type =
        map_spec_function_signature env [ attr ] v3
      in
      let semi = (* ";" *) token env v4 in
      let func_def =
        {
          fkind = (G.Function, fun_lit);
          fparams = params;
          frettype = Some ret_type;
          fbody = G.FBNothing;
        }
      in
      G.DefStmt (entity, G.FuncDef func_def) |> G.s
  | `Usual_spec_func (v1, v2, v3) ->
      let fun_lit = (* "fun" *) token env v1 in
      let entity, params, ret_type = map_spec_function_signature env [] v2 in
      let body = map_block env v3 in
      let func_def =
        {
          fkind = (G.Function, fun_lit);
          fparams = params;
          frettype = Some ret_type;
          fbody = G.FBStmt body;
        }
      in
      G.DefStmt (entity, G.FuncDef func_def) |> G.s
  | `Unin_spec_func (v1, v2, v3) ->
      let fun_lit = (* "fun" *) token env v1 in
      let entity, params, ret_type = map_spec_function_signature env [] v2 in
      let semi = (* ";" *) token env v3 in
      let func_def =
        {
          fkind = (G.Function, fun_lit);
          fparams = params;
          frettype = Some ret_type;
          fbody = G.FBNothing;
        }
      in
      G.DefStmt (entity, G.FuncDef func_def) |> G.s

and map_spec_include (env : env) ((v1, v2, v3) : CST.spec_include) =
  let incl = (* "include" *) G.I (str env v1) in
  let expr = G.E (map_expression env v2) in
  let v3 = (* ";" *) token env v3 in
  G.OtherStmt (G.OS_Todo, [ incl ] @ [ expr ]) |> G.s

and map_spec_invariant (env : env) ((v1, v2, v3, v4, v5) : CST.spec_invariant) =
  let inv = (* "invariant" *) G.I (str env v1) in
  let update_or =
    match v2 with
    | Some x -> (
        match x with
        | `Update tok -> G.Tk (* "update" *) (token env tok)
        | `Pack tok -> G.Tk (* "pack" *) (token env tok)
        | `Unpack tok -> G.Tk (* "unpack" *) (token env tok)
        | `Module tok -> G.Tk (* "module" *) (token env tok))
    | None -> G.Tk (G.fake "")
  in
  let props =
    Option.map (map_condition_properties env) v3
    |> Option.to_list
    |> List_.map (fun x -> G.At x)
  in
  let expr = G.E (map_expression env v4) in
  let v5 = (* ";" *) token env v5 in
  G.OtherStmt (G.OS_Todo, [ inv ] @ [ update_or ] @ props @ [ expr ]) |> G.s

and map_spec_let (env : env) ((v1, v2, v3, v4, v5, v6) : CST.spec_let) =
  let let_ = (* "let" *) token env v1 in
  let post_TODO = Option.map ((* "post" *) token env) v2 in
  let var_name = (* identifier *) str env v3 in
  let v4 = (* "=" *) token env v4 in
  let value = map_expression env v5 in
  let v6 = (* ";" *) token env v6 in
  let def = G.VarDef { vinit = Some value; vtype = None; vtok = Some let_ } in
  let entity =
    { name = G.EN (H2.name_of_id var_name); attrs = []; tparams = None }
  in
  G.DefStmt (entity, def) |> G.s

and map_unary_expression (env : env) (x : CST.unary_expression) : G.expr =
  match x with
  | `Choice_un_exp_ x -> (
      match x with
      | `Un_exp_ x -> map_unary_expression_ env x
      | `Borrow_exp x ->
          let expr, _attr = map_borrow_expression env x in
          (*todo deal with attr*)
          expr
      | `Dere_exp x -> map_dereference_expression env x
      | `Move_or_copy_exp x -> map_move_or_copy_expression env x
      | `Exp_term x -> map_expression_term env x
      | `Choice_choice_access_field x -> map_dot_or_index_chain env x)
  | `Ellips tok -> G.Ellipsis (token env tok) |> G.e
  | `Deep_ellips x -> map_deep_ellipsis env x
  | `Field_access_ellips_expr x -> map_field_access_ellipsis_expr env x

and map_unary_expression_ (env : env) ((v1, v2) : CST.unary_expression_) :
    G.expr =
  let bang = map_unary_op env v1 in
  let expr = map_expression env v2 in
  G.opcall (G.Not, bang) [ expr ]

and map_vector_expression (env : env) ((v1, v2, v3, v4) : CST.vector_expression)
    =
  let _types =
    (*todo do something with these types*)
    match v1 with
    | `Vect tok -> (* "vector[" *) []
    | `Vect_type_rep_COMMA_type_opt_COMMA_GT_LBRACK (v1, v2, v3, v4, v5, v6) ->
        let v1 = (* "vector<" *) token env v1 in
        let type_ = map_type_ env v2 in
        let types =
          List_.map
            (fun (v1, v2) ->
              let v1 = token env v1 in
              let v2 = map_type_ env v2 in
              v2)
            v3
        in
        let all_types = type_ :: types in
        let v4 =
          match v4 with
          | Some tok -> (* "," *) Some (token env tok)
          | None -> None
        in
        let v5 = (* ">" *) token env v5 in
        let v6 = (* "[" *) token env v6 in
        all_types
  in
  let expr_list =
    List_.map
      (fun (v1, v2) ->
        let v1 = map_expression env v1 in
        let v2 = token env v2 in
        v1)
      v2
  in
  let values =
    match v3 with
    | Some x -> expr_list @ [ map_expression env x ]
    | None -> expr_list
  in
  let v4 = (* "]" *) token env v4 in
  G.Container (G.Array, (G.fake "[", values, G.fake "]")) |> G.e

and map_move_or_copy_expression (env : env)
    ((v1, v2) : CST.move_or_copy_expression) : G.expr =
  let v1 =
    match v1 with
    | `Move tok -> (* "move" *) str env tok
    | `Copy tok -> (* "copy" *) str env tok
  in
  let expr = map_expression env v2 in
  G.Call (G.N (H2.name_of_id v1) |> G.e, fb [ G.Arg expr ]) |> G.e

and map_constant (env : env) ((v1, v2, v3, v4, v5, v6, v7) : CST.constant) =
  let tconst = (* "const" *) token env v1 in
  let ident = (* identifier *) str env v2 in
  let v3 = (* ":" *) token env v3 in
  let const_type = map_type_ env v4 in
  let v5 = (* "=" *) token env v5 in
  let value = map_expression env v6 in
  let sc = (* ";" *) token env v7 in

  let var_def =
    { G.vinit = Some value; G.vtype = Some const_type; vtok = Some sc }
  in
  let attrs = [ G.attr G.Const tconst ] in
  let ent = G.basic_entity ~attrs ident in
  G.DefStmt (ent, G.VarDef var_def) |> G.s

and map_function_item (env : env) (x : CST.function_item) : G.stmt =
  match x with
  | `Native_func_defi (v1, v2) ->
      let fn_def, fn_ent = map_function_signature env [] None v1 in
      let v2 = (* ";" *) token env v2 in
      G.DefStmt (fn_ent, G.FuncDef fn_def) |> G.s
  | `Macro_func_defi (v1, v2, v3, v4) ->
      let modfier_attrs =
        match v1 with
        | Some x -> [ map_modifier env x ]
        | None -> []
      in
      let macro_ = (* "macro" *) token env v2 in
      let attrs, ident, type_parameters, function_parameters, ret_type =
        map_macro_signature env v3
      in
      (*todo what to do with func params*)
      let marco_smts = map_block_ env v4 |> List_.map (fun x -> G.S x) in
      let ident : G.ident = ident in
      let entity =
        G.basic_entity ~attrs:(attrs @ modfier_attrs) ~tparams:type_parameters
          ident
      in
      let macro_def = { G.macroparams = []; G.macrobody = marco_smts } in
      G.DefStmt (entity, G.MacroDef macro_def) |> G.s
  | `Func_defi (v1, v2) ->
      let body = map_block env v2 in
      let fn_def, fn_ent = map_function_signature env [] (Some body) v1 in
      G.DefStmt (fn_ent, G.FuncDef fn_def) |> G.s

and map_let_expr (env : env) ((v1, v2, v3, v4) : CST.let_statement) : G.expr =
  let let_ = (* "let" *) token env v1 in
  let bind = map_bind_list env v2 in
  let type_hint =
    v3
    |> Option.map (fun (v1, v2) ->
           let v1 = (* ":" *) token env v1 in
           let v2 = map_type_ env v2 in
           v2)
  in
  let value =
    v4
    |> Option.map (fun (v1, v2) ->
           let v1 = (* "=" *) token env v1 in
           let v2 = map_expression env v2 in
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

let map_semgrep_expression (env : env) (x : CST.semgrep_expression) =
  G.E
    (match x with
    | `Exp x -> map_expression env x
    | `Let_stmt (v1, v2, v3, v4) -> map_let_expr env (v1, v2, v3, v4))

let map_enum_item (env : env) (x : CST.enum_item) =
  match x with
  | `Enum_defi (v1, v2, v3, v4) ->
      let attrs =
        match v1 with
        | Some tok -> [ G.KeywordAttr (G.Public, token env tok) ]
        | None -> []
      in
      let name, type_params, abilites = map_enum_signature env v2 in
      let all_variants = map_enum_variants env v3 in
      let _all_abilities =
        (*todo deal with abilities*)
        match v4 with
        | Some x -> abilites @ map_postfix_ability_decls env x
        | None -> abilites
      in
      let type_def = { G.tbody = all_variants } in
      let ent =
        {
          G.name = G.EN (G.Id (name, G.empty_id_info ()));
          G.attrs;
          G.tparams = type_params;
        }
      in
      G.DefStmt (ent, G.TypeDef type_def) |> G.s

let map_module_body (env : env) (x : CST.module_body) : G.stmt list =
  match x with
  | `Choice_SEMI_rep_choice_use_decl_opt_RCURL (v1, v2, v3) ->
      let v1 =
        match v1 with
        | `SEMI tok -> (* ";" *) token env tok
        | `LCURL tok -> (* "{" *) token env tok
      in
      let stmt_list_list =
        List_.map
          (fun x ->
            match x with
            | `Use_decl x ->
                map_use_declaration env x
                |> List_.map (fun x -> G.DirectiveStmt x |> G.s)
            | `Friend_decl x ->
                [
                  map_friend_declaration env x
                  |> (fun x -> G.DirectiveStmt x)
                  |> G.s;
                ]
            | `Cst x -> [ map_constant env x ]
            | `Func_item x -> [ map_function_item env x ]
            | `Struct_item x -> [ map_struct_item env x ]
            | `Enum_item x -> [ map_enum_item env x ]
            | `Spec_blk x -> [ map_spec_block env x ])
          v2
      in
      let v3 =
        match v3 with
        | Some tok -> (* "}" *) Some (token env tok)
        | None -> None
      in
      let stmt_list = stmt_list_list |> List_.flatten in
      stmt_list
  | `Ellips tok -> [ G.Ellipsis (token env tok) |> G.e |> G.exprstmt ]

let map_module_definition (env : env) ((v1, v2, v3) : CST.module_definition) =
  let v1 = (* "module" *) token env v1 in
  let address, mod_name = map_module_identity env v2 in
  let mod_body = map_module_body env v3 in
  let name = G.EN (H2.name_of_ids [ address; mod_name ]) in
  let ent = { name; attrs = []; tparams = None } in
  let def = G.ModuleDef { mbody = G.ModuleStruct (None, mod_body) } in
  G.DefStmt (ent, def) |> G.s

let map_semgrep_statement (env : env) (xs : CST.semgrep_statement) =
  let stmt_list_list =
    xs
    |> List_.map (fun x ->
           match x with
           | `Blk_item x -> [ map_block_item env x ]
           | `Use_decl x ->
               map_use_declaration env x
               |> List_.map (fun x -> G.DirectiveStmt x |> G.s)
           | `Friend_decl x ->
               [
                 map_friend_declaration env x
                 |> (fun x -> G.DirectiveStmt x)
                 |> G.s;
               ]
           | `Cst x -> [ map_constant env x ]
           | `Func_item x -> [ map_function_item env x ]
           | `Struct_item x -> [ map_struct_item env x ]
           | `Enum_item x -> [ map_enum_item env x ]
           | `Spec_blk x -> [ map_spec_block env x ]
           | `Module_body x -> map_module_body env x)
  in
  let stmt_list = stmt_list_list |> List_.flatten in
  G.Ss stmt_list

let map_source_file (env : env) (x : CST.source_file) =
  match x with
  | `Rep_module_defi xs -> G.Pr (xs |> List_.map (map_module_definition env))
  | `Semg_exp x -> map_semgrep_expression env x
  | `Semg_stmt x -> map_semgrep_statement env x
  | `Semg_part x -> (
      match x with
      | `Func_sign x ->
          let fn_def, ent = map_function_signature env [] None x in
          G.Partial (G.PartialDef (ent, G.FuncDef fn_def))
      | `Struct_sign x ->
          let struct_, abilities, ent = map_struct_signature env [] x in
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
          let name, type_params, abilites = map_enum_signature env x in
          let ent =
            {
              G.name = G.EN (G.Id (name, G.empty_id_info ()));
              G.attrs = [];
              G.tparams = type_params;
            }
          in
          let enum_def = { G.tbody = G.OrType [ G.OrEnum (name, None) ] } in
          G.Partial (G.PartialDef (ent, G.TypeDef enum_def)))

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let parse file =
  H.wrap_parser
    (fun () -> Tree_sitter_move_on_sui.Parse.file !!file)
    (fun cst _extras ->
      let env = { H.file; conv = H.line_col_to_pos file; extra = Target } in
      match map_source_file env cst with
      | G.Pr xs -> xs
      | _ -> failwith "not a program")

let parse_expression_or_source_file str =
  Tree_sitter_move_on_sui.Parse.string str

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
