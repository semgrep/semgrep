(**
   Derive a JS/TS AST from a tree-sitter TS (or TSX) CST.

   This is derived from generated code 'semgrep-typescript/lib/Boilerplate.ml'
   in tree-sitter-lang.
*)

open Common
module AST = Ast_js
module H = Parse_tree_sitter_helpers
module G = AST_generic_
module PI = Parse_info
module H2 = AST_generic_helpers
open Ast_js

(*
   Development notes

   - Try to change the structure of this file as little as possible,
     since it's derived from generated code and we'll have to merge
     updates as the grammar changes.
   - Typescript is a superset of Javascript.
   - We started by ignoring typescript-specific constructs and mapping
     the rest to a Javascript AST.
*)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

type env = unit H.env

let token = H.token
let str = H.str
let fake = PI.unsafe_fake_info ""
let fb = PI.unsafe_fake_bracket
let mk_functype (params, rett) = TyFun (params, rett)

(* Note that this file also raises some Impossible and Ast_builder_error *)
let _todo _env _x = failwith "internal error: not implemented"

(*
   We preserve the distinction between a plain identifier and a more complex
   pattern because function parameters make this distinction.
   This conversion is intended to create a sub-pattern.
*)
let sub_pattern (id_or_pat : (a_ident, a_pattern) either) : a_pattern =
  match id_or_pat with
  | Left id -> Id id
  | Right pat -> pat

let optional env opt f =
  match opt with
  | None -> None
  | Some x -> Some (f env x)

(* tree-sitter-typescript is now very laxist in what it accepts after
 * an 'extends' for classes and allow now any expression, even though
 * most expressions have the form foo.t in which case it's really a TyName.
 * For interfaces, typescript uses 'extends_type_clause' which
 * is simpler and restrict the 'extends' to be a type, but for regular
 * classes it uses this 'extends_clause' which is very laxist.
 * The function below tries to reverse-engineer a type from an expr
 * to match what we do in parser_js.mly.
 *)
let tyname_or_expr_of_expr e _targsTODO =
  let rec ids_of_expr = function
    | Id id -> [ id ]
    | ObjAccess (e, _, PN id) -> id :: ids_of_expr e
    | _ -> raise Not_found
  in
  try
    let ids = ids_of_expr e |> List.rev in
    Right (TyName ids)
  with
  | Not_found -> Left e

(*
   Map the comma-separated representation of a list to an ocaml list.
   The separator doesn't have to be a comma but must be a simple token.

   This is used usually where the commaSep function was used as a macro
   in the original grammar.js.

   Usage:

     map_sep_list env v1 v2 (fun env x ->
       ...
     )
*)
let map_sep_list (env : env) (head : 'a) (tail : (_ * 'a) list)
    (f : env -> 'a -> 'b) : 'b list =
  let head = f env head in
  let tail =
    Common.map (fun ((_sep : Tree_sitter_run.Token.t), elt) -> f env elt) tail
  in
  head :: tail

(*****************************************************************************)
(* Boilerplate converter *)
(*****************************************************************************)

module CST = CST_tree_sitter_typescript (* typescript+tsx, merged *)

let identifier (env : env) (tok : CST.identifier) : a_ident = str env tok

let identifier_ (env : env) (x : CST.identifier_) : expr =
  match x with
  | `Unde tok -> IdSpecial (Undefined, token env tok)
  | `Id tok -> identifier env tok |> idexp_or_special

(* LATER: this is overriden by another automatic_semicolon later, normal? *)
let automatic_semicolon (_env : env) (_tok : CST.automatic_semicolon) =
  (* do like in pfff: *)
  PI.unsafe_fake_info ";"

let semicolon (env : env) (x : CST.semicolon) =
  match x with
  | `Auto_semi tok -> automatic_semicolon env tok (* automatic_semicolon *)
  | `SEMI tok -> (* ";" *) token env tok

let this env tok = IdSpecial (This, token env tok)
let super env tok = IdSpecial (Super, token env tok)

let number (env : env) (tok : CST.number) =
  let s, t = str env tok (* number *) in
  (* TODO? float_of_string_opt_also_from_hexoctbin *)
  (Common2.float_of_string_opt s, t)

let empty_stmt env tok =
  let t = token env tok in
  Block (t, [], t)

let number_as_string (env : env) (tok : CST.number) =
  let _, s = tok in
  let opt_num, t = number env tok in
  let num_str =
    match opt_num with
    | None -> s
    | Some n -> string_of_float n
  in
  (num_str, t)

let string_ (env : env) (x : CST.string_) : string wrap =
  match x with
  | `DQUOT_rep_choice_unes_double_str_frag_DQUOT (v1, v2, v3) ->
      let open_ = token env v1 (* "\"" *) in
      let contents =
        Common.map
          (fun x ->
            match x with
            | `Unes_double_str_frag tok ->
                str env tok (* pattern "[^\"\\\\]+" *)
            | `Esc_seq tok -> (* escape_sequence *) str env tok)
          v2
      in
      let close = token env v3 (* "\"" *) in
      let str = contents |> Common.map fst |> String.concat "" in
      let toks = (contents |> Common.map snd) @ [ close ] in
      (str, PI.combine_infos open_ toks)
  | `SQUOT_rep_choice_unes_single_str_frag_SQUOT (v1, v2, v3) ->
      let open_ = token env v1 (* "'" *) in
      let v2 =
        Common.map
          (fun x ->
            match x with
            | `Unes_single_str_frag tok -> str env tok (* pattern "[^'\\\\]+" *)
            | `Esc_seq tok -> str env tok
            (* escape_sequence *))
          v2
      in
      let close = token env v3 (* "'" *) in
      let str = v2 |> Common.map fst |> String.concat "" in
      let toks = (v2 |> Common.map snd) @ [ close ] in
      (str, PI.combine_infos open_ toks)

let namespace_import (env : env) ((v1, v2, v3) : CST.namespace_import) =
  let star = token env v1 (* "*" *) in
  let _as = token env v2 (* "as" *) in
  let id = identifier env v3 (* identifier *) in
  (star, id)

let jsx_identifier_ (env : env) (x : CST.jsx_identifier_) =
  match x with
  | `Jsx_id tok ->
      str env tok (* pattern [a-zA-Z_$][a-zA-Z\d_$]*-[a-zA-Z\d_$\-]* *)
  | `Id tok -> identifier env tok

let jsx_namespace_name (env : env) ((v1, v2, v3) : CST.jsx_namespace_name) =
  let v1 = jsx_identifier_ env v1 in
  let _v2 = token env v2 (* ":" *) in
  let v3 = jsx_identifier_ env v3 in
  (v1, v3)

let jsx_attribute_name (env : env) (x : CST.jsx_attribute_name) =
  match x with
  | `Choice_jsx_id x -> jsx_identifier_ env x
  | `Jsx_name_name x ->
      let id1, id2 = jsx_namespace_name env x in
      let str = fst id1 ^ ":" ^ fst id2 in
      (str, PI.combine_infos (snd id1) [ snd id2 ])

let rec id_or_nested_id (env : env) (x : CST.anon_choice_type_id_42c0412) :
    a_ident list =
  match x with
  | `Id tok -> [ identifier env tok ] (* identifier *)
  | `Nested_id x -> nested_identifier env x

and nested_identifier (env : env) ((v1, v2, v3) : CST.nested_identifier) =
  let v1 = id_or_nested_id env v1 in
  let _v2 = token env v2 (* "." *) in
  let v3 = identifier env v3 (* identifier *) in
  v1 @ [ v3 ]

let jsx_element_name (env : env) (x : CST.jsx_element_name) : a_ident =
  match x with
  | `Choice_jsx_id x -> jsx_identifier_ env x
  | `Nested_id x ->
      let xs = nested_identifier env x in
      let str = xs |> Common.map fst |> String.concat "." in
      let hd, tl =
        match xs with
        | [] -> raise Impossible
        | x :: xs -> (x, xs)
      in
      (str, PI.combine_infos (snd hd) (tl |> Common.map snd))
  | `Jsx_name_name x ->
      let id1, id2 = jsx_namespace_name env x in
      let str = fst id1 ^ ":" ^ fst id2 in
      (str, PI.combine_infos (snd id1) [ snd id2 ])

let jsx_closing_element (env : env) ((v1, v2, v3, v4) : CST.jsx_closing_element)
    =
  let v1 = token env v1 (* "<" *) in
  let v2 = token env v2 (* "/" *) in
  let str, v3 = jsx_element_name env v3 in
  let v4 = token env v4 (* ">" *) in
  let t = PI.combine_infos v1 [ v2; v3; v4 ] in
  (str, t)

let from_clause (env : env) ((v1, v2) : CST.from_clause) : tok * string wrap =
  let v1 = token env v1 (* "from" *) in
  let v2 = string_ env v2 in
  (v1, v2)

let accessibility_modifier (env : env) (x : CST.accessibility_modifier) =
  match x with
  | `Public tok -> (Public, token env tok) (* "public" *)
  | `Priv tok -> (Private, token env tok) (* "private" *)
  | `Prot tok -> (* "protected" *) (Protected, token env tok)

let accessibility_modifier_opt_to_list env v =
  match v with
  | Some x -> [ accessibility_modifier env x ]
  | None -> []

let kwd_attr_opt_to_list env kwd v =
  match v with
  | Some tok -> [ (kwd, token env tok) ]
  | None -> []

let predefined_type (env : env) (x : CST.predefined_type) : a_ident =
  match x with
  | `Any tok
  | `Num tok
  | `Bool tok
  | `Str tok
  | `Symb tok
  | `Void tok
  | `Obj tok
  | `Never tok
  | `Unkn tok ->
      identifier env tok

let anon_choice_PLUSPLUS_e498e28 (env : env)
    (x : CST.anon_choice_PLUSPLUS_e498e28) =
  match x with
  | `PLUSPLUS tok -> (G.Incr, token env tok) (* "++" *)
  | `DASHDASH tok -> (* "--" *) (G.Decr, token env tok)

let map_anon_choice_DOT_d88d0af (env : env) (x : CST.anon_choice_DOT_d88d0af) =
  match x with
  | `DOT tok -> (* "." *) token env tok
  (* TODO: return something different *)
  | `QMARKDOT tok -> (* "?." *) token env tok

let map_anon_choice_priv_prop_id_89abb74 (env : env)
    (x : CST.anon_choice_priv_prop_id_89abb74) : a_ident =
  match x with
  | `Priv_prop_id tok -> (* private_property_identifier *) str env tok
  | `Id tok -> (* identifier *) str env tok

let type_or_typeof (env : env) (x : CST.anon_choice_type_2b11f6b) =
  match x with
  | `Type tok -> token env tok (* "type" *)
  | `Typeof tok -> (* "typeof" *) token env tok

let automatic_semicolon (env : env) (tok : CST.automatic_semicolon) =
  token env tok

let automatic_semicolon_opt env v =
  match v with
  | Some tok -> Some (automatic_semicolon env tok)
  | None -> None

let anon_choice_get_8fb02de (env : env) (x : CST.anon_choice_get_8fb02de) =
  match x with
  | `Get tok -> (Get, token env tok) (* "get" *)
  | `Set tok -> (Set, token env tok) (* "set" *)
  | `STAR tok -> (* "*" *) (Generator, token env tok)

let reserved_identifier (env : env) (x : CST.reserved_identifier) =
  match x with
  | `Decl tok
  | `Name tok
  | `Type tok
  | `Public tok
  | `Priv tok
  | `Prot tok
  | `Read tok
  | `Module tok
  | `Any tok
  | `Num tok
  | `Bool tok
  | `Str tok
  | `Symb tok
  | `Export tok
  | `Over tok ->
      identifier env tok
  | `Choice_get x -> (
      match x with
      | `Get tok -> identifier env tok (* "get" *)
      | `Set tok -> identifier env tok (* "set" *)
      | `Async tok -> identifier env tok (* "async" *)
      | `Static tok -> identifier env tok (* "static" *)
      | `Export tok -> identifier env tok (* export *))

let anon_choice_COMMA_5194cb4 (env : env) (x : CST.anon_choice_COMMA_5194cb4) =
  match x with
  | `COMMA tok -> token env tok (* "," *)
  | `Choice_auto_semi x -> semicolon env x

let import_export_specifier (env : env)
    ((v1, v2, v3) : CST.import_export_specifier) :
    (a_ident * a_ident option) option =
  let type_or_typeof =
    match v1 with
    | Some x -> Some (type_or_typeof env x)
    | None -> None
  in
  let opt_as_id =
    match v3 with
    | None -> None
    | Some (_as_tok, id_tok) -> Some (identifier env id_tok)
  in
  match type_or_typeof with
  | Some _ -> (* TODO: 'type foo', 'typeof foo' *) None
  | None ->
      let expr_id = identifier env v2 in
      Some (expr_id, opt_as_id)

let concat_nested_identifier (idents : a_ident list) : a_ident =
  let str = idents |> Common.map fst |> String.concat "." in
  let tokens = Common.map snd idents in
  let x, xs =
    match tokens with
    | [] -> assert false
    | x :: xs -> (x, xs)
  in
  (str, PI.combine_infos x xs)

(* 'import id = require(...)' are Commonjs-style import.
 * See https://www.typescriptlang.org/docs/handbook/2/modules.html#commonjs-style-import-and-export- for reference.
 * We translate them in regular typescript import.
 *  example:
 *      import zip = require("./ZipCodeValidator");
 *   => import * as zip from "./ZipCodeValidator"
 *
 *)
let import_require_clause tk (env : env)
    ((v1, v2, v3, v4, v5, v6) : CST.import_require_clause) =
  let v1 = identifier env v1 (* identifier *) in
  let _v2 = token env v2 (* "=" *) in
  let _v3 = identifier env v3 (* "require" *) in
  let _v4 = token env v4 (* "(" *) in
  let v5 = string_ env v5 in
  let _v6 = token env v6 (* ")" *) in
  ModuleAlias (tk, v1, v5)

let literal_type (env : env) (x : CST.literal_type) : expr =
  match x with
  | `Num_ (v1, v2) ->
      let s, t1 =
        match v1 with
        | `DASH tok -> str env tok (* "-" *)
        | `PLUS tok -> str env tok (* "+" *)
      in
      let s2, t2 = str env v2 (* number *) in
      (* TODO: float_of_string_opt_also_from_hexoctbin *)
      L (Num (float_of_string_opt (s ^ s2), PI.combine_infos t1 [ t2 ]))
  | `Num tok ->
      let n = number env tok in
      L (Num n)
  | `Str x -> L (String (string_ env x))
  | `True tok -> L (Bool (true, token env tok (* "true" *)))
  | `False tok -> L (Bool (false, token env tok (* "false" *)))
  | `Null tok -> IdSpecial (Null, token env tok)
  | `Unde tok -> IdSpecial (Undefined, token env tok)

let nested_type_identifier (env : env)
    ((v1, v2, v3) : CST.nested_type_identifier) : a_ident list =
  let v1 = id_or_nested_id env v1 in
  let _v2 = token env v2 (* "." *) in
  let v3 = str env v3 (* identifier *) in
  v1 @ [ v3 ]

let id_or_reserved_id (env : env)
    (x :
      [ `Id of Tree_sitter_run.Token.t
      | `Choice_decl of CST.reserved_identifier ]) : a_ident =
  match x with
  | `Id tok -> identifier env tok (* identifier *)
  | `Choice_decl x -> reserved_identifier env x

let import_export_specifiers (env : env)
    ((v1, v2) :
      CST.anon_import_export_spec_rep_COMMA_import_export_spec_3a1421d) :
    (a_ident * a_ident option) list =
  map_sep_list env v1 v2 import_export_specifier
  |> List.filter_map (fun opt -> opt)

let export_clause (env : env) ((v1, v2, v3, v4) : CST.export_clause) =
  let _open = token env v1 (* "{" *) in
  let xs =
    match v2 with
    | Some x -> import_export_specifiers env x
    | None -> []
  in
  let _trailing_comma =
    match v3 with
    | Some _tok -> (* "," *) ()
    | None -> ()
  in
  let _close = token env v4 (* "}" *) in
  xs

let named_imports (env : env) ((v1, v2, v3, v4) : CST.named_imports) =
  let _open = token env v1 (* "{" *) in
  let imports =
    match v2 with
    | Some x -> import_export_specifiers env x
    | None -> []
  in
  let _trailing_comma =
    match v3 with
    | Some tok -> Some (token env tok) (* "," *)
    | None -> None
  in
  let _close = token env v4 (* "}" *) in
  fun (import_tok : tok) (from_path : a_filename) ->
    imports
    |> Common.map (fun (name, opt_as_name) ->
           Import (import_tok, (name, opt_as_name), from_path))

let import_clause (env : env) (x : CST.import_clause) =
  match x with
  | `Name_import x ->
      let _star, id = namespace_import env x in
      fun tok path -> [ ModuleAlias (tok, id, path) ]
  | `Named_imports x -> named_imports env x
  | `Id_opt_COMMA_choice_name_import (v1, v2) ->
      let v1 = identifier env v1 (* identifier *) in
      let v2 =
        match v2 with
        | Some (v1, v2) ->
            let _v1 = token env v1 (* "," *) in
            let v2 =
              match v2 with
              | `Name_import x ->
                  let _star, id = namespace_import env x in
                  fun tok path -> [ ModuleAlias (tok, id, path) ]
              | `Named_imports x -> named_imports env x
            in
            v2
        | None -> fun _t _path -> []
      in
      fun t path ->
        let default = Import (t, ((default_entity, snd v1), Some v1), path) in
        default :: v2 t path

let rec decorator_member_expression (env : env)
    ((v1, v2, v3) : CST.decorator_member_expression) : a_ident list =
  let v1 = anon_choice_type_id_b8f8ced env v1 in
  let _v2 = token env v2 (* "." *) in
  let v3 = identifier env v3 (* identifier *) in
  v1 @ [ v3 ]

and anon_choice_type_id_b8f8ced (env : env)
    (x : CST.anon_choice_type_id_b8f8ced) : a_ident list =
  match x with
  | `Id x -> [ identifier env x ]
  | `Deco_member_exp x -> decorator_member_expression env x

let rec parenthesized_expression (env : env)
    ((v1, v2, v3) : CST.parenthesized_expression) =
  let _v1 = token env v1 (* "(" *) in
  let v2 =
    match v2 with
    | `Exp_opt_type_anno (v1, v2) -> (
        let v1 = expression env v1 in
        match v2 with
        | Some x ->
            let tok, ty = type_annotation env x in
            Cast (v1, tok, ty)
        | None -> v1)
    | `Seq_exp x -> sequence_expression env x
  in
  let _v3 = token env v3 (* ")" *) in
  v2

and jsx_opening_element (env : env) ((v1, v2, v3, v4) : CST.jsx_opening_element)
    =
  let v1 = token env v1 (* "<" *) in
  let v2 =
    match v2 with
    | `Choice_choice_jsx_id x -> jsx_attribute_name env x
    | `Choice_id_opt_type_args (v1, v2) ->
        let ids = id_or_nested_id env v1 in
        let id = concat_nested_identifier ids in
        (* TODO:
           let v2 = type_arguments env v2 |> PI.unbracket
             |> Common.map (fun x -> G.TypeArg x) in
            H2.name_of_ids ~name_typeargs:(Some v2) v1
        *)
        let _v2TODO =
          match v2 with
          | Some x -> type_arguments env x |> PI.unbracket
          | None -> []
        in
        id
  in
  let v3 = Common.map (jsx_attribute_ env) v3 in
  let v4 = token env v4 (* ">" *) in
  (v1, v2, v3, v4)

and jsx_self_clos_elem (env : env)
    ((v1, v2, v3, v4, v5) : CST.jsx_self_closing_element) =
  let v1 = token env v1 (* "<" *) in
  let v2 =
    match v2 with
    | `Choice_choice_jsx_id x -> jsx_attribute_name env x
    | `Choice_id_opt_type_args (v1, v2) ->
        let v1 = id_or_nested_id env v1 in
        let id = concat_nested_identifier v1 in
        let _v2TODO =
          match v2 with
          | Some x -> type_arguments env x |> PI.unbracket
          | None -> []
        in
        id
  in
  let v3 = Common.map (jsx_attribute_ env) v3 in
  let v4 = token env v4 (* "/" *) in
  let v5 = token env v5 (* ">" *) in
  let t2 = PI.combine_infos v4 [ v5 ] in
  (v1, v2, v3, t2)

and jsx_fragment (env : env) ((v1, v2, v3, v4, v5, v6) : CST.jsx_fragment) : xml
    =
  let v1 = token env v1 (* "<" *) in
  let v2 = token env v2 (* ">" *) in
  let v3 = Common.map (jsx_child env) v3 in
  let v4 = token env v4 (* "<" *) in
  let v5 = token env v5 (* "/" *) in
  let v6 = token env v6 (* ">" *) in
  let t1 = PI.combine_infos v1 [ v2 ] in
  let t2 = PI.combine_infos v4 [ v5; v6 ] in
  { xml_kind = XmlFragment (t1, t2); xml_attrs = []; xml_body = v3 }

and jsx_expression (env : env) ((v1, v2, v3) : CST.jsx_expression) :
    expr option bracket =
  let v1 = token env v1 (* "{" *) in
  let v2 =
    match v2 with
    | Some x ->
        Some
          (match x with
          | `Exp x -> expression env x
          | `Seq_exp x -> sequence_expression env x
          | `Spread_elem x ->
              let t, e = spread_element env x in
              Apply (IdSpecial (Spread, t), fb [ e ]))
    (* abusing { } in XML to just add comments, e.g. { /* lint-ignore */ } *)
    | None -> None
  in
  let v3 = token env v3 (* "}" *) in
  (v1, v2, v3)

and jsx_attribute_ (env : env) (x : CST.jsx_attribute_) : xml_attribute =
  match x with
  | `Jsx_attr (v1, v2) ->
      let v1 = jsx_attribute_name env v1 in
      let teq, v2 =
        match v2 with
        | Some (v1, v2) ->
            let v1bis = token env v1 (* "=" *) in
            let v2 = jsx_attribute_value env v2 in
            (v1bis, v2)
        (* see https://www.reactenlightenment.com/react-jsx/5.7.html *)
        | None -> (snd v1, L (Bool (true, snd v1)))
      in
      XmlAttr (v1, teq, v2)
  (* less: we could enforce that it's only a Spread operation *)
  | `Jsx_exp x ->
      let x = jsx_expression_some env x in
      XmlAttrExpr x

and jsx_expression_some env x =
  let t1, eopt, t2 = jsx_expression env x in
  match eopt with
  | None ->
      raise (PI.Ast_builder_error ("jsx_expression_some got a None expr", t1))
  | Some e -> (t1, e, t2)

and jsx_attribute_value (env : env) (x : CST.jsx_attribute_value) =
  match x with
  | `Str x ->
      let s = string_ env x in
      L (String s)
  | `Jsx_exp x ->
      let _, e, _ = jsx_expression_some env x in
      e
  (* an attribute value can be a jsx element? *)
  | `Choice_jsx_elem x ->
      let xml = jsx_element_ env x in
      Xml xml
  | `Jsx_frag x ->
      let xml = jsx_fragment env x in
      Xml xml

and jsx_child (env : env) (x : CST.jsx_child) : xml_body =
  match x with
  | `Jsx_text tok ->
      let s = str env tok (* pattern [^{}<>]+ *) in
      XmlText s
  | `Choice_jsx_elem x ->
      let xml = jsx_element_ env x in
      XmlXml xml
  | `Jsx_exp x ->
      let x = jsx_expression env x in
      XmlExpr x
  | `Jsx_frag x ->
      let xml = jsx_fragment env x in
      XmlXml xml

and jsx_element_ (env : env) (x : CST.jsx_element_) : xml =
  match x with
  | `Jsx_elem (v1, v2, v3) ->
      let t0, tag, attrs, closing = jsx_opening_element env v1 in
      let v2 = Common.map (jsx_child env) v2 in
      let v3 = jsx_closing_element env v3 in
      {
        xml_kind = XmlClassic (t0, tag, closing, snd v3);
        xml_attrs = attrs;
        xml_body = v2;
      }
  | `Jsx_self_clos_elem x ->
      let t0, tag, attrs, closing = jsx_self_clos_elem env x in
      {
        xml_kind = XmlSingleton (t0, tag, closing);
        xml_attrs = attrs;
        xml_body = [];
      }

and pattern (env : env) (x : CST.pattern) : (a_ident, a_pattern) either =
  match x with
  | `Id tok -> Left (identifier env tok)
  | `Choice_decl x -> Left (reserved_identifier env x)
  | `Dest_pat x -> Right (destructuring_pattern env x)
  | `Rest_pat (v1, v2) ->
      let tok = token env v1 (* "..." *) in
      let _id_or_pat_TODO = id_or_destructuring_pattern env v2 in
      Right (IdSpecial (Spread, tok))

(*
   This is a pattern for destructuring an object property.
   It could use its own type rather than abusing the 'property' type.
   See notes in ast_js.ml in pfff.
*)
and object_property_pattern (env : env) (x : CST.anon_choice_pair_pat_3ff9cbe) :
    property =
  match x with
  | `Pair_pat (v1, v2, v3) ->
      let v1 = property_name env v1 in
      let _v2 = token env v2 (* ":" *) in
      let body = pattern env v3 |> sub_pattern in
      let ty = None in
      FieldColon
        { fld_name = v1; fld_attrs = []; fld_type = ty; fld_body = Some body }
  | `Rest_pat x ->
      let t, p = rest_pattern env x in
      let pat =
        match p with
        | Left id -> idexp id
        | Right pat -> pat
      in
      FieldSpread (t, pat)
  | `Obj_assign_pat (v1, v2, v3) ->
      let pat =
        match v1 with
        | `Choice_choice_decl x -> id_or_reserved_id env x |> idexp
        | `Dest_pat x -> destructuring_pattern env x
      in
      let tok = token env v2 (* "=" *) in
      (* default value for the property *)
      let e = expression env v3 in
      FieldPatDefault (pat, tok, e)
  | `Choice_id x ->
      let id = id_or_reserved_id env x in
      FieldColon
        {
          fld_name = PN id;
          fld_attrs = [];
          fld_type = None;
          fld_body = Some (idexp id);
        }

and destructuring_pattern (env : env) (x : CST.destructuring_pattern) :
    a_pattern =
  match x with
  | `Obj_pat (v1, v2, v3) ->
      (* similar to 'object_' *)
      let v1 = token env v1 (* "{" *) in
      let v2 =
        match v2 with
        | Some (v1, v2) ->
            map_sep_list env v1 v2 (fun env x ->
                match x with
                | Some x -> [ object_property_pattern env x ]
                | None -> [])
            |> List.flatten
        | None -> []
      in
      let v3 = token env v3 (* "}" *) in
      Obj (v1, v2, v3)
  | `Array_pat (v1, v2, v3) ->
      let open_ = token env v1 (* "[" *) in
      let elements =
        match v2 with
        | Some (v1, v2) ->
            map_sep_list env v1 v2 (fun env x ->
                match x with
                | Some x -> [ pat_or_assign_pat env x |> sub_pattern ]
                | None -> [])
            |> List.flatten
        | None -> []
      in
      let close = token env v3 (* "]" *) in
      Arr (open_, elements, close)

and variable_declaration (env : env)
    ((v1, v2, v3, v4) : CST.variable_declaration) : var list =
  let v1 = (Var, token env v1) (* "var" *) in
  let vars = map_sep_list env v2 v3 variable_declarator in
  let _v4 = semicolon env v4 in
  build_vars v1 vars

and function_ (env : env) ((v1, v2, v3, v4, v5) : CST.function_) :
    function_definition * a_ident option =
  let v1 =
    match v1 with
    | Some tok -> [ attr (Async, token env tok) ] (* "async" *)
    | None -> []
  in
  let v2 = token env v2 (* "function" *) in
  let v3 =
    match v3 with
    | Some tok -> Some (identifier env tok) (* identifier *)
    | None -> None
  in
  let _tparams, (v4, tret) = call_signature env v4 in
  let v5 = statement_block env v5 in
  let f_kind = (G.LambdaKind, v2) in
  ({ f_attrs = v1; f_params = v4; f_body = v5; f_rettype = tret; f_kind }, v3)

and generic_type (env : env) ((v1, _v2) : CST.generic_type) : a_dotted_ident =
  let v1 =
    match v1 with
    | `Id tok -> [ identifier env tok ] (* identifier *)
    | `Nested_type_id x -> nested_identifier env x
  in
  v1

and implements_clause (env : env) ((v1, v2, v3) : CST.implements_clause) :
    type_ list =
  let _v1 = token env v1 (* "implements" *) in
  let types = map_sep_list env v2 v3 type_ in
  types

and anon_choice_exp_9818c1b (env : env) (x : CST.anon_choice_exp_9818c1b) =
  match x with
  | `Exp x -> expression env x
  | `Spread_elem x ->
      let t, e = spread_element env x in
      Apply (IdSpecial (Spread, t), fb [ e ])

and switch_default (env : env) ((v1, v2, v3) : CST.switch_default) =
  let v1 = token env v1 (* "default" *) in
  let v2 = token env v2 (* ":" *) in
  let v3 = List.concat_map (statement env) v3 in
  Default (v1, stmt1 v2 v3)

and binary_expression (env : env) (x : CST.binary_expression) : expr =
  match x with
  | `Exp_AMPAMP_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "&&" *) in
      let v3 = expression env v3 in
      Apply (IdSpecial (ArithOp G.And, v2), fb [ v1; v3 ])
  | `Exp_BARBAR_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "||" *) in
      let v3 = expression env v3 in
      Apply (IdSpecial (ArithOp G.Or, v2), fb [ v1; v3 ])
  | `Exp_GTGT_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* ">>" *) in
      let v3 = expression env v3 in
      Apply (IdSpecial (ArithOp G.LSR, v2), fb [ v1; v3 ])
  | `Exp_GTGTGT_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* ">>>" *) in
      let v3 = expression env v3 in
      Apply (IdSpecial (ArithOp G.ASR, v2), fb [ v1; v3 ])
  | `Exp_LTLT_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "<<" *) in
      let v3 = expression env v3 in
      Apply (IdSpecial (ArithOp G.LSL, v2), fb [ v1; v3 ])
  | `Exp_AMP_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "&" *) in
      let v3 = expression env v3 in
      Apply (IdSpecial (ArithOp G.BitAnd, v2), fb [ v1; v3 ])
  | `Exp_HAT_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "^" *) in
      let v3 = expression env v3 in
      Apply (IdSpecial (ArithOp G.BitXor, v2), fb [ v1; v3 ])
  | `Exp_BAR_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "|" *) in
      let v3 = expression env v3 in
      Apply (IdSpecial (ArithOp G.BitOr, v2), fb [ v1; v3 ])
  | `Exp_PLUS_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "+" *) in
      let v3 = expression env v3 in
      Apply (IdSpecial (ArithOp G.Plus, v2), fb [ v1; v3 ])
  | `Exp_DASH_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "-" *) in
      let v3 = expression env v3 in
      Apply (IdSpecial (ArithOp G.Minus, v2), fb [ v1; v3 ])
  | `Exp_STAR_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "*" *) in
      let v3 = expression env v3 in
      Apply (IdSpecial (ArithOp G.Mult, v2), fb [ v1; v3 ])
  | `Exp_SLASH_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "/" *) in
      let v3 = expression env v3 in
      Apply (IdSpecial (ArithOp G.Div, v2), fb [ v1; v3 ])
  | `Exp_PERC_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "%" *) in
      let v3 = expression env v3 in
      Apply (IdSpecial (ArithOp G.Mod, v2), fb [ v1; v3 ])
  | `Exp_STARSTAR_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "**" *) in
      let v3 = expression env v3 in
      Apply (IdSpecial (ArithOp G.Pow, v2), fb [ v1; v3 ])
  | `Exp_LT_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "<" *) in
      let v3 = expression env v3 in
      Apply (IdSpecial (ArithOp G.Lt, v2), fb [ v1; v3 ])
  | `Exp_LTEQ_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "<=" *) in
      let v3 = expression env v3 in
      Apply (IdSpecial (ArithOp G.LtE, v2), fb [ v1; v3 ])
  | `Exp_EQEQ_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "==" *) in
      let v3 = expression env v3 in
      Apply (IdSpecial (ArithOp G.Eq, v2), fb [ v1; v3 ])
  | `Exp_EQEQEQ_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "===" *) in
      let v3 = expression env v3 in
      Apply (IdSpecial (ArithOp G.PhysEq, v2), fb [ v1; v3 ])
  | `Exp_BANGEQ_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "!=" *) in
      let v3 = expression env v3 in
      Apply (IdSpecial (ArithOp G.NotEq, v2), fb [ v1; v3 ])
  | `Exp_BANGEQEQ_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "!==" *) in
      let v3 = expression env v3 in
      Apply (IdSpecial (ArithOp G.NotPhysEq, v2), fb [ v1; v3 ])
  | `Exp_GTEQ_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* ">=" *) in
      let v3 = expression env v3 in
      Apply (IdSpecial (ArithOp G.GtE, v2), fb [ v1; v3 ])
  | `Exp_GT_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* ">" *) in
      let v3 = expression env v3 in
      Apply (IdSpecial (ArithOp G.Gt, v2), fb [ v1; v3 ])
  | `Exp_QMARKQMARK_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "??" *) in
      let v3 = expression env v3 in
      Apply (IdSpecial (ArithOp G.Nullish, v2), fb [ v1; v3 ])
  | `Exp_inst_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "instanceof" *) in
      let v3 = expression env v3 in
      Apply (IdSpecial (Instanceof, v2), fb [ v1; v3 ])
  | `Exp_in_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 = token env v2 (* "in" *) in
      let v3 = expression env v3 in
      Apply (IdSpecial (In, v2), fb [ v1; v3 ])

and arguments (env : env) ((v1, v2, v3) : CST.arguments) : a_arguments =
  let v1 = token env v1 (* "(" *) in
  let v2 = anon_opt_opt_choice_exp_rep_COMMA_opt_choice_exp_208ebb4 env v2 in
  let v3 = token env v3 (* ")" *) in
  (v1, v2, v3)

and generator_function_declaration (env : env)
    ((v1, v2, v3, v4, v5, v6, v7) : CST.generator_function_declaration) :
    definition =
  let v1 =
    match v1 with
    | Some tok -> [ attr (Async, token env tok) ] (* "async" *)
    | None -> []
  in
  let v2 = token env v2 (* "function" *) in
  let v3 = [ attr (Generator, token env v3) ] (* "*" *) in
  let v4 = identifier env v4 (* identifier *) in
  let _tparams, (v5, tret) = call_signature env v5 in
  let v6 = statement_block env v6 in
  let _v7 = automatic_semicolon_opt env v7 in
  let f_kind = (G.Function, v2) in
  let f =
    { f_attrs = v1 @ v3; f_params = v5; f_body = v6; f_rettype = tret; f_kind }
  in
  (basic_entity v4, FuncDef f)

and variable_declarator (env : env) (x : CST.variable_declarator) =
  match x with
  | `Choice_id_opt_type_anno_opt_init (v1, v2, v3) ->
      let id_or_pat = id_or_destructuring_pattern env v1 in
      let type_ =
        match v2 with
        | Some x -> Some (type_annotation env x |> snd)
        | None -> None
      in
      let default =
        match v3 with
        | Some x -> Some (initializer_ env x)
        | None -> None
      in
      (id_or_pat, type_, default)
  | `Id_BANG_type_anno (v1, v2, v3) ->
      let id_or_pat = Left (identifier env v1 (* identifier *)) in
      (* definite assignment assertion
         TODO: add to AST? *)
      let _v2 = token env v2 (* "!" *) in
      let type_ = type_annotation env v3 |> snd in
      (id_or_pat, Some type_, None)

and sequence_expression (env : env) ((v1, v2, v3) : CST.sequence_expression) =
  let v1 = expression env v1 in
  let v2 = token env v2 (* "," *) in
  let v3 =
    match v3 with
    | `Seq_exp x -> sequence_expression env x
    | `Exp x -> expression env x
  in
  Apply (IdSpecial (Seq, v2), fb [ v1; v3 ])

and type_arguments (env : env) ((v1, v2, v3, v4, v5) : CST.type_arguments) :
    type_ list bracket =
  let v1 = token env v1 (* "<" *) in
  let types = map_sep_list env v2 v3 type_ in
  let _v4 =
    match v4 with
    | Some tok -> Some (token env tok) (* "," *)
    | None -> None
  in
  let v5 = token env v5 (* ">" *) in
  (v1, types, v5)

and add_decorators xs property =
  match property with
  | Field fld -> Field { fld with fld_attrs = xs @ fld.fld_attrs }
  | FieldColon fld -> FieldColon { fld with fld_attrs = xs @ fld.fld_attrs }
  (* less: modify ast_js to allow decorator on those constructs? *)
  | FieldSpread _
  | FieldPatDefault _
  | FieldEllipsis _
  | FieldTodo _ ->
      property

(* TODO: types - class body can be just a signature. *)
and class_body (env : env) ((v1, v2, v3) : CST.class_body) :
    property list bracket =
  let v1 = token env v1 (* "{" *) in
  let rec aux acc_decorators xs =
    match xs with
    | [] -> []
    | x :: xs -> (
        match x with
        | `Deco x ->
            let attr = decorator env x in
            aux (attr :: acc_decorators) xs
        | `Meth_defi_opt_choice_auto_semi (v1, v2) ->
            let v1 = method_definition env v1 in
            let _v2 =
              match v2 with
              | Some x -> Some (semicolon env x)
              | None -> None
            in
            add_decorators (List.rev acc_decorators) v1 :: aux [] xs
        | `Meth_sign_choice_func_sign_auto_semi (v1, v2) ->
            let _v1 = method_signature env v1 in
            let _v2 =
              match v2 with
              | `Func_sign_auto_semi tok ->
                  (* function_signature_automatic_semicolon *) token env tok
              | `COMMA tok -> (* "," *) token env tok
            in
            (* TODO: types *)
            aux [] xs
        | `Choice_abst_meth_sign_choice_choice_auto_semi (v1, v2) -> (
            let v1 =
              match v1 with
              | `Abst_meth_sign x ->
                  (* TODO: types *)
                  let _v = abstract_method_signature env x in
                  None
              | `Index_sign x ->
                  let _t = index_signature env x in
                  None
              | `Meth_sign x ->
                  (* TODO: types *)
                  let _v = method_signature env x in
                  None
              | `Public_field_defi x -> Some (public_field_definition env x)
            in
            let _v2 =
              match v2 with
              | `Choice_auto_semi x -> semicolon env x
              | `COMMA tok -> token env tok
            in
            match v1 with
            | None -> aux [] xs
            | Some x -> add_decorators (List.rev acc_decorators) x :: aux [] xs)
        )
  in
  let v2 = aux [] v2 in
  let v3 = token env v3 (* "}" *) in
  (v1, v2, v3)

and type_parameter (env : env) ((v1, v2, v3) : CST.type_parameter) :
    a_type_parameter =
  let v1 = str env v1 (* identifier *) in
  let _v2 =
    match v2 with
    | Some x -> Some (constraint_ env x)
    | None -> None
  in
  let _v3 =
    match v3 with
    | Some x -> Some (default_type env x)
    | None -> None
  in
  v1

and member_expression (env : env) ((v1, v2, v3) : CST.member_expression) : expr
    =
  let expr = expr_or_prim_expr env v1 in
  (* TODO: distinguish optional chaining "?." from a simple access "." *)
  let dot_tok =
    match v2 with
    | `DOT tok (* "." *)
    | `QMARKDOT (* "?." *) tok ->
        token env tok
  in
  let id_tok =
    match v3 with
    | `Id x -> x
    | `Priv_prop_id x ->
        (* has a leading '#' indicating a private property.
           Should it have a special construct so we could match
           all private properties in semgrep with e.g. #$VAR ? *)
        x
  in
  let id = identifier env id_tok (* identifier *) in
  ObjAccess (expr, dot_tok, PN id)

and object_property (env : env) (x : CST.anon_choice_pair_20c9acd) : property =
  match x with
  | `Pair (v1, v2, v3) ->
      let v1 = property_name env v1 in
      let _v2 = token env v2 (* ":" *) in
      let v3 = expression env v3 in
      FieldColon
        { fld_name = v1; fld_attrs = []; fld_type = None; fld_body = Some v3 }
  | `Spread_elem x ->
      let t, e = spread_element env x in
      FieldSpread (t, e)
  | `Meth_defi x -> method_definition env x
  (* { x } shorthand for { x: x }, like in OCaml *)
  | `Choice_id x ->
      let id = id_or_reserved_id env x in
      FieldColon
        {
          fld_name = PN id;
          fld_attrs = [];
          fld_type = None;
          fld_body = Some (idexp id);
        }

and subscript_expression (env : env)
    ((v1, v2, v3, v4, v5) : CST.subscript_expression) : expr =
  let expr = expr_or_prim_expr env v1 in
  let _v2 =
    match v2 with
    | None -> None
    | Some tok -> (* "?." *) Some (token env tok)
  in
  let v3 = token env v3 (* "[" *) in
  let v4 = expressions env v4 in
  let v5 = token env v5 (* "]" *) in
  (* TODO: distinguish optional chaining "?." from a simple access "." *)
  ArrAccess (expr, (v3, v4, v5))

and initializer_ (env : env) ((v1, v2) : CST.initializer_) =
  let _v1 = token env v1 (* "=" *) in
  let v2 = expression env v2 in
  v2

and primary_expression (env : env) (x : CST.primary_expression) : expr =
  match x with
  | `Choice_subs_exp x -> (
      match x with
      | `Subs_exp x -> subscript_expression env x
      | `Member_exp x -> member_expression env x
      | `Paren_exp x -> parenthesized_expression env x
      | `Choice_unde x -> identifier_ env x
      | `Choice_decl x -> reserved_identifier env x |> idexp
      | `This tok -> this env tok (* "this" *)
      | `Super tok -> super env tok (* "super" *)
      | `Num tok ->
          let n = number env tok (* number *) in
          L (Num n)
      | `Str x ->
          let s = string_ env x in
          L (String s)
      | `Temp_str x ->
          let t1, xs, t2 = template_string env x in
          Apply (IdSpecial (Encaps false, t1), (t1, xs, t2))
      | `Regex (v1, v2, v3, v4) ->
          let v1 = token env v1 (* "/" *) in
          let v2 = str env v2 (* regex_pattern *) in
          let v3 = token env v3 (* "/" *) in
          let v4 =
            match v4 with
            | Some tok -> Some (str env tok) (* pattern [a-z]+ *)
            | None -> None
          in
          L (Regexp ((v1, v2, v3), v4))
      | `True tok -> L (Bool (true, token env tok) (* "true" *))
      | `False tok -> L (Bool (false, token env tok) (* "false" *))
      | `Null tok -> IdSpecial (Null, token env tok) (* "null" *)
      | `Import tok -> identifier env tok (* import *) |> idexp
      | `Obj x ->
          let o = object_ env x in
          Obj o
      | `Array x -> array_ env x
      | `Func x ->
          let f, idopt = function_ env x in
          Fun (f, idopt)
      | `Arrow_func (v1, v2, v3, v4) ->
          let v1 =
            match v1 with
            | Some tok -> [ attr (Async, token env tok) ] (* "async" *)
            | None -> []
          in
          let v2, tret =
            match v2 with
            | `Choice_choice_decl x ->
                let id = id_or_reserved_id env x in
                ([ ParamClassic (mk_param id) ], None)
            | `Call_sign x ->
                let _tparams, (params, tret) = call_signature env x in
                (params, tret)
          in
          let v3 = token env v3 (* "=>" *) in
          let v4 =
            match v4 with
            | `Exp x ->
                let e = expression env x in
                Return (v3, Some e, PI.sc v3)
            | `Stmt_blk x -> statement_block env x
          in
          let f_kind = (G.Arrow, v3) in
          let f =
            {
              f_attrs = v1;
              f_params = v2;
              f_body = v4;
              f_rettype = tret;
              f_kind;
            }
          in
          Fun (f, None)
      | `Gene_func (v1, v2, v3, v4, v5, v6) ->
          let v1 =
            match v1 with
            | Some tok -> [ (Async, token env tok) ] (* "async" *)
            | None -> []
          in
          let v2 = token env v2 (* "function" *) in
          let v3 = [ (Generator, token env v3) ] (* "*" *) in
          let v4 =
            match v4 with
            | Some tok -> Some (identifier env tok) (* identifier *)
            | None -> None
          in
          let _tparams, (v5, tret) = call_signature env v5 in
          let v6 = statement_block env v6 in
          let attrs = v1 @ v3 |> Common.map attr in
          let f_kind = (G.LambdaKind, v2) in
          let f =
            {
              f_attrs = attrs;
              f_params = v5;
              f_body = v6;
              f_rettype = tret;
              f_kind;
            }
          in
          Fun (f, v4)
      | `Class (v1, v2, v3, v4, v5, v6) ->
          let v1 = Common.map (decorator env) v1 in
          let v2 = token env v2 (* "class" *) in
          let v3 =
            match v3 with
            | Some tok -> Some (identifier env tok) (* identifier *)
            | None -> None
          in
          (* TODO types *)
          let _v4 =
            match v4 with
            | Some x -> type_parameters env x
            | None -> []
          in
          let c_extends, c_implements =
            match v5 with
            | Some x -> class_heritage env x
            | None -> ([], [])
          in
          let v6 = class_body env v6 in
          let class_ =
            {
              c_kind = (G.Class, v2);
              c_attrs = v1;
              c_extends;
              c_implements;
              c_body = v6;
            }
          in
          Class (class_, v3)
      | `Meta_prop (v1, v2, v3) ->
          let v1 = token env v1 (* "new" *) in
          let v2 = token env v2 (* "." *) in
          let v3 = token env v3 (* "target" *) in
          let t = PI.combine_infos v1 [ v2; v3 ] in
          IdSpecial (NewTarget, t)
      | `Call_exp x -> call_expression env x)
  | `Non_null_exp x -> non_null_expression env x

and call_expression (env : env) (x : CST.call_expression) =
  match x with
  | `Exp_opt_type_args_choice_args (v1, v2, v3) ->
      let v1 = expression env v1 in
      (* TODO: types *)
      let _v2TODO =
        match v2 with
        | Some x -> type_arguments env x |> PI.unbracket
        | None -> []
      in
      let v3 =
        match v3 with
        | `Args x ->
            let args = arguments env x in
            Apply (v1, args)
        | `Temp_str x ->
            let t1, xs, t2 = template_string env x in
            Apply (IdSpecial (Encaps true, t1), (t1, v1 :: xs, t2))
      in
      v3
  | `Prim_exp_QMARKDOT_opt_type_args_args (v1, v2, v3, v4) ->
      let v1 = primary_expression env v1 in
      let _v2 = token env v2 (* "?." *) in
      (* TODO: types *)
      let _v3TODO =
        match v3 with
        | Some x -> type_arguments env x |> PI.unbracket
        | None -> []
      in
      let v4 = arguments env v4 in
      (* TODO: distinguish "?." from a simple application *)
      Apply (v1, v4)

and anon_choice_prop_name_6cc9e4b (env : env)
    (x : CST.anon_choice_prop_name_6cc9e4b) =
  match x with
  | `Prop_name x -> (property_name env x, None)
  | `Enum_assign (v1, v2) ->
      let v1 = property_name env v1 in
      let v2 = initializer_ env v2 in
      (v1, Some v2)

and module__ (env : env) ((v1, v2) : CST.module__) =
  let v1 =
    (* module identifier *)
    match v1 with
    | `Str x -> string_ env x
    | `Id tok -> identifier env tok (* identifier *)
    | `Nested_id x -> nested_identifier env x |> concat_nested_identifier
  in
  let v2 =
    (* optional module body *)
    match v2 with
    | Some x -> Some (statement_block env x)
    | None -> None
  in
  (v1, v2)

and non_null_expression (env : env) ((v1, v2) : CST.non_null_expression) =
  let v1 = expression env v1 in
  let v2 = token env v2 (* "!" *) in
  let special = (ArithOp G.NotNullPostfix, v2) in
  Apply (IdSpecial special, fb [ v1 ])

and expression_statement (env : env) ((v1, v2) : CST.expression_statement) =
  let v1 = expressions env v1 in
  let v2 = semicolon env v2 in
  (v1, v2)

and catch_clause (env : env) ((v1, v2, v3) : CST.catch_clause) : catch =
  let catch_tok = token env v1 (* "catch" *) in
  let stmts = statement_block env v3 in
  let catch =
    match v2 with
    | Some (v1, v2, v3, v4) ->
        let _open = token env v1 (* "(" *) in
        let id_or_pat = id_or_destructuring_pattern env v2 in
        let type_ =
          match v3 with
          | Some x -> Some (type_annotation env x)
          | None -> None
        in
        let _close = token env v4 (* ")" *) in
        let pat =
          match id_or_pat with
          | Left id -> idexp id
          | Right pat -> pat
        in
        let pat =
          match type_ with
          | None -> pat
          | Some (colon_tok, type_) -> Cast (pat, colon_tok, type_)
        in
        BoundCatch (catch_tok, pat, stmts)
    | None -> UnboundCatch (catch_tok, stmts)
  in
  catch

and object_type (env : env) ((v1, v2, v3) : CST.object_type) =
  let v1 =
    match v1 with
    | `LCURL tok -> token env tok (* "{" *)
    | `LCURLBAR tok -> token env tok
    (* "{|" *)
  in
  let v2 =
    match v2 with
    | Some (v1, v2, v3, v4) ->
        let _v1 =
          match v1 with
          | Some x ->
              Some
                (match x with
                | `COMMA tok -> token env tok (* "," *)
                | `SEMI tok -> token env tok (* ";" *))
          | None -> None
        in
        let v2 = anon_choice_export_stmt_f90d83f env v2 in
        let v3 =
          Common.map
            (fun (v1, v2) ->
              let _v1 = anon_choice_COMMA_5194cb4 env v1 in
              let v2 = anon_choice_export_stmt_f90d83f env v2 in
              v2)
            v3
        in
        let _v4 =
          match v4 with
          | Some x -> Some (anon_choice_COMMA_5194cb4 env x)
          | None -> None
        in
        v2 :: v3
    | None -> []
  in
  let v3 =
    match v3 with
    | `RCURL tok -> token env tok (* "}" *)
    | `BARRCURL tok -> token env tok
    (* "|}" *)
  in
  (v1, v2, v3)

and template_string (env : env) ((v1, v2, v3) : CST.template_string) :
    expr list bracket =
  let v1 = token env v1 (* "`" *) in
  let v2 =
    Common.map
      (fun x ->
        match x with
        | `Temp_chars tok -> L (String (str env tok)) (* template_chars *)
        | `Esc_seq tok -> L (String (str env tok)) (* escape_sequence *)
        | `Temp_subs x -> template_substitution env x)
      v2
  in
  let v3 = token env v3 (* "`" *) in
  (v1, v2, v3)

and template_substitution (env : env) ((v1, v2, v3) : CST.template_substitution)
    : expr =
  let _v1 = token env v1 (* "${" *) in
  let v2 = expressions env v2 in
  let _v3 = token env v3 (* "}" *) in
  v2

and map_template_literal_type (env : env)
    ((v1, v2, v3) : CST.template_literal_type) : type_ =
  let lback = (* "`" *) token env v1 in
  let xs =
    Common.map
      (fun x ->
        match x with
        | `Temp_chars tok ->
            TyLiteral (String (* template_chars *) (str env tok))
        | `Temp_type x ->
            let _, x, _ = map_template_type env x in
            x)
      v2
  in
  let _rback = (* "`" *) token env v3 in
  TypeTodo (("TemplateLitType", lback), xs |> Common.map (fun x -> Type x))

and map_template_type (env : env) ((v1, v2, v3) : CST.template_type) :
    type_ bracket =
  let l = (* "${" *) token env v1 in
  let ty =
    match v2 with
    | `Prim_type x -> primary_type env x
    | `Infer_type x -> map_infer_type env x
  in
  let r = (* "}" *) token env v3 in
  (l, ty, r)

and decorator (env : env) ((v1, v2) : CST.decorator) : attribute =
  let v1 = token env v1 (* "@" *) in
  let ids, args_opt =
    match v2 with
    | `Id x ->
        let id = identifier env x in
        ([ id ], None)
    | `Deco_member_exp x ->
        let ids = decorator_member_expression env x in
        (ids, None)
    | `Deco_call_exp x ->
        let ids, args = decorator_call_expression env x in
        (ids, Some args)
  in
  NamedAttr (v1, ids, args_opt)

and internal_module (env : env) ((v1, v2) : CST.internal_module) =
  let _v1 = token env v1 (* "namespace" *) in
  let v2 = module__ env v2 in
  v2

and anon_opt_opt_choice_exp_rep_COMMA_opt_choice_exp_208ebb4 (env : env)
    (opt : CST.anon_opt_opt_choice_exp_rep_COMMA_opt_choice_exp_208ebb4) =
  match opt with
  | Some (v1, v2) ->
      let v1 =
        match v1 with
        | Some x -> [ anon_choice_exp_9818c1b env x ]
        | None -> []
      in
      let v2 = anon_rep_COMMA_opt_choice_exp_ca698a5 env v2 in
      v1 @ v2
  | None -> []

and for_header (env : env) ((v1, v2, v3, v4, v5) : CST.for_header) : for_header
    =
  let _open = token env v1 (* "(" *) in
  let var_or_expr =
    match v2 with
    | `Choice_choice_choice_member_exp x ->
        let expr = paren_expr_or_lhs_expr env x in
        Right expr
    | `Choice_var_choice_id (v1, v2) ->
        let var_kind =
          match v1 with
          | `Var tok -> (Var, token env tok) (* "var" *)
          | `Let tok -> (Let, token env tok) (* "let" *)
          | `Const tok -> (Const, token env tok)
          (* "const" *)
        in
        let id_or_pat = id_or_destructuring_pattern env v2 in
        let pat =
          match id_or_pat with
          | Left id -> idexp id
          | Right pat -> pat
        in
        let var = Ast_js.var_pattern_to_var var_kind pat (snd var_kind) None in
        Left var
  in
  let exprs = expressions env v4 in
  let for_header =
    match v3 with
    | `In tok -> (* "in" *) ForIn (var_or_expr, token env tok, exprs)
    | `Of tok -> (* "of" *) ForOf (var_or_expr, token env tok, exprs)
  in
  let _close = token env v5 (* ")" *) in
  for_header

and expr_or_prim_expr (env : env) (x : CST.anon_choice_exp_9cd0ed5) : expr =
  match x with
  | `Exp x -> expression env x
  | `Prim_exp x -> primary_expression env x

and expression (env : env) (x : CST.expression) : expr =
  match x with
  | `As_exp (v1, v2, v3) ->
      (* type assertion of the form 'exp as type' *)
      let e = expression env v1 in
      let tas = token env v2 (* "as" *) in
      let ty =
        match v3 with
        | `Type x -> type_ env x
        | `Temp_lit_type x -> map_template_literal_type env x
      in
      TypeAssert (e, tas, ty)
  | `Inte_module x -> (
      (* namespace (deprecated in favor of ES modules) *)
      (* TODO represent namespaces properly in the AST instead of the nonsense
         below. *)
      let name, opt_body = internal_module env x in
      match opt_body with
      | Some body ->
          let fun_ =
            {
              f_attrs = [];
              f_params = [];
              f_body = body;
              f_rettype = None;
              f_kind = (G.Function, fake);
            }
          in
          Apply (Fun (fun_, Some name), fb [])
      | None -> idexp name)
  | `Type_asse (v1, v2) -> (
      (* type assertion of the form <string>someValue *)
      let t1, xs, _ = type_arguments env v1 in
      let v2 = expression env v2 in
      match xs with
      | [ t ] -> TypeAssert (v2, t1, t)
      | _ -> raise (PI.Ast_builder_error ("wrong type assert expr", t1)))
  | `Prim_exp x -> primary_expression env x
  | `Choice_jsx_elem x ->
      let xml = jsx_element_ env x in
      Xml xml
  | `Jsx_frag x ->
      let xml = jsx_fragment env x in
      Xml xml
  | `Assign_exp (v1, v2, v3) ->
      let v1 = paren_expr_or_lhs_expr env v1 in
      let v2 = token env v2 (* "=" *) in
      let v3 = expression env v3 in
      Assign (v1, v2, v3)
  | `Augm_assign_exp (v1, v2, v3) ->
      let lhs =
        match v1 with
        | `Choice_member_exp x -> (
            match x with
            | `Member_exp x -> member_expression env x
            | `Subs_exp x -> subscript_expression env x
            | `Choice_decl x ->
                let id = reserved_identifier env x in
                idexp id
            | `Id tok ->
                let id = identifier env tok (* identifier *) in
                idexp id
            | `Paren_exp x -> parenthesized_expression env x)
        | `Non_null_exp x -> non_null_expression env x
      in
      let op, is_logical, tok =
        match v2 with
        | `PLUSEQ tok -> (G.Plus, false, token env tok) (* "+=" *)
        | `DASHEQ tok -> (G.Minus, false, token env tok) (* "-=" *)
        | `STAREQ tok -> (G.Mult, false, token env tok) (* "*=" *)
        | `SLASHEQ tok -> (G.Div, false, token env tok) (* "/=" *)
        | `PERCEQ tok -> (G.Mod, false, token env tok) (* "%=" *)
        | `HATEQ tok -> (G.BitXor, false, token env tok) (* "^=" *)
        | `AMPEQ tok -> (G.BitAnd, false, token env tok) (* "&=" *)
        | `BAREQ tok -> (G.BitOr, false, token env tok) (* "|=" *)
        | `GTGTEQ tok -> (G.LSR, false, token env tok) (* ">>=" *)
        | `GTGTGTEQ tok -> (G.ASR, false, token env tok) (* ">>>=" *)
        | `LTLTEQ tok -> (G.LSL, false, token env tok) (* "<<=" *)
        | `STARSTAREQ tok -> (G.Pow, false, token env tok) (* "**=" *)
        | `AMPAMPEQ tok -> (G.And, true, token env tok) (* "&&=" *)
        | `BARBAREQ tok -> (G.Or, true, token env tok) (* "||=" *)
        | `QMARKQMARKEQ tok -> (G.Nullish, true, token env tok)
        (* "??=" *)
      in
      let rhs = expression env v3 in
      (* less: should use intermediate instead of repeating v1 *)
      if is_logical then
        Apply (IdSpecial (ArithOp op, tok), fb [ lhs; Assign (lhs, tok, rhs) ])
      else
        Assign (lhs, tok, Apply (IdSpecial (ArithOp op, tok), fb [ lhs; rhs ]))
  | `Await_exp (v1, v2) ->
      let v1 = token env v1 (* "await" *) in
      let v2 = expression env v2 in
      Apply (IdSpecial (Await, v1), fb [ v2 ])
  | `Un_exp x -> unary_expression env x
  | `Bin_exp x -> binary_expression env x
  | `Tern_exp (v1, v2, v3, v4, v5) ->
      let v1 = expression env v1 in
      let _v2 = token env v2 (* "?" *) in
      let v3 = expression env v3 in
      let _v4 = token env v4 (* ":" *) in
      let v5 = expression env v5 in
      Conditional (v1, v3, v5)
  | `Update_exp x -> update_expression env x
  | `New_exp (v1, v2, v3, v4) ->
      let v1 = token env v1 (* "new" *) in
      let v2 = primary_expression env v2 in
      (* TODO types *)
      let _v3TODO =
        match v3 with
        | Some x -> type_arguments env x |> PI.unbracket
        | None -> []
      in
      let t1, xs, t2 =
        match v4 with
        | Some x -> arguments env x
        | None -> fb []
      in
      (* less: we should remove the extra Apply but that's what we do in pfff*)
      let newcall = Apply (IdSpecial (New, v1), fb [ v2 ]) in
      Apply (newcall, (t1, xs, t2))
  | `Yield_exp (v1, v2) ->
      let v1 = token env v1 (* "yield" *) in
      let v2 =
        match v2 with
        | `STAR_exp (v1bis, v2) ->
            let _v1bis = token env v1bis (* "*" *) in
            let v2 = expression env v2 in
            Apply (IdSpecial (YieldStar, v1), fb [ v2 ])
        | `Opt_exp opt -> (
            match opt with
            | Some x ->
                let x = expression env x in
                Apply (IdSpecial (Yield, v1), fb [ x ])
            | None -> Apply (IdSpecial (Yield, v1), fb []))
      in
      v2

and paren_expr_or_lhs_expr (env : env)
    (x :
      [ `Paren_exp of CST.parenthesized_expression
      | `Choice_choice_member_exp of CST.lhs_expression ]) : expr =
  match x with
  | `Paren_exp x -> parenthesized_expression env x
  | `Choice_choice_member_exp x -> lhs_expression env x

and primary_type (env : env) (x : CST.primary_type) : type_ =
  match x with
  | `Temp_lit_type x -> map_template_literal_type env x
  | `Paren_type (v1, v2, v3) ->
      let _v1 = token env v1 (* "(" *) in
      let v2 = type_ env v2 in
      let _v3 = token env v3 (* ")" *) in
      v2
  | `Pred_type x ->
      let id = predefined_type env x in
      (* less: could also be a G.TyBuiltin *)
      TyName [ id ]
  | `Id tok ->
      let id = identifier env tok (* identifier *) in
      TyName [ id ]
  | `Nested_type_id x ->
      let xs = nested_type_identifier env x in
      TyName xs
  | `Gene_type x -> TyName (generic_type env x)
  | `Obj_type x ->
      let t1, xs, t2 = object_type env x in
      let _xs =
        xs
        |> Common.map_filter (function
             (* TODO *)
             | Left _fld -> None
             | Right _sts -> None)
      in
      TyRecordAnon (t1, (), t2)
  | `Array_type (v1, v2, v3) ->
      let type_ = primary_type env v1 in
      let open_ = token env v2 (* "[" *) in
      let close = token env v3 (* "]" *) in
      TyArray (type_, (open_, (), close))
  | `Tuple_type (v1, v2, v3, v4) ->
      let open_ = token env v1 (* "[" *) in
      let members =
        match v2 with
        | None -> []
        | Some (v1, v2) -> map_sep_list env v1 v2 tuple_type_member
      in
      let _trailing_comma = optional env v3 token (* "," *) in
      let close = token env v4 (* "]" *) in
      TyTuple (open_, members, close)
  | `Flow_maybe_type (v1, v2) ->
      let v1 = token env v1 (* "?" *) in
      let v2 = primary_type env v2 in
      TyQuestion (v1, v2)
  | `Type_query x -> type_query env x
  | `Index_type_query (v1, v2) ->
      let keyof = token env v1 (* "keyof" *) in
      let type_ = primary_type env v2 in
      TypeTodo (("KeyOf", keyof), [ Type type_ ])
  | `This tok ->
      let v1 = token env tok in
      (* "this" *)
      TypeTodo (("This", v1), [])
  | `Exis_type tok ->
      let v1 = token env tok (* "*" *) in
      TypeTodo (("*", v1), [])
  | `Lit_type x ->
      let v1 = literal_type env x in
      TypeTodo (("LitType", fake), [ Expr v1 ])
  | `Lookup_type (v1, v2, v3, v4) ->
      let v1 = primary_type env v1 in
      let v2 = token env v2 (* "[" *) in
      let v3 = type_ env v3 in
      let _v4 = token env v4 (* "]" *) in
      TypeTodo (("LookupType", v2), [ Type v1; Type v3 ])
  | `Cond_type (v1, v2, v3, v4, v5, v6, v7) ->
      let v1 = type_ env v1 in
      let _v2 = token env v2 (* "extends" *) in
      let v3 = type_ env v3 in
      let v4 = token env v4 (* "?" *) in
      let v5 = type_ env v5 in
      let _v6 = token env v6 (* ":" *) in
      let v7 = type_ env v7 in
      TypeTodo (("ConditionalType", v4), [ Type v1; Type v3; Type v5; Type v7 ])

and index_signature (env : env) ((v1, v2, v3, v4, v5) : CST.index_signature) =
  let _v1 =
    match v1 with
    | Some (v1, v2) ->
        let v1 =
          match v1 with
          | Some tok -> Some (token env tok) (* "-" *)
          | None -> None
        in
        let v2 = token env v2 (* "readonly" *) in
        Some (v1, v2)
        (* TODO add to AST *)
    | None -> None
  in
  let v2 = token env v2 (* "[" *) in
  let v3 =
    match v3 with
    | `Choice_id_COLON_type (v2, v3, v4) ->
        let v2 = id_or_reserved_id env v2 in
        let v3 = token env v3 (* ":" *) in
        let v4 = type_ env v4 in
        TypeTodo (("IndexKey", v3), [ Type (TyName [ v2 ]); Type v4 ])
    | `Mapped_type_clause x -> mapped_type_clause env x
  in
  let _v4 = token env v4 (* "]" *) in
  let v5 =
    match v5 with
    | `Type_anno x -> type_annotation env x |> snd
    | `Omit_type_anno (v1, v2) ->
        let _v1_TODO = token env v1 (* "-?:" *) in
        let v2 = type_ env v2 in
        v2
    | `Opting_type_anno (v1, v2) ->
        let _v1_TODO = token env v1 (* "?:" *) in
        let v2 = type_ env v2 in
        v2
  in
  TypeTodo (("Indexsig", v2), [ Type v3; Type v5 ])

and type_query (env : env) ((v1, v2) : CST.type_query) : type_ =
  let ttypeof = (* "typeof" *) token env v1 in
  let e =
    match v2 with
    | `Type_query_subs_exp x -> map_type_query_subscript_expression env x
    | `Type_query_member_exp x -> map_type_query_member_expression env x
    | `Type_query_call_exp x -> map_type_query_call_expression env x
    | `Id tok ->
        let id = (* identifier *) str env tok in
        idexp_or_special id
  in
  TypeTodo (("Typeof", ttypeof), [ Expr e ])

and map_anon_choice_type_id_e96bf13 (env : env)
    (x : CST.anon_choice_type_id_e96bf13) : expr =
  match x with
  | `Id tok ->
      let id = (* identifier *) str env tok in
      idexp_or_special id
  | `Type_query_subs_exp x -> map_type_query_subscript_expression env x
  | `Type_query_member_exp x -> map_type_query_member_expression env x
  | `Type_query_call_exp x -> map_type_query_call_expression env x

and map_type_query_call_expression (env : env)
    ((v1, v2) : CST.type_query_call_expression) : expr =
  let e =
    match v1 with
    (* ?? what is that? *)
    | `Import tok ->
        let id = (* import *) str env tok in
        idexp_or_special id
    | `Id tok ->
        let id = (* identifier *) str env tok in
        idexp_or_special id
    | `Type_query_member_exp x -> map_type_query_member_expression env x
    | `Type_query_subs_exp x -> map_type_query_subscript_expression env x
  in
  let args = arguments env v2 in
  Apply (e, args)

and map_type_query_member_expression (env : env)
    ((v1, v2, v3) : CST.type_query_member_expression) : expr =
  let e = map_anon_choice_type_id_e96bf13 env v1 in
  let tdot = map_anon_choice_DOT_d88d0af env v2 in
  let fld = map_anon_choice_priv_prop_id_89abb74 env v3 in
  ObjAccess (e, tdot, PN fld)

and map_type_query_subscript_expression (env : env)
    ((v1, v2, v3, v4, v5) : CST.type_query_subscript_expression) : expr =
  let e = map_anon_choice_type_id_e96bf13 env v1 in
  let _v2TODO =
    match v2 with
    | Some tok -> Some ((* "?." *) token env tok)
    | None -> None
  in
  let lbra = (* "[" *) token env v3 in
  let arg =
    match v4 with
    | `Pred_type x ->
        (* ?? *)
        let id = predefined_type env x in
        (* TODO? ExprTodo (TyName id)? *)
        Id id
    | `Str x ->
        let s = string_ env x in
        L (String s)
    | `Num tok ->
        let n = (* number *) number env tok in
        L (Num n)
  in
  let rbra = (* "]" *) token env v5 in
  ArrAccess (e, (lbra, arg, rbra))

and unary_expression (env : env) (x : CST.unary_expression) =
  match x with
  | `BANG_exp (v1, v2) ->
      let v1 = token env v1 (* "!" *) in
      let v2 = expression env v2 in
      Apply (IdSpecial (ArithOp G.Not, v1), fb [ v2 ])
  | `TILDE_exp (v1, v2) ->
      let v1 = token env v1 (* "~" *) in
      let v2 = expression env v2 in
      Apply (IdSpecial (ArithOp G.BitNot, v1), fb [ v2 ])
  | `DASH_exp (v1, v2) ->
      let v1 = token env v1 (* "-" *) in
      let v2 = expression env v2 in
      Apply (IdSpecial (ArithOp G.Minus, v1), fb [ v2 ])
  | `PLUS_exp (v1, v2) ->
      let v1 = token env v1 (* "+" *) in
      let v2 = expression env v2 in
      Apply (IdSpecial (ArithOp G.Plus, v1), fb [ v2 ])
  | `Typeof_exp (v1, v2) ->
      let v1 = token env v1 (* "typeof" *) in
      let v2 = expression env v2 in
      Apply (IdSpecial (Typeof, v1), fb [ v2 ])
  | `Void_exp (v1, v2) ->
      let v1 = token env v1 (* "void" *) in
      let v2 = expression env v2 in
      Apply (IdSpecial (Void, v1), fb [ v2 ])
  | `Delete_exp (v1, v2) ->
      let v1 = token env v1 (* "delete" *) in
      let v2 = expression env v2 in
      Apply (IdSpecial (Delete, v1), fb [ v2 ])

and pat_or_assign_pat (env : env) (x : CST.anon_choice_pat_3297d92) :
    (a_ident, a_pattern) either =
  match x with
  | `Pat x -> pattern env x
  | `Assign_pat (v1, v2, v3) ->
      let pat = pattern env v1 in
      let _eq = token env v2 (* "=" *) in
      let _default = expression env v3 in
      pat

and formal_parameter (env : env) (x : CST.formal_parameter) : parameter =
  let id_or_pat, opt_type, opt_default =
    match x with
    | `Requ_param (v1, v2, v3) ->
        (* required_parameter *)
        let id_or_pat = parameter_name env v1 in
        let type_ =
          match v2 with
          | Some x -> Some (type_annotation env x |> snd)
          | None -> None
        in
        let default =
          match v3 with
          | Some x -> Some (initializer_ env x)
          | None -> None
        in
        (id_or_pat, type_, default)
    | `Opt_param (v1, v2, v3, v4) ->
        (* optional_parameter *)
        let id_or_pat = parameter_name env v1 in
        let _questionmark_TODO = token env v2 (* "?" *) in
        let opt_type =
          match v3 with
          | Some x -> Some (type_annotation env x |> snd)
          | None -> None
        in
        let opt_default =
          match v4 with
          | Some x -> Some (initializer_ env x)
          | None -> None
        in
        (id_or_pat, opt_type, opt_default)
  in
  match id_or_pat with
  | Left id ->
      ParamClassic
        {
          p_name = id;
          p_default = opt_default;
          p_dots = None;
          p_type = opt_type;
          p_attrs = [];
        }
  | Right pat ->
      let pat =
        match opt_type with
        | None -> pat
        | Some type_ -> Cast (pat, PI.unsafe_fake_info ":", type_)
      in
      let pat =
        match opt_default with
        | None -> pat
        | Some expr -> Assign (pat, PI.unsafe_fake_info "=", expr)
      in
      ParamPattern pat

and formal_parameters (env : env) ((v1, v2, v3) : CST.formal_parameters) :
    parameter list =
  let _open = token env v1 (* "(" *) in
  let params =
    match v2 with
    | Some (v1, v2, v3) ->
        let params = map_sep_list env v1 v2 formal_parameter in
        let _trailing_comma = optional env v3 token (* "," *) in
        params
    | None -> []
  in
  let _close = token env v3 (* ")" *) in
  params

(* class Component<Props = any, State = any> { ... *)
and default_type (env : env) ((v1, v2) : CST.default_type) =
  let _v1 = token env v1 (* "=" *) in
  let v2 = type_ env v2 in
  v2

and switch_body (env : env) ((v1, v2, v3) : CST.switch_body) =
  let _v1 = token env v1 (* "{" *) in
  let v2 =
    Common.map
      (fun x ->
        match x with
        | `Switch_case x -> switch_case env x
        | `Switch_defa x -> switch_default env x)
      v2
  in
  let _v3 = token env v3 (* "}" *) in
  v2

and mapped_type_clause (env : env) ((v1, v2, v3, v4) : CST.mapped_type_clause) =
  let id = str env v1 (* identifier *) in
  let tin = token env v2 (* "in" *) in
  let ty = type_ env v3 in
  let asopt =
    match v4 with
    | Some (v1, v2) ->
        let _tas = (* "as" *) token env v1 in
        let ty = type_ env v2 in
        [ Type ty ]
    | None -> []
  in
  TypeTodo (("MappedType", tin), [ Expr (Id id); Type ty ] @ asopt)

and statement1 (env : env) (x : CST.statement) : stmt =
  statement env x |> unsafe_stmt1

and statement (env : env) (x : CST.statement) : stmt list =
  match x with
  | `Export_stmt x ->
      let xs = export_statement env x in
      xs
  | `Import_stmt (v1, v2, v3, v4) ->
      let v1 = token env v1 (* "import" *) in
      let import_tok = v1 in
      let _v2 =
        match v2 with
        | Some x -> Some (type_or_typeof env x)
        | None -> None
      in
      let v3 =
        match v3 with
        | `Import_clause_from_clause (v1, v2) ->
            let f = import_clause env v1 in
            let _t, from_path = from_clause env v2 in
            f import_tok from_path
        | `Import_requ_clause x -> [ import_require_clause v1 env x ]
        | `Str x ->
            let file = string_ env x in
            [ ImportFile (import_tok, file) ]
      in
      let _v4 = semicolon env v4 in
      v3 |> Common.map (fun m -> M m)
  | `Debu_stmt (v1, v2) ->
      let v1 = identifier env v1 (* "debugger" *) in
      let v2 = semicolon env v2 in
      [ ExprStmt (idexp v1, v2) ]
  | `Exp_stmt x ->
      let e, t = expression_statement env x in
      [ ExprStmt (e, t) ]
  | `Decl x ->
      let vars = declaration env x in
      vars |> Common.map (fun x -> DefStmt x)
  | `Stmt_blk x -> [ statement_block env x ]
  | `If_stmt (v1, v2, v3, v4) ->
      let v1 = token env v1 (* "if" *) in
      let v2 = parenthesized_expression env v2 in
      let v3 = statement1 env v3 in
      let v4 =
        match v4 with
        | Some (v1, v2) ->
            let _v1 = token env v1 (* "else" *) in
            let v2 = statement1 env v2 in
            Some v2
        | None -> None
      in
      [ If (v1, v2, v3, v4) ]
  | `Switch_stmt (v1, v2, v3) ->
      let v1 = token env v1 (* "switch" *) in
      let v2 = parenthesized_expression env v2 in
      let v3 = switch_body env v3 in
      [ Switch (v1, v2, v3) ]
  | `For_stmt (v1, v2, v3, v4, v5, v6, v7) ->
      let v1 = token env v1 (* "for" *) in
      let _v2 = token env v2 (* "(" *) in
      let v3 =
        match v3 with
        | `Lexi_decl x ->
            let vars = lexical_declaration env x in
            Left vars
        | `Var_decl x ->
            let vars = variable_declaration env x in
            Left vars
        | `Exp_stmt x ->
            let e, _t = expression_statement env x in
            Right e
        | `Empty_stmt tok ->
            let _x = token env tok (* ";" *) in
            Left []
      in
      let v4 =
        match v4 with
        | `Exp_stmt x ->
            let e, _t = expression_statement env x in
            Some e
        | `Empty_stmt tok ->
            let _x = token env tok (* ";" *) in
            None
      in
      let v5 =
        match v5 with
        | Some x -> Some (expressions env x)
        | None -> None
      in
      let _v6 = token env v6 (* ")" *) in
      let v7 = statement1 env v7 in
      [ For (v1, ForClassic (v3, v4, v5), v7) ]
  | `For_in_stmt (v1, v2, v3, v4) ->
      let v1 = token env v1 (* "for" *) in
      let _v2TODO =
        match v2 with
        | Some tok -> Some (token env tok) (* "await" *)
        | None -> None
      in
      let v3 = for_header env v3 in
      let v4 = statement1 env v4 in
      [ For (v1, v3, v4) ]
  | `While_stmt (v1, v2, v3) ->
      let v1 = token env v1 (* "while" *) in
      let v2 = parenthesized_expression env v2 in
      let v3 = statement1 env v3 in
      [ While (v1, v2, v3) ]
  | `Do_stmt (v1, v2, v3, v4, v5) ->
      let v1 = token env v1 (* "do" *) in
      let v2 = statement1 env v2 in
      let _v3 = token env v3 (* "while" *) in
      let v4 = parenthesized_expression env v4 in
      let _v5 = semicolon env v5 in
      [ Do (v1, v2, v4) ]
  | `Try_stmt (v1, v2, v3, v4) ->
      let v1 = token env v1 (* "try" *) in
      let v2 = statement_block env v2 in
      let v3 =
        match v3 with
        | Some x -> Some (catch_clause env x)
        | None -> None
      in
      let v4 =
        match v4 with
        | Some x -> Some (finally_clause env x)
        | None -> None
      in
      [ Try (v1, v2, v3, v4) ]
  | `With_stmt (v1, v2, v3) ->
      let v1 = token env v1 (* "with" *) in
      let v2 = parenthesized_expression env v2 in
      let v3 = statement1 env v3 in
      [ With (v1, v2, v3) ]
  | `Brk_stmt (v1, v2, v3) ->
      let v1 = token env v1 (* "break" *) in
      let v2 =
        match v2 with
        | Some tok -> Some (identifier env tok) (* identifier *)
        | None -> None
      in
      let v3 = semicolon env v3 in
      [ Break (v1, v2, v3) ]
  | `Cont_stmt (v1, v2, v3) ->
      let v1 = token env v1 (* "continue" *) in
      let v2 =
        match v2 with
        | Some tok -> Some (identifier env tok) (* identifier *)
        | None -> None
      in
      let v3 = semicolon env v3 in
      [ Continue (v1, v2, v3) ]
  | `Ret_stmt (v1, v2, v3) ->
      let v1 = token env v1 (* "return" *) in
      let v2 =
        match v2 with
        | Some x -> Some (expressions env x)
        | None -> None
      in
      let v3 = semicolon env v3 in
      [ Return (v1, v2, v3) ]
  | `Throw_stmt (v1, v2, v3) ->
      let v1 = token env v1 (* "throw" *) in
      let v2 = expressions env v2 in
      let v3 = semicolon env v3 in
      [ Throw (v1, v2, v3) ]
  | `Empty_stmt tok -> [ empty_stmt env tok (* ";" *) ]
  | `Labe_stmt (v1, v2, v3) ->
      let v1 = id_or_reserved_id env v1 in
      let _v2 = token env v2 (* ":" *) in
      let v3 = statement1 env v3 in
      [ Label (v1, v3) ]

and method_definition (env : env)
    ((v1, v2, v2bis, v3, v4, v5, v6, v7, v8, v9) : CST.method_definition) :
    property =
  let v1 = accessibility_modifier_opt_to_list env v1 in
  let v2 = kwd_attr_opt_to_list env Static v2 in
  let v2bis = kwd_attr_opt_to_list env Override v2bis in
  let v3 = kwd_attr_opt_to_list env Readonly v3 in
  let v4 = kwd_attr_opt_to_list env Async v4 in
  let v5 =
    match v5 with
    | Some x -> [ anon_choice_get_8fb02de env x ]
    | None -> []
  in
  let v6 = property_name env v6 in
  let v7 =
    match v7 with
    (* indicates optional method? *)
    | Some tok -> [ (Optional, token env tok) ] (* "?" *)
    | None -> []
  in
  let _tparams, (v8, tret) = call_signature env v8 in
  let v9 = statement_block env v9 in
  let attrs = v1 @ v2 @ v2bis @ v3 @ v4 @ v5 @ v7 |> Common.map attr in
  let f_kind = (G.Method, fake) in
  let f =
    { f_attrs = []; f_params = v8; f_body = v9; f_rettype = tret; f_kind }
  in
  let e = Fun (f, None) in
  let ty = None in
  Field { fld_name = v6; fld_attrs = attrs; fld_type = ty; fld_body = Some e }

and class_declaration (env : env)
    ((v1, v2, v3, v4, v5, v6, v7) : CST.class_declaration) : definition =
  let v1 = Common.map (decorator env) v1 in
  let v2 = token env v2 (* "class" *) in
  let v3 = identifier env v3 (* identifier *) in
  (* TODO types: type_parameters *)
  let _v4 =
    match v4 with
    | Some x -> type_parameters env x
    | None -> []
  in
  let c_extends, c_implements =
    match v5 with
    | Some x -> class_heritage env x
    | None -> ([], [])
  in
  let v6 = class_body env v6 in
  let _v7 = automatic_semicolon_opt env v7 in
  let c =
    {
      c_kind = (G.Class, v2);
      c_extends;
      c_implements;
      c_body = v6;
      c_attrs = v1;
    }
  in
  (basic_entity v3, ClassDef c)

and array_ (env : env) ((v1, v2, v3) : CST.array_) =
  let v1 = token env v1 (* "[" *) in
  let v2 = anon_opt_opt_choice_exp_rep_COMMA_opt_choice_exp_208ebb4 env v2 in
  let v3 = token env v3 (* "]" *) in
  Arr (v1, v2, v3)

and export_statement (env : env) (x : CST.export_statement) : stmt list =
  match x with
  | `Choice_export_choice_STAR_from_clause_choice_auto_semi x -> (
      match x with
      | `Export_choice_STAR_from_clause_choice_auto_semi (v1, v2) ->
          let export_tok = token env v1 (* "export" *) in
          let v2 =
            match v2 with
            | `STAR_from_clause_choice_auto_semi (v1, v2, v3) ->
                (* export * from 'foo'; *)
                let star = token env v1 (* "*" *) in
                let from, path = from_clause env v2 in
                let _v3 = semicolon env v3 in
                [ M (ReExportNamespace (export_tok, star, None, from, path)) ]
            | `Name_import_from_clause_choice_auto_semi (v1, v2, v3) ->
                (* export * as foo from "module"; *)
                let star, alias = namespace_import env v1 (* * as foo *) in
                let from, path = from_clause env v2 (* from "module" *) in
                let _semi = semicolon env v3 (* ; *) in
                [
                  M
                    (ReExportNamespace (export_tok, star, Some alias, from, path));
                ]
            | `Export_clause_from_clause_choice_auto_semi (v1, v2, v3) ->
                (* export { name1, name2, nameN } from 'foo'; *)
                let v1 = export_clause env v1 in
                let tok2, path = from_clause env v2 in
                let _v3 = semicolon env v3 in
                v1
                |> List.concat_map (fun (n1, n2opt) ->
                       let tmpname = ("!tmp_" ^ fst n1, snd n1) in
                       let import = Import (tok2, (n1, Some tmpname), path) in
                       let e = idexp tmpname in
                       match n2opt with
                       | None ->
                           let v = Ast_js.mk_const_var n1 e in
                           [ M import; DefStmt v; M (Export (export_tok, n1)) ]
                       | Some n2 ->
                           let v = Ast_js.mk_const_var n2 e in
                           [ M import; DefStmt v; M (Export (export_tok, n2)) ])
            | `Export_clause_choice_auto_semi (v1, v2) ->
                (* export { import1 as name1, import2 as name2, nameN } from 'foo'; *)
                let v1 = export_clause env v1 in
                let _v2 = semicolon env v2 in
                v1
                |> List.concat_map (fun (n1, n2opt) ->
                       match n2opt with
                       | None -> [ M (Export (export_tok, n1)) ]
                       | Some n2 ->
                           let v = Ast_js.mk_const_var n2 (idexp n1) in
                           [ DefStmt v; M (Export (export_tok, n2)) ])
          in
          v2
      | `Rep_deco_export_choice_decl (v1, v2, v3) ->
          let decorators = Common.map (decorator env) v1 in
          let export_tok = token env v2 (* "export" *) in
          let v3 =
            match v3 with
            | `Decl x ->
                let defs = declaration env x in
                defs
                |> List.concat_map (fun def ->
                       let ent, defkind = def in
                       let n = ent.name in
                       let ent = { ent with attrs = ent.attrs @ decorators } in
                       [ DefStmt (ent, defkind); M (Export (export_tok, n)) ])
            | `Defa_choice_decl (v1, v2) -> (
                let tok_default (* TODO *) = token env v1 (* "default" *) in
                match v2 with
                | `Decl x ->
                    let defs = declaration env x in
                    defs
                    |> List.concat_map (fun def ->
                           let ent, defkind = def in
                           let ent =
                             { ent with attrs = ent.attrs @ decorators }
                           in
                           let def = (ent, defkind) in

                           let default_decl, default_name =
                             let expr = idexp ent.name in
                             Ast_js.mk_default_entity_def tok_default expr
                           in

                           (*
                         We translate into 3 statements:

                           export default const foo = bar

                         -->

                           const foo = bar
                           const !default! = foo
                           export !default!
                      *)
                           [
                             DefStmt def;
                             DefStmt default_decl;
                             M (Export (export_tok, default_name));
                           ])
                | `Exp_choice_auto_semi (v1, v2) ->
                    let e = expression env v1 in
                    let _semi = semicolon env v2 in
                    let def, n = Ast_js.mk_default_entity_def tok_default e in
                    [ DefStmt def; M (Export (export_tok, n)) ])
          in
          v3)
  | `Export_type_export_clause (v1, v2, v3) ->
      let _export = token env v1 (* "export" *) in
      let _type = token env v2 (* "type" *) in
      let _exported_types = export_clause env v3 in
      (* TODO: 'export type { foo, type bar, typeof thing };' *)
      []
  | `Export_EQ_id_choice_auto_semi (v1, v2, v3, v4) ->
      let _v1 = token env v1 (* "export" *) in
      let _v2 = token env v2 (* "=" *) in
      let _v3 = token env v3 (* identifier *) in
      let _v4 = semicolon env v4 in
      (* TODO 'export = ZipCodeValidator;' *)
      []
  | `Export_as_name_id_choice_auto_semi (v1, v2, v3, v4, v5) ->
      let _v1 = token env v1 (* "export" *) in
      let _v2 = token env v2 (* "as" *) in
      let _v3 = token env v3 (* "namespace" *) in
      let _v4 = token env v4 (* identifier *) in
      let _v5 = semicolon env v5 in
      (* TODO 'export as namespace mathLib;' *)
      []

and type_annotation (env : env) ((v1, v2) : CST.type_annotation) =
  let v1 = token env v1 (* ":" *) in
  let v2 = type_ env v2 in
  (v1, v2)

and anon_rep_COMMA_opt_choice_exp_ca698a5 (env : env)
    (xs : CST.anon_rep_COMMA_opt_choice_exp_ca698a5) =
  List.filter_map
    (fun (v1, v2) ->
      let _v1 = token env v1 (* "," *) in
      let v2 =
        match v2 with
        | Some x -> Some (anon_choice_exp_9818c1b env x)
        | None -> None
      in
      v2)
    xs

and decorator_call_expression (env : env)
    ((v1, v2) : CST.decorator_call_expression) =
  let v1 = anon_choice_type_id_b8f8ced env v1 in
  let v2 = arguments env v2 in
  (v1, v2)

and update_expression (env : env) (x : CST.update_expression) =
  match x with
  | `Exp_choice_PLUSPLUS (v1, v2) ->
      let v1 = expression env v1 in
      let op, t = anon_choice_PLUSPLUS_e498e28 env v2 in
      Apply (IdSpecial (IncrDecr (op, G.Postfix), t), fb [ v1 ])
  | `Choice_PLUSPLUS_exp (v1, v2) ->
      let op, t = anon_choice_PLUSPLUS_e498e28 env v1 in
      let v2 = expression env v2 in
      Apply (IdSpecial (IncrDecr (op, G.Prefix), t), fb [ v2 ])

and anon_choice_export_stmt_f90d83f (env : env)
    (x : CST.anon_choice_export_stmt_f90d83f) =
  match x with
  | `Export_stmt x ->
      let xs = export_statement env x in
      Right xs
  | `Prop_sign (v1, v2, v2bis, v3, v4, v5, v6) ->
      let v1 = accessibility_modifier_opt_to_list env v1 in
      let v2 = kwd_attr_opt_to_list env Static v2 in
      let v2bis = kwd_attr_opt_to_list env Override v2bis in
      let v3 = kwd_attr_opt_to_list env Readonly v3 in
      let v4 = property_name env v4 in
      let v5 =
        match v5 with
        | Some tok -> [ (Optional, token env tok) ] (* "?" *)
        | None -> []
      in
      let v6 =
        match v6 with
        | Some x -> Some (type_annotation env x |> snd)
        | None -> None
      in
      let attrs = v1 @ v2 @ v2bis @ v3 @ v5 |> Common.map attr in
      let fld =
        { fld_name = v4; fld_attrs = attrs; fld_type = v6; fld_body = None }
      in
      Left (Field fld)
  | `Call_sign_ x ->
      let _tparams, x = call_signature env x in
      let ty = mk_functype x in
      let name = PN ("CTOR??TODO", fake) in
      let fld =
        { fld_name = name; fld_attrs = []; fld_type = Some ty; fld_body = None }
      in
      Left (Field fld)
  | `Cons_sign (v1, v2, v3, v4) ->
      let v1 = token env v1 (* "new" *) in
      let _tparams =
        match v2 with
        | Some x -> type_parameters env x
        | None -> []
      in
      let v3 = formal_parameters env v3 in
      let v4 =
        match v4 with
        | Some x -> Some (type_annotation env x |> snd)
        | None -> None
      in
      let ty = mk_functype (v3, v4) in
      let name = PN ("new", v1) in
      let fld =
        { fld_name = name; fld_attrs = []; fld_type = Some ty; fld_body = None }
      in
      Left (Field fld)
  | `Index_sign x ->
      let ty = index_signature env x in
      let name = PN ("IndexMethod??TODO?", fake) in
      let fld =
        { fld_name = name; fld_attrs = []; fld_type = Some ty; fld_body = None }
      in
      Left (Field fld)
  | `Meth_sign x ->
      let x = method_signature env x in
      Left (Field x)

and public_field_definition (env : env)
    ((v1, v2, v3, v4, v5, v6, v7) : CST.public_field_definition) : property =
  let _tok_declare = optional env v1 token in
  let access_modif = accessibility_modifier_opt_to_list env v2 in
  let attributes =
    match v3 with
    | `Opt_static_opt_over_modi_opt_read (v1, v2bis, v2) ->
        let v1 = kwd_attr_opt_to_list env Static v1 in
        let v2 = kwd_attr_opt_to_list env Readonly v2 in
        let v2bis = kwd_attr_opt_to_list env Override v2bis in
        access_modif @ v1 @ v2 @ v2bis
    | `Opt_abst_opt_read (v1, v2)
    | `Opt_read_opt_abst (v2, v1) ->
        let v1 = kwd_attr_opt_to_list env Abstract v1 in
        let v2 = kwd_attr_opt_to_list env Readonly v2 in
        access_modif @ v1 @ v2
  in
  let prop_name = property_name env v4 in
  let _question_or_exclam =
    match v5 with
    | Some x -> (
        match x with
        | `QMARK tok -> [ (Optional, token env tok) ] (* "?" *)
        | `BANG tok -> [ (NotNull, token env tok) ] (* "!" *))
    | None -> []
  in
  let opt_type =
    match v6 with
    | Some x -> Some (type_annotation env x |> snd)
    | None -> None
  in
  let opt_init =
    match v7 with
    | Some x -> Some (initializer_ env x)
    | None -> None
  in
  let attrs = attributes |> Common.map attr in
  Field
    {
      fld_name = prop_name;
      fld_attrs = attrs;
      fld_type = opt_type;
      fld_body = opt_init;
    }

and lexical_declaration (env : env) ((v1, v2, v3, v4) : CST.lexical_declaration)
    : var list =
  let kind =
    match v1 with
    | `Let tok -> (Let, token env tok (* "let" *))
    | `Const tok -> (Const, token env tok (* "const" *))
  in
  let vars = map_sep_list env v2 v3 variable_declarator in
  let _v4 = semicolon env v4 in
  build_vars kind vars

and map_extends_clause (env : env) ((v1, v2, v3, v4) : CST.extends_clause) :
    parent list =
  let _textends = (* "extends" *) token env v1 in
  let v2 = expression env v2 in
  let v3 =
    match v3 with
    | Some x -> type_arguments env x |> PI.unbracket
    | None -> []
  in
  let v4 =
    Common.map
      (fun (v1, v2, v3) ->
        let _v1 = (* "," *) token env v1 in
        let v2 = expression env v2 in
        let v3 =
          match v3 with
          | Some x -> type_arguments env x |> PI.unbracket
          | None -> []
        in
        tyname_or_expr_of_expr v2 v3)
      v4
  in
  tyname_or_expr_of_expr v2 v3 :: v4

and enum_body (env : env) ((v1, v2, v3) : CST.enum_body) =
  let v1 = token env v1 (* "{" *) in
  let body =
    match v2 with
    | Some (v1, v2, v3) ->
        let body = map_sep_list env v1 v2 anon_choice_prop_name_6cc9e4b in
        let _v3 =
          match v3 with
          | Some tok -> Some (token env tok) (* "," *)
          | None -> None
        in
        body
    | None -> []
  in
  let v3 = token env v3 (* "}" *) in
  (v1, body, v3)

and class_heritage (env : env) (x : CST.class_heritage) :
    parent list * type_ list =
  match x with
  | `Extends_clause_opt_imples_clause (v1, v2) ->
      let v1 = map_extends_clause env v1 in
      let v2 =
        match v2 with
        | Some x -> implements_clause env x
        | None -> []
      in
      (v1, v2)
  | `Imples_clause x ->
      let x = implements_clause env x in
      ([], x)

and property_name (env : env) (x : CST.property_name) =
  match x with
  | `Choice_id x ->
      let id = id_or_reserved_id env x in
      PN id
  | `Priv_prop_id tok ->
      let id = str env tok in
      PN id
  | `Str x ->
      let s = string_ env x in
      PN s
  | `Num tok ->
      let n = number_as_string env tok (* number *) in
      PN n
  | `Comp_prop_name (v1, v2, v3) ->
      let _v1 = token env v1 (* "[" *) in
      let v2 = expression env v2 in
      let _v3 = token env v3 (* "]" *) in
      PN_Computed v2

and switch_case (env : env) ((v1, v2, v3, v4) : CST.switch_case) =
  let v1 = token env v1 (* "case" *) in
  let v2 = expressions env v2 in
  let v3 = token env v3 (* ":" *) in
  let v4 = List.concat_map (statement env) v4 in
  Case (v1, v2, stmt1 v3 v4)

and spread_element (env : env) ((v1, v2) : CST.spread_element) =
  let v1 = token env v1 (* "..." *) in
  let v2 = expression env v2 in
  (v1, v2)

and expressions (env : env) (x : CST.expressions) : expr =
  match x with
  | `Exp x -> expression env x
  | `Seq_exp x -> sequence_expression env x

and abstract_method_signature (env : env)
    ((v1, v2, v3, v4, v5, v6) : CST.abstract_method_signature) =
  let v1 = accessibility_modifier_opt_to_list env v1 in
  let v2 = [ (Abstract, token env v2) ] (* "abstract" *) in
  let v3 =
    match v3 with
    | Some x -> [ anon_choice_get_8fb02de env x ]
    | None -> []
  in
  let v4 = property_name env v4 in
  let v5 =
    match v5 with
    | Some tok -> [ (Optional, token env tok) ] (* "?" *)
    | None -> []
  in
  let attrs = v1 @ v2 @ v3 @ v5 |> Common.map attr in
  let _tparams, x = call_signature env v6 in
  let t = mk_functype x in
  { fld_name = v4; fld_attrs = attrs; fld_type = Some t; fld_body = None }

and finally_clause (env : env) ((v1, v2) : CST.finally_clause) =
  let v1 = token env v1 (* "finally" *) in
  let v2 = statement_block env v2 in
  (v1, v2)

and map_type_predicate (env : env) ((v1, v2, v3) : CST.type_predicate) =
  let v1 =
    match v1 with
    | `Id tok ->
        let id = identifier env tok (* identifier *) in
        idexp_or_special id
    | `This tok -> this env tok
  in
  let tis = token env v2 (* "is" *) in
  let ty = type_ env v3 in
  TypeTodo (("IsType", tis), [ Expr v1; Type ty ])

and map_asserts (env : env) ((v1, v2, v3) : CST.asserts) : type_ =
  let tcolon = token env v1 (* ":" *) in
  let _asserts = token env v2 (* "asserts" *) in
  let any =
    match v3 with
    | `Type_pred x -> Type (map_type_predicate env x)
    | `Id tok ->
        let id = identifier env tok (* identifier *) in
        Expr (idexp_or_special id)
    | `This tok -> Expr (this env tok)
  in
  TypeTodo (("Asserts", tcolon), [ any ])

and call_signature (env : env) ((v1, v2, v3) : CST.call_signature) :
    a_type_parameter list * (parameter list * type_ option) =
  let v1 =
    match v1 with
    | Some x -> type_parameters env x
    | None -> []
  in
  let v2 = formal_parameters env v2 in
  let v3 =
    match v3 with
    | Some x -> (
        match x with
        | `Type_anno x -> Some (type_annotation env x |> snd)
        | `Asserts x ->
            let ty = map_asserts env x in
            Some ty
        | `Type_pred_anno (v1, v2) ->
            let _v1 = token env v1 (* ":" *) in
            let v2 = map_type_predicate env v2 in
            Some v2)
    | None -> None
  in
  (v1, (v2, v3))

and object_ (env : env) ((v1, v2, v3) : CST.object_) : a_obj =
  let v1 = token env v1 (* "{" *) in
  let properties =
    match v2 with
    | Some (v1, v2) ->
        map_sep_list env v1 v2 (fun env x ->
            match x with
            | Some x -> [ object_property env x ]
            | None -> [])
        |> List.flatten
    | None -> []
  in
  let v3 = token env v3 (* "}" *) in
  (v1, properties, v3)

and type_predicate (env : env) ((v1, v2, v3) : CST.type_predicate) : type_ =
  let _e =
    match v1 with
    | `Id tok -> (* identifier *) identifier env tok |> idexp
    | `This tok -> (* "this" *) this env tok
  in
  let _is = token env v2 (* "is" *) in
  let type_ = type_ env v3 in
  type_

and type_ (env : env) (x : CST.type_) : type_ =
  match x with
  | `Prim_type x -> primary_type env x
  | `Union_type (v1, v2, v3) -> (
      let v2 = token env v2 (* "|" *) in
      let v3 = type_ env v3 in
      match v1 with
      | Some x ->
          let x = type_ env x in
          TyOr (x, v2, v3)
      | None -> v3
      (* ?? *))
  | `Inte_type (v1, v2, v3) -> (
      let v2 = token env v2 (* "&" *) in
      let v3 = type_ env v3 in
      match v1 with
      | Some x ->
          let x = type_ env x in
          TyAnd (x, v2, v3)
      | None -> v3
      (* ?? *))
  | `Func_type (v1, v2, v3, v4) ->
      let _tparams =
        match v1 with
        | Some x -> type_parameters env x
        | None -> []
      in
      let params = formal_parameters env v2 in
      let _arrow = token env v3 (* "=>" *) in
      let type_ =
        match v4 with
        | `Type x -> type_ env x
        | `Type_pred x ->
            let type_ = type_predicate env x in
            type_
      in
      mk_functype (params, Some type_)
  | `Read_type (v1, v2) ->
      let _TODOreadonly = token env v1 (* "readonly" *) in
      let type_ = type_ env v2 in
      type_
  | `Cons_type (v1, v2, v3, v4, v5) ->
      let v1 = token env v1 (* "new" *) in
      let _tparams =
        match v2 with
        | Some x -> type_parameters env x
        | None -> []
      in
      let v3 = formal_parameters env v3 in
      let _v4 = token env v4 (* "=>" *) in
      let v5 = type_ env v5 in
      let ty = mk_functype (v3, Some v5) in
      TypeTodo (("New", v1), [ Type ty ])
  | `Infer_type x -> map_infer_type env x

and map_infer_type env (v1, v2) =
  let v1 = token env v1 (* "infer" *) in
  let v2 = identifier env v2 (* identifier *) in
  TypeTodo (("Infer", v1), [ Type (TyName [ v2 ]) ])

and type_parameters (env : env) ((v1, v2, v3, v4, v5) : CST.type_parameters) :
    a_type_parameter list =
  let _v1 = token env v1 (* "<" *) in
  let type_params = map_sep_list env v2 v3 type_parameter in
  let _v4 =
    match v4 with
    | Some tok -> Some (token env tok) (* "," *)
    | None -> None
  in
  let _v5 = token env v5 (* ">" *) in
  type_params

and constraint_ (env : env) ((v1, v2) : CST.constraint_) :
    a_type_parameter_constraint =
  let _v1 =
    match v1 with
    | `Extends tok -> token env tok (* "extends" *)
    | `COLON tok -> token env tok
    (* ":" *)
  in
  let v2 = type_ env v2 in
  v2

and parameter_name (env : env) ((v1, v2, v2bis, v3, v4) : CST.parameter_name) :
    (a_ident, a_pattern) Common.either =
  let _decorators = Common.map (decorator env) v1 in
  let _accessibility = accessibility_modifier_opt_to_list env v2 in
  let _override = kwd_attr_opt_to_list env Override v2bis in
  let _readonly = kwd_attr_opt_to_list env Readonly v3 in
  let id_or_pat =
    match v4 with
    | `Pat x -> pattern env x
    | `This tok ->
        (* treating 'this' as a regular identifier for now *)
        let id = identifier env tok (* "this" *) in
        Left id
  in
  id_or_pat

and lhs_expression (env : env) (x : CST.lhs_expression) : expr =
  match x with
  | `Choice_member_exp x -> (
      match x with
      | `Member_exp x -> member_expression env x
      | `Subs_exp x -> subscript_expression env x
      | `Choice_unde x -> identifier_ env x
      | `Choice_decl x -> reserved_identifier env x |> idexp
      | `Dest_pat x -> destructuring_pattern env x)
  | `Non_null_exp x -> non_null_expression env x

and statement_block (env : env) ((v1, v2, v3, v4) : CST.statement_block) =
  let v1 = token env v1 (* "{" *) in
  let v2 = List.concat_map (statement env) v2 in
  let v3 = token env v3 (* "}" *) in
  let _v4 = automatic_semicolon_opt env v4 in
  Block (v1, v2, v3)

and function_declaration (env : env)
    ((v1, v2, v3, v4, v5, v6) : CST.function_declaration) : definition =
  let v1 =
    match v1 with
    | Some tok -> [ attr (Async, token env tok) ] (* "async" *)
    | None -> []
  in
  let v2 = token env v2 (* "function" *) in
  let v3 = identifier env v3 (* identifier *) in
  let _tparams, (v4, tret) = call_signature env v4 in
  let v5 = statement_block env v5 in
  let _v6 = automatic_semicolon_opt env v6 in
  let f_kind = (G.Function, v2) in
  let f =
    { f_attrs = v1; f_params = v4; f_body = v5; f_rettype = tret; f_kind }
  in
  (basic_entity v3, FuncDef f)

and anon_choice_type_id_940079a (env : env)
    (x : CST.anon_choice_type_id_940079a) : (a_ident, a_pattern) either =
  match x with
  | `Id tok -> Left (identifier env tok) (* identifier *)
  | `Dest_pat x -> Right (destructuring_pattern env x)

and id_or_destructuring_pattern env x : (a_ident, a_pattern) either =
  anon_choice_type_id_940079a env x

and rest_pattern (env : env) ((v1, v2) : CST.rest_pattern) :
    tok * (a_ident, a_pattern) either =
  let dots = token env v1 (* "..." *) in
  let id_or_pat = id_or_destructuring_pattern env v2 in
  (dots, id_or_pat)

and tuple_type_member (env : env) (x : CST.tuple_type_member) :
    tuple_type_member =
  match x with
  | `Tuple_param (v1, v2) ->
      (* tuple_parameter *)
      let _v1_TODO =
        match v1 with
        | `Id tok -> Left (identifier env tok) (* identifier *)
        | `Rest_pat x ->
            let _dots, id_or_pat = rest_pattern env x in
            id_or_pat
      in
      let v2 = type_annotation env v2 |> snd in
      TyTupMember v2
  | `Opt_tuple_param (v1, v2, v3) ->
      (* optional_tuple_parameter *)
      let _v1_TODO = identifier env v1 in
      let _v2_TODO = token env v2 (* "?" *) in
      let v3 = type_annotation env v3 |> snd in
      TyTupMember v3
  | `Opt_type (v1, v2) ->
      let v1 = type_ env v1 in
      let v2 = token env v2 (* "?" *) in
      TyTupOpt (v1, v2)
  | `Rest_type (v1, v2) ->
      let v1 = token env v1 (* "..." *) in
      let v2 = type_ env v2 in
      TyTupRest (v1, v2)
  | `Type x -> TyTupMember (type_ env x)

and method_signature (env : env)
    ((v1, v2, v2bis, v3, v4, v5, v6, v7, v8) : CST.method_signature) =
  let v1 = accessibility_modifier_opt_to_list env v1 in
  let v2 = kwd_attr_opt_to_list env Static v2 in
  let v2bis = kwd_attr_opt_to_list env Override v2bis in
  let v3 = kwd_attr_opt_to_list env Readonly v3 in
  let v4 = kwd_attr_opt_to_list env Async v4 in
  let v5 =
    match v5 with
    | Some x -> [ anon_choice_get_8fb02de env x ]
    | None -> []
  in
  let v6 = property_name env v6 in
  let v7 = kwd_attr_opt_to_list env Optional v7 in
  let attrs = v1 @ v2 @ v2bis @ v3 @ v4 @ v5 @ v7 |> Common.map attr in
  let _tparams, x = call_signature env v8 in
  let t = mk_functype x in
  { fld_name = v6; fld_attrs = attrs; fld_type = Some t; fld_body = None }

(* TODO: types *)
(* This covers mostly type definitions but includes also javascript constructs
   like function parameters, so it will be called even if we ignore types. *)
and declaration (env : env) (x : CST.declaration) : definition list =
  match x with
  | `Choice_func_decl x -> (
      match x with
      | `Func_decl x -> [ function_declaration env x ]
      | `Gene_func_decl x -> [ generator_function_declaration env x ]
      | `Class_decl x -> [ class_declaration env x ]
      | `Lexi_decl x -> lexical_declaration env x |> vars_to_defs
      | `Var_decl x -> variable_declaration env x |> vars_to_defs)
  | `Func_sign (v1, v2, v3, v4, v5) ->
      let _v1 =
        match v1 with
        | Some tok -> [ (Async, token env tok) ] (* "async" *)
        | None -> []
      in
      let v2 = token env v2 (* "function" *) in
      let v3 = identifier env v3 (* identifier *) in
      let _tparams, x = call_signature env v4 in
      let ty = mk_functype x in
      let _v5 =
        match v5 with
        | `Choice_auto_semi x -> semicolon env x
        | `Func_sign_auto_semi tok -> token env tok
      in
      [
        ( basic_entity v3,
          (* DefTodo? *)
          VarDef { v_kind = (Const, v2); v_init = None; v_type = Some ty } );
      ]
  | `Abst_class_decl (v1, v2, v3, v4, v5, v6, v7) ->
      let _v1_TODO = Common.map (decorator env) v1 in
      let v2 = attr (Abstract, token env v2) (* "abstract" *) in
      let v3 = token env v3 (* "class" *) in
      let v4 = identifier env v4 (* identifier *) in
      let _tparams =
        match v5 with
        | Some x -> type_parameters env x
        | None -> []
      in
      let c_extends, c_implements =
        match v6 with
        | Some x -> class_heritage env x
        | None -> ([], [])
      in
      let v7 = class_body env v7 in
      let attrs = [ v2 ] in
      let c =
        {
          c_kind = (G.Class, v3);
          c_extends;
          c_implements;
          c_body = v7;
          c_attrs = attrs;
        }
      in
      [ (basic_entity v4, ClassDef c) ]
  | `Module (v1, v2) ->
      (* does this exist only in .d.ts files? *)
      let _v1 = token env v1 (* "module" *) in
      let _id, _opt_body = module__ env v2 in
      []
      (* TODO *)
  | `Inte_module x ->
      (* namespace *)
      let _x = internal_module env x in
      []
      (* TODO *)
  | `Type_alias_decl (v1, v2, v3, v4, v5, v6) ->
      let typekwd = token env v1 (* "type" *) in
      let id = str env v2 (* identifier *) in
      let _tparamsTODO =
        match v3 with
        | Some x -> type_parameters env x
        | None -> []
      in
      let _teq = token env v4 (* "=" *) in
      let ty = type_ env v5 in
      let sc = semicolon env v6 in
      let ent = basic_entity id in
      [ (ent, DefTodo (("typedef", typekwd), [ Type ty; Tk sc ])) ]
  | `Enum_decl (v1, v2, v3, v4) ->
      let _v1 =
        match v1 with
        | Some tok -> [ token env tok ] (* "const" *)
        | None -> []
      in
      let _v2 = token env v2 (* "enum" *) in
      let _v3 = identifier env v3 (* identifier *) in
      let _v4 = enum_body env v4 in
      []
      (* TODO *)
  | `Inte_decl (v1, v2, v3, v4, v5) ->
      let v1 = token env v1 (* "interface" *) in
      let v2 = identifier env v2 (* identifier *) in
      let _v3 =
        match v3 with
        | Some x -> type_parameters env x
        | None -> []
      in
      let v4 =
        match v4 with
        | Some x -> map_extends_type_clause env x
        | None -> []
      in
      let t1, xs, t2 = object_type env v5 in
      let xs =
        xs
        |> Common.map_filter (function
             (* TODO *)
             | Left _fld -> None
             | Right _sts -> None)
      in
      let c =
        {
          c_kind = (G.Interface, v1);
          c_extends = v4;
          c_implements = [];
          c_body = (t1, xs, t2);
          c_attrs = [];
        }
      in
      [ (basic_entity v2, ClassDef c) ]
  | `Import_alias (v1, v2, v3, v4, v5) ->
      let _v1 = token env v1 (* "import" *) in
      let _v2 = identifier env v2 (* identifier *) in
      let _v3 = token env v3 (* "=" *) in
      let _v4 = id_or_nested_id env v4 in
      let _v5 = semicolon env v5 in
      []
      (* TODO *)
  | `Ambi_decl (v1, v2) ->
      let _v1 = token env v1 (* "declare" *) in
      let v2 =
        match v2 with
        | `Decl x -> declaration env x
        | `Global_stmt_blk (v1, v2) ->
            let v1 = token env v1 (* "global" *) in
            let v2 = statement_block env v2 in
            let name = ("!global!", v1) in
            let f_kind = (G.LambdaKind, fake) in
            let f =
              {
                f_attrs = [];
                f_params = [];
                f_body = v2;
                f_rettype = None;
                f_kind;
              }
            in
            (* TODO: DefTodo *)
            [
              ( basic_entity name,
                VarDef
                  {
                    v_kind = (Const, v1);
                    v_init = Some (Fun (f, None));
                    v_type = None;
                  } );
            ]
        | `Module_DOT_id_COLON_type_choice_auto_semi (v1, v2, v3, v4, v5, v6) ->
            let tok_module = token env v1 (* "module" *) in
            let _dot = token env v2 (* "." *) in
            let name = identifier env v3 (* identifier *) in
            let _colon = token env v4 (* ":" *) in
            let type_ = type_ env v5 in
            let _semi = semicolon env v6 in
            (* TODO: DefTodo *)
            [
              ( basic_entity name,
                VarDef
                  {
                    v_kind = (Const, tok_module);
                    v_init = None;
                    v_type = Some type_;
                  } );
            ]
      in
      v2

and map_extends_type_clause (env : env) ((v1, v2, v3) : CST.extends_type_clause)
    : parent list =
  let _textends = (* "extends" *) token env v1 in
  let v2 = map_anon_choice_type_id_a85f573 env v2 in
  let v3 =
    Common.map
      (fun (v1, v2) ->
        let _v1 = (* "," *) token env v1 in
        let v2 = map_anon_choice_type_id_a85f573 env v2 in
        Right v2)
      v3
  in
  Right v2 :: v3

and map_anon_choice_type_id_a85f573 (env : env)
    (x : CST.anon_choice_type_id_a85f573) : type_ =
  match x with
  | `Id tok -> TyName [ (* identifier *) str env tok ]
  | `Nested_type_id x -> TyName (nested_type_identifier env x)
  | `Gene_type x -> TyName (generic_type env x)

let toplevel env x = statement env x

let program (env : env) ((v1, v2) : CST.program) : a_program =
  let _v1 =
    match v1 with
    | Some tok -> Some (token env tok) (* pattern #!.* *)
    | None -> None
  in
  let v2 = List.concat_map (toplevel env) v2 in
  v2

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

type dialect = [ `Typescript | `TSX ]

let guess_dialect opt_dialect file : dialect =
  match opt_dialect with
  | Some x -> x
  | None ->
      (* TODO: should remove the no_sem below, bug in ml_to_generic.ml *)
      if file =~ ".*\\.tsx" then (* nosem *)
        `TSX else `Typescript

type cst_result = CST.program Tree_sitter_run.Parsing_result.t

let parse ?dialect file =
  let debug = false in
  H.wrap_parser
    (fun () ->
      let dialect = guess_dialect dialect file in
      match dialect with
      | `Typescript ->
          let cst = Tree_sitter_typescript.Parse.file file in
          (cst :> cst_result)
      | `TSX ->
          let cst = Tree_sitter_tsx.Parse.file file in
          (cst :> cst_result))
    (fun cst ->
      let env = { H.file; conv = H.line_col_to_pos file; extra = () } in

      if debug then (
        Printexc.record_backtrace true;
        CST.dump_tree cst);

      program env cst)
