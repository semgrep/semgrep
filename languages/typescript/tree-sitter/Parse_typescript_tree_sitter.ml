(**
   Derive a JS/TS AST from a tree-sitter TS (or TSX) CST.

   This is derived from generated code 'semgrep-typescript/lib/Boilerplate.ml'
   in tree-sitter-lang.
*)

open Common
open Fpath_.Operators
open Either_
module AST = Ast_js
module H = Parse_tree_sitter_helpers
module G = AST_generic
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
(* Types *)
(*****************************************************************************)

type context = Program | Pattern
type env = context H.env

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let token = H.token
let str = H.str
let fake = Tok.unsafe_fake_tok ""
let fb = Tok.unsafe_fake_bracket
let mk_functype ((_, params, _), rett) = TyFun (params, rett)

(* Note that this file also raises some Impossible and Ast_builder_error *)
let _todo _env _x = failwith "internal error: not implemented"

(*
   We preserve the distinction between a plain identifier and a more complex
   pattern because function parameters make this distinction.
   This conversion is intended to create a sub-pattern.
*)
let sub_pattern (id_or_pat : (a_ident, a_pattern) Either.t) : a_pattern =
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
    (* THINK: Probably shouldn't have a question mark here. *)
    | ObjAccess (e, (Dot, _), PN id) -> id :: ids_of_expr e
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
    List_.map (fun ((_sep : Tree_sitter_run.Token.t), elt) -> f env elt) tail
  in
  head :: tail

(* Since some patterns parse both with and without the `__SEMGREP_EXPRESSION`
 * prefix, we sometimes parse something without but would like to parse it
 * with the prefix anyways.
 * This function will just detect whether the program is of a form which
 * falls under that. This includes `{ ... }` and `foo: e`.
 *)
let is_semgrep_pattern_we_would_rather_parse_as_expression cst =
  match cst with
  | `Opt_hash_bang_line_rep_stmt
      ( _,
        [
          (* A single statement which is a label looks like `foo: e`, which
             is probably better suited to being a partial pattern.
           *)
          ( `Labe_stmt _
          (* A block statement with only one thing in it, or empty, is probably actually
             a record.
           *)
          | `Stmt_blk (_, ([] | [ _ ]), _, _) );
        ] ) ->
      true
  | _ -> false

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
  Tok.unsafe_fake_tok ";"

let semicolon (env : env) (x : CST.semicolon) =
  match x with
  | `Auto_semi tok -> automatic_semicolon env tok (* automatic_semicolon *)
  | `SEMI tok -> (* ";" *) token env tok

let this env tok = IdSpecial (This, token env tok)
let super env tok = IdSpecial (Super, token env tok)

let number (env : env) (tok : CST.number) =
  let s, t =
    str env tok
    (* number *)
  in
  (* TODO? float_of_string_opt_also_from_hexoctbin *)
  (Float.of_string_opt s, t)

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

let jsx_string (env : env) (x : CST.jsx_string) =
  match x with
  | `DQUOT_rep_choice_unes_double_jsx_str_frag_DQUOT (v1, v2, v3) ->
      let v1 = (* "\"" *) token env v1 in
      let v2 =
        List_.map
          (fun x ->
            match x with
            | `Unes_double_jsx_str_frag tok ->
                (* pattern "([^\"&]|&[^#A-Za-z])+" *) str env tok
            | `Html_char_ref tok ->
                (* pattern &(#([xX][0-9a-fA-F]{1,6}|[0-9]{1,5})|[A-Za-z]{1,30}); *)
                str env tok)
          v2
      in
      let v3 = (* "\"" *) token env v3 in
      let str = v2 |> List_.map fst |> String.concat "" in
      let toks = (v2 |> List_.map snd) @ [ v3 ] in
      (str, Tok.combine_toks v1 toks)
  | `SQUOT_rep_choice_unes_single_jsx_str_frag_SQUOT (v1, v2, v3) ->
      let v1 = (* "'" *) token env v1 in
      let v2 =
        List_.map
          (fun x ->
            match x with
            | `Unes_single_jsx_str_frag tok ->
                (* pattern "([^'&]|&[^#A-Za-z])+" *) str env tok
            | `Html_char_ref tok ->
                (* pattern &(#([xX][0-9a-fA-F]{1,6}|[0-9]{1,5})|[A-Za-z]{1,30}); *)
                str env tok)
          v2
      in
      let v3 = (* "'" *) token env v3 in
      let str = v2 |> List_.map fst |> String.concat "" in
      let toks = (v2 |> List_.map snd) @ [ v3 ] in
      (str, Tok.combine_toks v1 toks)

let string_ (env : env) (x : CST.string_) : string wrap =
  match x with
  | `DQUOT_rep_choice_unes_double_str_frag_DQUOT (v1, v2, v3) ->
      let open_ =
        token env v1
        (* "\"" *)
      in
      let contents =
        List_.map
          (fun x ->
            match x with
            | `Unes_double_str_frag tok ->
                str env tok (* pattern "[^\"\\\\]+" *)
            | `Esc_seq tok -> (* escape_sequence *) str env tok)
          v2
      in
      let close =
        token env v3
        (* "\"" *)
      in
      let str = contents |> List_.map fst |> String.concat "" in
      let toks = (contents |> List_.map snd) @ [ close ] in
      (str, Tok.combine_toks open_ toks)
  | `SQUOT_rep_choice_unes_single_str_frag_SQUOT (v1, v2, v3) ->
      let open_ =
        token env v1
        (* "'" *)
      in
      let v2 =
        List_.map
          (fun x ->
            match x with
            | `Unes_single_str_frag tok -> str env tok (* pattern "[^'\\\\]+" *)
            | `Esc_seq tok -> str env tok
            (* escape_sequence *))
          v2
      in
      let close =
        token env v3
        (* "'" *)
      in
      let str = v2 |> List_.map fst |> String.concat "" in
      let toks = (v2 |> List_.map snd) @ [ close ] in
      (str, Tok.combine_toks open_ toks)

let module_export_name (env : env) (x : CST.module_export_name) =
  match x with
  | `Id tok -> identifier env tok
  | `Str x -> string_ env x

let namespace_import (env : env) ((v1, v2, v3) : CST.namespace_import_export) =
  let star =
    token env v1
    (* "*" *)
  in
  let _as =
    token env v2
    (* "as" *)
  in
  let id =
    identifier env v3
    (* identifier *)
  in
  (star, id)

let jsx_identifier_ (env : env) (x : CST.jsx_identifier_) =
  match x with
  | `Jsx_id tok ->
      str env tok (* pattern [a-zA-Z_$][a-zA-Z\d_$]*-[a-zA-Z\d_$\-]* *)
  | `Id tok -> identifier env tok

let jsx_namespace_name (env : env) ((v1, v2, v3) : CST.jsx_namespace_name) =
  let v1 = jsx_identifier_ env v1 in
  let _v2 =
    token env v2
    (* ":" *)
  in
  let v3 = jsx_identifier_ env v3 in
  (v1, v3)

let jsx_attribute_name (env : env) (x : CST.jsx_attribute_name) =
  match x with
  | `Choice_jsx_id x -> jsx_identifier_ env x
  | `Jsx_name_name x ->
      let id1, id2 = jsx_namespace_name env x in
      let str = fst id1 ^ ":" ^ fst id2 in
      (str, Tok.combine_toks (snd id1) [ snd id2 ])

let rec id_or_nested_id (env : env) (x : CST.anon_choice_type_id_42c0412) :
    a_ident list =
  match x with
  | `Id tok -> [ identifier env tok ] (* identifier *)
  | `Nested_id x -> nested_identifier env x

and nested_identifier (env : env) ((v1, v2, v3) : CST.nested_identifier) =
  let v1 = id_or_nested_id env v1 in
  let _v2 =
    token env v2
    (* "." *)
  in
  let v3 =
    identifier env v3
    (* identifier *)
  in
  v1 @ [ v3 ]

let jsx_element_name (env : env) (x : CST.jsx_element_name) : a_ident =
  match x with
  | `Choice_jsx_id x -> jsx_identifier_ env x
  | `Nested_id x ->
      let xs = nested_identifier env x in
      let str = xs |> List_.map fst |> String.concat "." in
      let hd, tl =
        match xs with
        | [] -> raise Impossible
        | x :: xs -> (x, xs)
      in
      (str, Tok.combine_toks (snd hd) (tl |> List_.map snd))
  | `Jsx_name_name x ->
      let id1, id2 = jsx_namespace_name env x in
      let str = fst id1 ^ ":" ^ fst id2 in
      (str, Tok.combine_toks (snd id1) [ snd id2 ])

let jsx_closing_element (env : env) ((v1, v2, v3) : CST.jsx_closing_element) =
  let v1 =
    token env v1
    (* "</" *)
  in
  let str =
    match v2 with
    | Some x -> Some (jsx_element_name env x)
    | None -> None
  in
  let v3 =
    token env v3
    (* ">" *)
  in
  let t = Tok.combine_toks v1 [ v3 ] in
  (str, t)

let meta_property (env : env) (x : CST.meta_property) =
  match x with
  | `New_DOT_target (v1, v2, v3) ->
      let v1 = (* "new" *) token env v1 in
      let v2 = (* "." *) token env v2 in
      let v3 = (* "target" *) token env v3 in
      (v1, v2, v3)
  | `Import_DOT_meta (v1, v2, v3) ->
      let v1 = (* "import" *) token env v1 in
      let v2 = (* "." *) token env v2 in
      let v3 = (* "meta" *) token env v3 in
      (v1, v2, v3)

let from_clause (env : env) ((v1, v2) : CST.from_clause) : tok * string wrap =
  let v1 =
    token env v1
    (* "from" *)
  in
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
  | `Unique_symb (v1, v2) ->
      let v1 = (* "unique" *) identifier env v1 in
      let _v2 = (* "symbol" *) token env v2 in
      v1

let map_anon_choice_DASH_81d4819 (env : env) (x : CST.anon_choice_DASH_81d4819)
    =
  match x with
  | `DASH tok -> (* "-" *) token env tok
  | `PLUS tok -> (* "+" *) token env tok

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
  | `Obj tok
  | `New tok
  | `Over tok ->
      identifier env tok
  | `Choice_get x -> (
      match x with
      | `Get tok -> identifier env tok (* "get" *)
      | `Set tok -> identifier env tok (* "set" *)
      | `Async tok -> identifier env tok (* "async" *)
      | `Static tok -> identifier env tok (* "static" *)
      | `Export tok -> identifier env tok (* export *)
      | `Let tok -> identifier env tok (* "let" *))

let anon_choice_COMMA_5194cb4 (env : env) (x : CST.anon_choice_COMMA_5194cb4) =
  match x with
  | `COMMA tok -> token env tok (* "," *)
  | `Choice_auto_semi x -> semicolon env x

let export_specifier (env : env) ((v1, v2, v3) : CST.export_specifier) =
  let v1 =
    match v1 with
    | Some x -> Some (type_or_typeof env x)
    | None -> None
  in
  let opt_as_id =
    match v3 with
    | Some (v1, v2) ->
        let _v1 = (* "as" *) token env v1 in
        Some (module_export_name env v2)
    | None -> None
  in
  match v1 with
  | Some _ -> (* TODO: 'type foo', 'typeof foo' *) None
  | None ->
      let expr_id = module_export_name env v2 in
      Some (expr_id, opt_as_id)

let import_identifier (env : env) (x : CST.import_identifier) =
  match x with
  | `Id tok -> (* identifier *) identifier env tok
  | `Type tok ->
      (* I cannot find docs distinguishing `import { x as type } from ...`
         I am just going to treat it as an identifier *)
      (* "type" *)
      identifier env tok

let import_specifier (env : env) ((v1, v2) : CST.import_specifier) :
    a_ident * a_ident option =
  (* What is `import typeof`? I know what `import type` is, but this is weird. *)
  let _v1 =
    match v1 with
    | Some x -> Some (type_or_typeof env x)
    | None -> None
  in
  let v2 =
    match v2 with
    | `Import_id x -> (import_identifier env x, None)
    | `Choice_module_export_name_as_import_id (v1, v2, v3) ->
        let v1 =
          match v1 with
          | `Module_export_name x -> module_export_name env x
          | `Type tok -> (* "type" *) identifier env tok
        in
        let _v2 = (* "as" *) token env v2 in
        let v3 = import_identifier env v3 in
        (v1, Some v3)
  in
  v2

let concat_nested_identifier (idents : a_ident list) : a_ident =
  let str = idents |> List_.map fst |> String.concat "." in
  let tokens = List_.map snd idents in
  let x, xs =
    match tokens with
    | [] -> assert false
    | x :: xs -> (x, xs)
  in
  (str, Tok.combine_toks x xs)

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
  let v1 =
    identifier env v1
    (* identifier *)
  in
  let _v2 =
    token env v2
    (* "=" *)
  in
  let _v3 =
    identifier env v3
    (* "require" *)
  in
  let _v4 =
    token env v4
    (* "(" *)
  in
  let v5 = string_ env v5 in
  let _v6 =
    token env v6
    (* ")" *)
  in
  ModuleAlias (tk, v1, v5)

let literal_type (env : env) (x : CST.literal_type) : expr =
  match x with
  | `Num_ (v1, v2) ->
      let s, t1 =
        match v1 with
        | `DASH tok -> str env tok (* "-" *)
        | `PLUS tok -> str env tok (* "+" *)
      in
      let s2, t2 =
        str env v2
        (* number *)
      in
      (* TODO: float_of_string_opt_also_from_hexoctbin *)
      L (Num (float_of_string_opt (s ^ s2), Tok.combine_toks t1 [ t2 ]))
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
  let _v2 =
    token env v2
    (* "." *)
  in
  let v3 =
    str env v3
    (* identifier *)
  in
  v1 @ [ v3 ]

let namespace_export (env : env) ((v1, v2, v3) : CST.namespace_export) =
  let star = (* "*" *) token env v1 in
  let _v2 = (* "as" *) token env v2 in
  let v3 = module_export_name env v3 in
  (star, v3)

let id_or_reserved_id (env : env)
    (x :
      [ `Id of Tree_sitter_run.Token.t
      | `Choice_decl of CST.reserved_identifier ]) : a_ident =
  match x with
  | `Id tok -> identifier env tok (* identifier *)
  | `Choice_decl x -> reserved_identifier env x

let export_specifiers (env : env)
    ((v1, v2) :
      CST.export_specifier
      * (Tree_sitter_run.Token.t * CST.export_specifier) list) :
    (a_ident * a_ident option) list =
  map_sep_list env v1 v2 export_specifier |> List_.filter_map (fun opt -> opt)

let export_clause (env : env) ((v1, v2, v3, v4) : CST.export_clause) =
  let _open =
    token env v1
    (* "{" *)
  in
  let xs =
    match v2 with
    | Some x -> export_specifiers env x
    | None -> []
  in
  let _trailing_comma =
    match v3 with
    | Some _tok -> (* "," *) ()
    | None -> ()
  in
  let _close =
    token env v4
    (* "}" *)
  in
  xs

let named_imports (env : env) ((v1, v2, v3, v4) : CST.named_imports) =
  let _open =
    token env v1
    (* "{" *)
  in
  let imports =
    match v2 with
    | Some (v1, v2) ->
        let v1 = import_specifier env v1 in
        let v2 =
          List_.map
            (fun (v1, v2) ->
              let _v1 = (* "," *) token env v1 in
              let v2 = import_specifier env v2 in
              v2)
            v2
        in
        v1 :: v2
    | None -> []
  in
  let _trailing_comma =
    match v3 with
    | Some tok -> Some (token env tok) (* "," *)
    | None -> None
  in
  let _close =
    token env v4
    (* "}" *)
  in
  fun (import_tok : tok) (from_path : a_filename) ->
    [ Import (import_tok, imports, from_path) ]

let import_clause (env : env) (x : CST.import_clause) =
  match x with
  | `Name_import x ->
      let _star, id = namespace_import env x in
      fun tok path -> [ ModuleAlias (tok, id, path) ]
  | `Named_imports x -> named_imports env x
  | `Import_id_opt_COMMA_choice_name_import (v1, v2) ->
      let v1 = import_identifier env v1 in
      let v2 =
        match v2 with
        | Some (v1, v2) ->
            let _v1 =
              token env v1
              (* "," *)
            in
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
        let default =
          Import (t, [ ((default_entity, snd v1), Some v1) ], path)
        in
        default :: v2 t path

let rec decorator_member_expression (env : env)
    ((v1, v2, v3) : CST.decorator_member_expression) : a_ident list =
  let v1 = anon_choice_type_id_b8f8ced env v1 in
  let _v2 =
    token env v2
    (* "." *)
  in
  let v3 =
    identifier env v3
    (* identifier *)
  in
  v1 @ [ v3 ]

and anon_choice_type_id_b8f8ced (env : env)
    (x : CST.anon_choice_type_id_b8f8ced) : a_ident list =
  match x with
  | `Id x -> [ identifier env x ]
  | `Deco_member_exp x -> decorator_member_expression env x

let rec parenthesized_expression (env : env)
    ((v1, v2, v3) : CST.parenthesized_expression) =
  let v1 =
    token env v1
    (* "(" *)
  in
  let v2 =
    match v2 with
    | `Exp_opt_type_anno (v1, v2) -> (
        let v1 = expression env v1 in
        match v2 with
        | Some x -> (
            let tok, ty = type_annotation env x in
            match (env.extra, v1) with
            | Pattern, Id ((s, _) as id) when AST_generic.is_metavar_name s ->
                TypedMetavar (id, tok, ty)
            | _ -> Cast (v1, tok, ty))
        | None -> v1)
    | `Seq_exp x -> sequence_expression env x
  in
  let v3 =
    token env v3
    (* ")" *)
  in
  ParenExpr (v1, v2, v3)

and jsx_opening_element (env : env) ((v1, v2, v3) : CST.jsx_opening_element) =
  let v1 =
    token env v1
    (* "<" *)
  in
  let tag_attrs_opt =
    map_anon_opt_choice_jsx_attr_name_rep_jsx_attr__8497dc0 env v2
  in
  let v3 =
    token env v3
    (* ">" *)
  in
  (v1, tag_attrs_opt, v3)

and jsx_self_clos_elem (env : env) ((v1, v2, v3) : CST.jsx_self_closing_element)
    =
  let v1 =
    token env v1
    (* "<" *)
  in
  let v2 = map_anon_opt_choice_jsx_attr_name_rep_jsx_attr__8497dc0 env v2 in
  let v3 =
    token env v3
    (* "/>" *)
  in
  (v1, v2, v3)

and jsx_expression (env : env) ((v1, v2, v3) : CST.jsx_expression) :
    expr option bracket =
  let v1 =
    token env v1
    (* "{" *)
  in
  let v2 =
    match v2 with
    | Some x ->
        Some
          (match x with
          | `Exp x -> expression env x
          | `Seq_exp x -> sequence_expression env x
          | `Spread_elem x ->
              let t, e = spread_element env x in
              Apply (IdSpecial (Spread, t), fb [], fb [ e ]))
    (* abusing { } in XML to just add comments, e.g. { /* lint-ignore */ } *)
    | None -> None
  in
  let v3 =
    token env v3
    (* "}" *)
  in
  (v1, v2, v3)

and anon_choice_jsx_attr_name_b052322 (env : env)
    (x : CST.anon_choice_jsx_attr_name_b052322) =
  match x with
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
        | Some x -> type_arguments env x |> Tok.unbracket
        | None -> []
      in
      id

and map_anon_opt_choice_jsx_attr_name_rep_jsx_attr__8497dc0 (env : env)
    (opt : CST.anon_opt_choice_jsx_attr_name_rep_jsx_attr__8497dc0) =
  match opt with
  | Some (v1, v2) ->
      let v1 = anon_choice_jsx_attr_name_b052322 env v1 in
      let v2 = List_.map (jsx_attribute_ env) v2 in
      Some (v1, v2)
  | None -> None

and jsx_attribute_ (env : env) (x : CST.jsx_attribute_) : xml_attribute =
  match x with
  | `Jsx_attr (v1, v2) ->
      let v1 = jsx_attribute_name env v1 in
      let teq, v2 =
        match v2 with
        | Some (v1, v2) ->
            let v1bis =
              token env v1
              (* "=" *)
            in
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
      raise
        (Parsing_error.Ast_builder_error
           ("jsx_expression_some got a None expr", t1))
  | Some e -> (t1, e, t2)

and jsx_attribute_value (env : env) (x : CST.jsx_attribute_value) =
  match x with
  | `Jsx_str x ->
      let id = jsx_string env x in
      L (String id)
  | `Jsx_exp x ->
      let _, e, _ = jsx_expression_some env x in
      e
  (* an attribute value can be a jsx element? *)
  | `Choice_jsx_elem x ->
      let xml = jsx_element_ env x in
      Xml xml

and jsx_child (env : env) (x : CST.jsx_child) : xml_body =
  match x with
  | `Choice_jsx_text x -> (
      match x with
      | `Jsx_text tok ->
          let s =
            str env tok
            (* pattern [^{}<>]+ *)
          in
          XmlText s
      | `Html_char_ref tok ->
          (* I don't think we should treat this differently than a regular textual case. *)
          let s =
            (* pattern &(#([xX][0-9a-fA-F]{1,6}|[0-9]{1,5})|[A-Za-z]{1,30}); *)
            str env tok
          in
          XmlText s
      | `Choice_jsx_elem x ->
          let xml = jsx_element_ env x in
          XmlXml xml
      | `Jsx_exp x ->
          let x = jsx_expression env x in
          XmlExpr x)
  | `Semg_ellips tok -> XmlText ((* "..." *) str env tok)
  | `Semg_meta tok -> XmlText ((* pattern \$[A-Z_][A-Z_0-9]* *) str env tok)

and jsx_element_ (env : env) (x : CST.jsx_element_) : xml =
  match x with
  | `Jsx_elem (v1, v2, v3) ->
      let t0, tag_attrs_opt, closing = jsx_opening_element env v1 in
      let v2 = List_.map (jsx_child env) v2 in
      let v3 = jsx_closing_element env v3 in
      let xml_kind, xml_attrs =
        match tag_attrs_opt with
        | Some (tag, attrs) -> (XmlClassic (t0, tag, closing, snd v3), attrs)
        | None -> (XmlFragment (t0, closing), [])
      in
      { xml_kind; xml_attrs; xml_body = v2 }
  | `Jsx_self_clos_elem x ->
      let t0, tag_attrs_opt, closing = jsx_self_clos_elem env x in
      let xml_kind, xml_attrs =
        match tag_attrs_opt with
        | Some (tag, attrs) -> (XmlSingleton (t0, tag, closing), attrs)
        (* Why would you do this? This is </> *)
        | None -> (XmlFragment (t0, closing), [])
      in
      { xml_kind; xml_attrs; xml_body = [] }

and pattern (env : env) (x : CST.pattern) : (a_ident, a_pattern) Either.t =
  match x with
  | `Choice_choice_member_exp e -> (
      let lhs = lhs_expression env e in
      match lhs with
      | Id x -> Left x
      | _ -> Right lhs)
  | `Rest_pat (v1, v2) ->
      let tok =
        token env v1
        (* "..." *)
      in
      let _lhs_TODO = lhs_expression env v2 in
      Right (IdSpecial (Spread, tok))

and pair_pattern (env : env) (x : CST.pair_pattern) =
  match x with
  | `Prop_name_COLON_choice_pat (v1, v2, v3) ->
      let v1 = property_name env v1 in
      let _v2 =
        token env v2
        (* ":" *)
      in
      let body = pat_or_assign_pat env v3 |> sub_pattern in
      let ty = None in
      FieldColon
        { fld_name = v1; fld_attrs = []; fld_type = ty; fld_body = Some body }
  | `Semg_ellips tok -> FieldEllipsis (* "..." *) (token env tok)

(*
   This is a pattern for destructuring an object property.
   It could use its own type rather than abusing the 'property' type.
   See notes in ast_js.ml in pfff.
*)
and object_property_pattern (env : env) (x : CST.anon_choice_pair_pat_3ff9cbe) :
    property =
  match x with
  | `Pair_pat x -> pair_pattern env x
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
      let tok =
        token env v2
        (* "=" *)
      in
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
      let v1 =
        token env v1
        (* "{" *)
      in
      let v2 =
        match v2 with
        | Some (v1, v2) ->
            map_sep_list env v1 v2 (fun env x ->
                match x with
                | Some x -> [ object_property_pattern env x ]
                | None -> [])
            |> List_.flatten
        | None -> []
      in
      let v3 =
        token env v3
        (* "}" *)
      in
      Obj (v1, v2, v3)
  | `Array_pat (v1, v2, v3) ->
      let open_ =
        token env v1
        (* "[" *)
      in
      let elements =
        match v2 with
        | Some (v1, v2) ->
            map_sep_list env v1 v2 (fun env x ->
                match x with
                | Some x -> [ pat_or_assign_pat env x |> sub_pattern ]
                | None -> [])
            |> List_.flatten
        | None -> []
      in
      let close =
        token env v3
        (* "]" *)
      in
      Arr (open_, elements, close)

and variable_declaration (env : env)
    ((v1, v2, v3, v4) : CST.variable_declaration) : var list =
  let v1 =
    (Var, token env v1)
    (* "var" *)
  in
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
  let v2 =
    token env v2
    (* "function" *)
  in
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
  let _v1 =
    token env v1
    (* "implements" *)
  in
  let types = map_sep_list env v2 v3 type_ in
  types

and anon_choice_exp_9818c1b (env : env) (x : CST.anon_choice_exp_9818c1b) =
  match x with
  | `Exp x -> expression env x
  | `Spread_elem x ->
      let t, e = spread_element env x in
      Apply (IdSpecial (Spread, t), fb [], fb [ e ])

and switch_default (env : env) ((v1, v2, v3) : CST.switch_default) =
  let v1 =
    token env v1
    (* "default" *)
  in
  let v2 =
    token env v2
    (* ":" *)
  in
  let v3 = List.concat_map (statement env) v3 in
  Default (v1, stmt1 v2 v3)

and asserts_annotation (env : env) ((v1, v2) : CST.asserts_annotation) =
  let _v1 = (* ":" *) token env v1 in
  let v2 = map_asserts env v2 in
  v2

and binary_expression (env : env) (x : CST.binary_expression) : expr =
  match x with
  | `Exp_AMPAMP_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 =
        token env v2
        (* "&&" *)
      in
      let v3 = expression env v3 in
      Apply (IdSpecial (ArithOp G.And, v2), fb [], fb [ v1; v3 ])
  | `Exp_BARBAR_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 =
        token env v2
        (* "||" *)
      in
      let v3 = expression env v3 in
      Apply (IdSpecial (ArithOp G.Or, v2), fb [], fb [ v1; v3 ])
  | `Exp_GTGT_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 =
        token env v2
        (* ">>" *)
      in
      let v3 = expression env v3 in
      Apply (IdSpecial (ArithOp G.LSR, v2), fb [], fb [ v1; v3 ])
  | `Exp_GTGTGT_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 =
        token env v2
        (* ">>>" *)
      in
      let v3 = expression env v3 in
      Apply (IdSpecial (ArithOp G.ASR, v2), fb [], fb [ v1; v3 ])
  | `Exp_LTLT_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 =
        token env v2
        (* "<<" *)
      in
      let v3 = expression env v3 in
      Apply (IdSpecial (ArithOp G.LSL, v2), fb [], fb [ v1; v3 ])
  | `Exp_AMP_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 =
        token env v2
        (* "&" *)
      in
      let v3 = expression env v3 in
      Apply (IdSpecial (ArithOp G.BitAnd, v2), fb [], fb [ v1; v3 ])
  | `Exp_HAT_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 =
        token env v2
        (* "^" *)
      in
      let v3 = expression env v3 in
      Apply (IdSpecial (ArithOp G.BitXor, v2), fb [], fb [ v1; v3 ])
  | `Exp_BAR_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 =
        token env v2
        (* "|" *)
      in
      let v3 = expression env v3 in
      Apply (IdSpecial (ArithOp G.BitOr, v2), fb [], fb [ v1; v3 ])
  | `Exp_PLUS_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 =
        token env v2
        (* "+" *)
      in
      let v3 = expression env v3 in
      Apply (IdSpecial (ArithOp G.Plus, v2), fb [], fb [ v1; v3 ])
  | `Exp_DASH_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 =
        token env v2
        (* "-" *)
      in
      let v3 = expression env v3 in
      Apply (IdSpecial (ArithOp G.Minus, v2), fb [], fb [ v1; v3 ])
  | `Exp_STAR_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 =
        token env v2
        (* "*" *)
      in
      let v3 = expression env v3 in
      Apply (IdSpecial (ArithOp G.Mult, v2), fb [], fb [ v1; v3 ])
  | `Exp_SLASH_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 =
        token env v2
        (* "/" *)
      in
      let v3 = expression env v3 in
      Apply (IdSpecial (ArithOp G.Div, v2), fb [], fb [ v1; v3 ])
  | `Exp_PERC_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 =
        token env v2
        (* "%" *)
      in
      let v3 = expression env v3 in
      Apply (IdSpecial (ArithOp G.Mod, v2), fb [], fb [ v1; v3 ])
  | `Exp_STARSTAR_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 =
        token env v2
        (* "**" *)
      in
      let v3 = expression env v3 in
      Apply (IdSpecial (ArithOp G.Pow, v2), fb [], fb [ v1; v3 ])
  | `Exp_LT_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 =
        token env v2
        (* "<" *)
      in
      let v3 = expression env v3 in
      Apply (IdSpecial (ArithOp G.Lt, v2), fb [], fb [ v1; v3 ])
  | `Exp_LTEQ_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 =
        token env v2
        (* "<=" *)
      in
      let v3 = expression env v3 in
      Apply (IdSpecial (ArithOp G.LtE, v2), fb [], fb [ v1; v3 ])
  | `Exp_EQEQ_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 =
        token env v2
        (* "==" *)
      in
      let v3 = expression env v3 in
      Apply (IdSpecial (ArithOp G.Eq, v2), fb [], fb [ v1; v3 ])
  | `Exp_EQEQEQ_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 =
        token env v2
        (* "===" *)
      in
      let v3 = expression env v3 in
      Apply (IdSpecial (ArithOp G.PhysEq, v2), fb [], fb [ v1; v3 ])
  | `Exp_BANGEQ_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 =
        token env v2
        (* "!=" *)
      in
      let v3 = expression env v3 in
      Apply (IdSpecial (ArithOp G.NotEq, v2), fb [], fb [ v1; v3 ])
  | `Exp_BANGEQEQ_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 =
        token env v2
        (* "!==" *)
      in
      let v3 = expression env v3 in
      Apply (IdSpecial (ArithOp G.NotPhysEq, v2), fb [], fb [ v1; v3 ])
  | `Exp_GTEQ_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 =
        token env v2
        (* ">=" *)
      in
      let v3 = expression env v3 in
      Apply (IdSpecial (ArithOp G.GtE, v2), fb [], fb [ v1; v3 ])
  | `Exp_GT_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 =
        token env v2
        (* ">" *)
      in
      let v3 = expression env v3 in
      Apply (IdSpecial (ArithOp G.Gt, v2), fb [], fb [ v1; v3 ])
  | `Exp_QMARKQMARK_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 =
        token env v2
        (* "??" *)
      in
      let v3 = expression env v3 in
      Apply (IdSpecial (ArithOp G.Nullish, v2), fb [], fb [ v1; v3 ])
  | `Exp_inst_exp (v1, v2, v3) ->
      let v1 = expression env v1 in
      let v2 =
        token env v2
        (* "instanceof" *)
      in
      let v3 = expression env v3 in
      Apply (IdSpecial (Instanceof, v2), fb [], fb [ v1; v3 ])
  | `Choice_exp_in_exp (v1, v2, v3) ->
      let v1 =
        match v1 with
        | `Exp x -> expression env x
        | `Priv_prop_id tok ->
            (* private_property_identifier *) identifier env tok |> idexp
      in
      let v2 = (* "in" *) token env v2 in
      let v3 = expression env v3 in
      Apply (IdSpecial (In, v2), fb [], fb [ v1; v3 ])

and arguments (env : env) ((v1, v2, v3) : CST.arguments) : a_arguments =
  let v1 =
    token env v1
    (* "(" *)
  in
  let v2 = anon_opt_opt_choice_exp_rep_COMMA_opt_choice_exp_208ebb4 env v2 in
  let v3 =
    token env v3
    (* ")" *)
  in
  (v1, v2, v3)

and generator_function_declaration (env : env)
    ((v1, v2, v3, v4, v5, v6, v7) : CST.generator_function_declaration) :
    definition =
  let v1 =
    match v1 with
    | Some tok -> [ attr (Async, token env tok) ] (* "async" *)
    | None -> []
  in
  let v2 =
    token env v2
    (* "function" *)
  in
  let v3 =
    [ attr (Generator, token env v3) ]
    (* "*" *)
  in
  let v4 =
    identifier env v4
    (* identifier *)
  in
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
      let default = initializer_opt env v3 in
      (id_or_pat, type_, default)
  | `Id_BANG_type_anno (v1, v2, v3) ->
      let id_or_pat = Left (identifier env v1 (* identifier *)) in
      (* definite assignment assertion
         TODO: add to AST? *)
      let _v2 =
        token env v2
        (* "!" *)
      in
      let type_ = type_annotation env v3 |> snd in
      (id_or_pat, Some type_, None)

and sequence_expression (env : env) ((v1, v2) : CST.sequence_expression) =
  let v1 = expression env v1 in
  let v2 =
    List_.map
      (fun (v1, v2) ->
        let _v1 = (* "," *) token env v1 in
        let v2 = expression env v2 in
        v2)
      v2
  in
  Apply (IdSpecial (Seq, Tok.unsafe_fake_tok ""), fb [], fb (v1 :: v2))

and type_arguments (env : env) ((v1, v2, v3, v4, v5) : CST.type_arguments) :
    type_ list bracket =
  let v1 =
    token env v1
    (* "<" *)
  in
  let types = map_sep_list env v2 v3 type_ in
  let _v4 =
    match v4 with
    | Some tok -> Some (token env tok) (* "," *)
    | None -> None
  in
  let v5 =
    token env v5
    (* ">" *)
  in
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
  let v1 =
    token env v1
    (* "{" *)
  in
  let rec aux acc_decorators xs =
    match xs with
    | [] -> []
    | x :: xs -> (
        match x with
        | `Semg_ellips tok ->
            let tok = token env tok in
            if acc_decorators <> [] then
              raise
                (Parsing_error.Ast_builder_error
                   ("ellipsis cannot follow decorators", tok))
            else FieldEllipsis tok :: aux [] xs
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
                  let v = abstract_method_signature env x in
                  Some (Field v)
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
  let v3 =
    token env v3
    (* "}" *)
  in
  (v1, v2, v3)

and type_parameter (env : env) ((v1, v2, v3, v4) : CST.type_parameter) :
    a_type_parameter =
  let _v1 =
    match v1 with
    | Some tok -> Some ((* "const" *) token env tok)
    | None -> None
  in
  let v2 =
    str env v2
    (* identifier *)
  in
  let _v3 =
    match v3 with
    | Some x -> Some (constraint_ env x)
    | None -> None
  in
  let _v4 =
    match v4 with
    | Some x -> Some (default_type env x)
    | None -> None
  in
  v2

and member_expression (env : env) ((v1, v2, v3) : CST.member_expression) : expr
    =
  let expr =
    match v1 with
    | `Exp x -> expression env x
    | `Prim_exp x -> primary_expression env x
    (* Again, really weird we just have an `import` keyword here.
       Let's call it an identifier. I can't find any docs on this.
     *)
    | `Import tok ->
        (* import *)
        identifier env tok |> idexp
  in
  let dot =
    match v2 with
    | `DOT tok (* "." *) -> (Dot, token env tok)
    | `Opt_chain (* "?." *) tok -> (QuestDot, token env tok)
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
  let id =
    identifier env id_tok
    (* identifier *)
  in
  ObjAccess (expr, dot, PN id)

and map_anon_choice_import_c99ceb4 (env : env)
    (x : CST.anon_choice_import_c99ceb4) =
  match x with
  | `Import tok -> identifier env tok (* import *) |> idexp
  | `Id tok -> identifier env tok (* identifier *) |> idexp
  | `Type_query_member_exp x -> map_type_query_member_expression env x
  | `Type_query_subs_exp x -> map_type_query_subscript_expression env x

and pair (env : env) (x : CST.pair) =
  match x with
  | `Prop_name_COLON_exp (v1, v2, v3) ->
      let v1 = property_name env v1 in
      let _v2 =
        token env v2
        (* ":" *)
      in
      let v3 = expression env v3 in
      FieldColon
        { fld_name = v1; fld_attrs = []; fld_type = None; fld_body = Some v3 }
  | `Semg_ellips tok -> FieldEllipsis (* "..." *) (token env tok)

and object_property (env : env) (x : CST.anon_choice_pair_20c9acd) : property =
  match x with
  | `Pair x -> pair env x
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
  let v3 =
    token env v3
    (* "[" *)
  in
  let v4 = expressions env v4 in
  let v5 =
    token env v5
    (* "]" *)
  in
  (* TODO: distinguish optional chaining "?." from a simple access "." *)
  ArrAccess (expr, (v3, v4, v5))

and initializer_ (env : env) ((v1, v2) : CST.initializer_) =
  let _v1 =
    token env v1
    (* "=" *)
  in
  let v2 = expression env v2 in
  v2

and initializer_opt env v =
  match v with
  | Some x -> Some (initializer_ env x)
  | None -> None

and primary_expression (env : env) (x : CST.primary_expression) : expr =
  match x with
  | `Semg_exp_ellips tok ->
      let tok = token env tok in
      Ellipsis tok
  | `Choice_choice_subs_exp x -> (
      match x with
      | `Choice_subs_exp x -> (
          match x with
          | `Subs_exp x -> subscript_expression env x
          | `Member_exp x -> member_expression env x
          | `Paren_exp x -> parenthesized_expression env x
          | `Choice_unde x -> identifier_ env x
          | `Choice_decl x -> reserved_identifier env x |> idexp_or_special
          | `This tok -> this env tok (* "this" *)
          | `Super tok -> super env tok (* "super" *)
          | `Num tok ->
              let n =
                number env tok
                (* number *)
              in
              L (Num n)
          | `Str x ->
              let s = string_ env x in
              L (String s)
          | `Temp_str x ->
              let t1, xs, t2 = template_string env x in
              Apply (IdSpecial (Encaps false, t1), fb [], (t1, xs, t2))
          | `Regex (v1, v2, v3, v4) ->
              let v1 =
                token env v1
                (* "/" *)
              in
              let v2 =
                str env v2
                (* regex_pattern *)
              in
              let v3 =
                token env v3
                (* "/" *)
              in
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
          | `Func_exp x ->
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
                    (fb [ ParamClassic (mk_param id) ], None)
                | `Call_sign x ->
                    let _tparams, (params, tret) = call_signature env x in
                    (params, tret)
              in
              let v3 =
                token env v3
                (* "=>" *)
              in
              let v4 =
                match v4 with
                | `Exp x ->
                    let e = expression env x in
                    Return (v3, Some e, Tok.sc v3)
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
              let v2 =
                token env v2
                (* "function" *)
              in
              let v3 =
                [ (Generator, token env v3) ]
                (* "*" *)
              in
              let v4 =
                match v4 with
                | Some tok -> Some (identifier env tok) (* identifier *)
                | None -> None
              in
              let _tparams, (v5, tret) = call_signature env v5 in
              let v6 = statement_block env v6 in
              let attrs = v1 @ v3 |> List_.map attr in
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
              let v1 = List_.map (decorator env) v1 in
              let v2 =
                token env v2
                (* "class" *)
              in
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
          | `Meta_prop x ->
              let v1, v2, v3 = meta_property env x in
              let t = Tok.combine_toks v1 [ v2; v3 ] in
              IdSpecial (NewTarget, t)
          | `Call_exp x -> call_expression env x)
      | `Non_null_exp x -> non_null_expression env x)

and call_expression (env : env) (x : CST.call_expression) =
  match x with
  | `Choice_exp_opt_type_args_args (v1, v2, v3) ->
      let v1 =
        match v1 with
        | `Exp x -> expression env x
        | `Import tok -> (* import *) identifier env tok |> idexp
      in
      (* TODO: types *)
      let v2 =
        match v2 with
        | Some x -> type_arguments env x |> Tok.unbracket
        | None -> []
      in
      let args = arguments env v3 in
      Apply (v1, fb v2, args)
  | `Choice_prim_exp_temp_str (v1, v2) ->
      let v1 =
        match v1 with
        | `Prim_exp x -> primary_expression env x
        | `New_exp x -> new_expression env x
      in
      let t1, xs, t2 = template_string env v2 in
      Apply (IdSpecial (Encaps true, t1), fb [], (t1, v1 :: xs, t2))
  | `Prim_exp_QMARKDOT_opt_type_args_args (v1, v2, v3, v4) ->
      let v1 = primary_expression env v1 in
      let _v2 =
        token env v2
        (* "?." *)
      in
      (* TODO: types *)
      let _v3TODO =
        match v3 with
        | Some x -> type_arguments env x |> Tok.unbracket
        | None -> []
      in
      let v4 = arguments env v4 in
      (* TODO: distinguish "?." from a simple application *)
      Apply (v1, fb [], v4)

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

and new_expression (env : env) ((v1, v2, v3, v4) : CST.new_expression) =
  let v1 = (* "new" *) token env v1 in
  let v2 = primary_expression env v2 in
  let _v3_TODO =
    match v3 with
    | Some x -> type_arguments env x |> Tok.unbracket
    | None -> []
  in
  let v4 =
    match v4 with
    | Some x -> arguments env x
    | None -> fb []
  in
  New (v1, v2, v4)

and non_null_expression (env : env) ((v1, v2) : CST.non_null_expression) =
  let v1 = expression env v1 in
  let v2 =
    token env v2
    (* "!" *)
  in
  let special = (ArithOp G.NotNullPostfix, v2) in
  Apply (IdSpecial special, fb [], fb [ v1 ])

and expression_statement (env : env) ((v1, v2) : CST.expression_statement) =
  let v1 = expressions env v1 in
  let v2 = semicolon env v2 in
  (v1, v2)

and catch_clause (env : env) ((v1, v2, v3) : CST.catch_clause) : catch =
  let catch_tok =
    token env v1
    (* "catch" *)
  in
  let stmts = statement_block env v3 in
  let catch =
    match v2 with
    | Some (v1, v2, v3, v4) ->
        let _open =
          token env v1
          (* "(" *)
        in
        let id_or_pat = id_or_destructuring_pattern env v2 in
        let type_ =
          match v3 with
          | Some x -> Some (type_annotation env x)
          | None -> None
        in
        let _close =
          token env v4
          (* ")" *)
        in
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
          List_.map
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
  let v1 =
    token env v1
    (* "`" *)
  in
  let v2 =
    List_.map
      (fun x ->
        match x with
        | `Temp_chars tok -> L (String (str env tok)) (* template_chars *)
        | `Esc_seq tok -> L (String (str env tok)) (* escape_sequence *)
        | `Temp_subs x -> template_substitution env x)
      v2
  in
  let v3 =
    token env v3
    (* "`" *)
  in
  (v1, v2, v3)

and template_substitution (env : env) ((v1, v2, v3) : CST.template_substitution)
    : expr =
  let _v1 =
    token env v1
    (* "${" *)
  in
  let v2 = expressions env v2 in
  let _v3 =
    token env v3
    (* "}" *)
  in
  v2

and map_template_literal_type (env : env)
    ((v1, v2, v3) : CST.template_literal_type) : type_ =
  let lback = (* "`" *) token env v1 in
  let xs =
    List_.map
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
  TypeTodo (("TemplateLitType", lback), xs |> List_.map (fun x -> Type x))

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
  let v1 =
    token env v1
    (* "@" *)
  in
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
    | `Deco_paren_exp x ->
        let ids, args = decorator_parenthesized_expression env x in
        (ids, args)
  in
  NamedAttr (v1, ids, args_opt)

and internal_module (env : env) ((v1, v2) : CST.internal_module) =
  let _v1 =
    token env v1
    (* "namespace" *)
  in
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
  let _open =
    token env v1
    (* "(" *)
  in
  let var_or_expr =
    match v2 with
    | `Choice_choice_choice_member_exp x ->
        let expr = paren_expr_or_lhs_expr env x in
        Right expr
    | `Var_choice_id_opt_init (v1, v2, v3) ->
        let var_kind = (Var, token env v1) in
        let id_or_pat = id_or_destructuring_pattern env v2 in
        let pat = id_or_pat |> sub_pattern in
        let rhs = initializer_opt env v3 in
        let var = Ast_js.var_pattern_to_var var_kind pat (snd var_kind) rhs in
        Left var
    | `Choice_let_choice_id_opt_auto_semi (v1, v2, v3) ->
        let var_kind =
          match v1 with
          | `Let tok -> (Let, token env tok) (* "let" *)
          | `Const tok -> (Const, token env tok)
          (* "const" *)
        in
        let id_or_pat = id_or_destructuring_pattern env v2 in
        let _v3 =
          match v3 with
          | Some tok -> Some ((* automatic_semicolon *) token env tok)
          | None -> None
        in
        let pat = id_or_pat |> sub_pattern in
        let var = Ast_js.var_pattern_to_var var_kind pat (snd var_kind) None in
        Left var
  in

  let exprs = expressions env v4 in
  let for_header =
    match v3 with
    | `In tok -> (* "in" *) ForIn (var_or_expr, token env tok, exprs)
    | `Of tok -> (* "of" *) ForOf (var_or_expr, token env tok, exprs)
  in
  let _close =
    token env v5
    (* ")" *)
  in
  for_header

and expr_or_prim_expr (env : env) (x : CST.anon_choice_exp_9cd0ed5) : expr =
  match x with
  | `Exp x -> expression env x
  | `Prim_exp x -> primary_expression env x

and expression (env : env) (x : CST.expression) : expr =
  match x with
  | `As_exp (v1, v2, v3) -> (
      (* type assertion of the form 'exp as type' *)
      let e = expression env v1 in
      let tas =
        token env v2
        (* "as" *)
      in
      match v3 with
      | `Type x ->
          let ty = type_ env x in
          TypeAssert (e, tas, ty)
      | `Const _tok -> e)
  (* https://2ality.com/2025/02/satisfies-operator.html *)
  | `Satiss_exp (v1, v2, v3) ->
      let e = expression env v1 in
      let v2 = (* "satisfies" *) token env v2 in
      let ty = type_ env v3 in
      Satisfies (e, v2, ty)
  (* https://blog.ohansemmanuel.com/the-new-instantiation-expression-in-typescript/ *)
  | `Inst_exp (v1, v2) ->
      let e = expression env v1 in
      let ty = type_arguments env v2 in
      Instantiation (e, ty)
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
              f_params = fb [];
              f_body = body;
              f_rettype = None;
              f_kind = (G.Function, fake);
            }
          in
          Apply (Fun (fun_, Some name), fb [], fb [])
      | None -> idexp name)
  | `Type_asse (v1, v2) -> (
      (* type assertion of the form <string>someValue *)
      let t1, xs, _ = type_arguments env v1 in
      let v2 = expression env v2 in
      match xs with
      | [ t ] -> TypeAssert (v2, t1, t)
      | _ ->
          raise (Parsing_error.Ast_builder_error ("wrong type assert expr", t1))
      )
  | `Prim_exp x -> primary_expression env x
  | `Choice_jsx_elem x ->
      let xml = jsx_element_ env x in
      Xml xml
  | `Assign_exp (v1, v2, v3, v4) ->
      (* TODO: `using`
         https://www.totaltypescript.com/typescript-5-2-new-keyword-using
       *)
      let _v1 =
        match v1 with
        | Some tok -> Some ((* "using" *) token env tok)
        | None -> None
      in
      let v2 = paren_expr_or_lhs_expr env v2 in
      let v3 =
        token env v3
        (* "=" *)
      in
      let v4 = expression env v4 in
      Assign (v2, v3, v4)
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
                let id =
                  identifier env tok
                  (* identifier *)
                in
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
        Apply
          ( IdSpecial (ArithOp op, tok),
            fb [],
            fb [ lhs; Assign (lhs, tok, rhs) ] )
      else
        Assign
          (lhs, tok, Apply (IdSpecial (ArithOp op, tok), fb [], fb [ lhs; rhs ]))
  | `Await_exp (v1, v2) ->
      let v1 =
        token env v1
        (* "await" *)
      in
      let v2 = expression env v2 in
      Apply (IdSpecial (Await, v1), fb [], fb [ v2 ])
  | `Un_exp x -> unary_expression env x
  | `Bin_exp x -> binary_expression env x
  | `Tern_exp (v1, v2, v3, v4, v5) ->
      let v1 = expression env v1 in
      let tquestion =
        token env v2
        (* "?" *)
      in
      let v3 = expression env v3 in
      let tcolon =
        token env v4
        (* ":" *)
      in
      let v5 = expression env v5 in
      Conditional (v1, tquestion, v3, tcolon, v5)
  | `Update_exp x -> update_expression env x
  | `New_exp x -> new_expression env x
  | `Yield_exp (v1, v2) ->
      let v1 =
        token env v1
        (* "yield" *)
      in
      let v2 =
        match v2 with
        | `STAR_exp (v1bis, v2) ->
            let _v1bis =
              token env v1bis
              (* "*" *)
            in
            let v2 = expression env v2 in
            Apply (IdSpecial (YieldStar, v1), fb [], fb [ v2 ])
        | `Opt_exp opt -> (
            match opt with
            | Some x ->
                let x = expression env x in
                Apply (IdSpecial (Yield, v1), fb [], fb [ x ])
            | None -> Apply (IdSpecial (Yield, v1), fb [], fb []))
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
  | `Union_type (v1, v2, v3) -> (
      let v2 =
        token env v2
        (* "|" *)
      in
      let v3 = type_ env v3 in
      match v1 with
      | Some x ->
          let x = type_ env x in
          TyOr (x, v2, v3)
      | None -> v3
      (* ?? *))
  | `Inte_type (v1, v2, v3) -> (
      let v2 =
        token env v2
        (* "&" *)
      in
      let v3 = type_ env v3 in
      match v1 with
      | Some x ->
          let x = type_ env x in
          TyAnd (x, v2, v3)
      | None -> v3
      (* ?? *))
  | `Temp_lit_type x -> map_template_literal_type env x
  | `Paren_type (v1, v2, v3) ->
      let _v1 =
        token env v1
        (* "(" *)
      in
      let v2 = type_ env v2 in
      let _v3 =
        token env v3
        (* ")" *)
      in
      v2
  | `Pred_type x ->
      let id = predefined_type env x in
      (* less: could also be a G.TyBuiltin *)
      TyName [ id ]
  | `Id tok ->
      let id =
        identifier env tok
        (* identifier *)
      in
      TyName [ id ]
  | `Nested_type_id x ->
      let xs = nested_type_identifier env x in
      TyName xs
  | `Gene_type x -> TyName (generic_type env x)
  | `Obj_type x ->
      let t1, xs, t2 = object_type env x in
      let xs =
        xs
        |> List_.filter_map (function
             (* TODO *)
             | Left prop -> Some prop
             | Right _sts -> None)
      in
      TyRecordAnon (t1, xs, t2)
  | `Array_type (v1, v2, v3) ->
      let type_ = primary_type env v1 in
      let open_ =
        token env v2
        (* "[" *)
      in
      let close =
        token env v3
        (* "]" *)
      in
      TyArray (type_, (open_, (), close))
  | `Tuple_type (v1, v2, v3, v4) ->
      let open_ =
        token env v1
        (* "[" *)
      in
      let members =
        match v2 with
        | None -> []
        | Some (v1, v2) -> map_sep_list env v1 v2 tuple_type_member
      in
      let _trailing_comma =
        optional env v3 token
        (* "," *)
      in
      let close =
        token env v4
        (* "]" *)
      in
      TyTuple (open_, members, close)
  | `Flow_maybe_type (v1, v2) ->
      let v1 =
        token env v1
        (* "?" *)
      in
      let v2 = primary_type env v2 in
      TyQuestion (v1, v2)
  | `Type_query x -> type_query env x
  | `Index_type_query (v1, v2) ->
      let keyof =
        token env v1
        (* "keyof" *)
      in
      let type_ = primary_type env v2 in
      TypeTodo (("KeyOf", keyof), [ Type type_ ])
  | `This tok ->
      let v1 = token env tok in
      (* "this" *)
      TypeTodo (("This", v1), [])
  | `Exis_type tok ->
      let v1 =
        token env tok
        (* "*" *)
      in
      TypeTodo (("*", v1), [])
  | `Lit_type x ->
      let v1 = literal_type env x in
      TypeTodo (("LitType", fake), [ Expr v1 ])
  | `Lookup_type (v1, v2, v3, v4) ->
      let v1 = primary_type env v1 in
      let v2 =
        token env v2
        (* "[" *)
      in
      let v3 = type_ env v3 in
      let _v4 =
        token env v4
        (* "]" *)
      in
      TypeTodo (("LookupType", v2), [ Type v1; Type v3 ])
  | `Cond_type (v1, v2, v3, v4, v5, v6, v7) ->
      let v1 = type_ env v1 in
      let _v2 =
        token env v2
        (* "extends" *)
      in
      let v3 = type_ env v3 in
      let v4 =
        token env v4
        (* "?" *)
      in
      let v5 = type_ env v5 in
      let _v6 =
        token env v6
        (* ":" *)
      in
      let v7 = type_ env v7 in
      TypeTodo (("ConditionalType", v4), [ Type v1; Type v3; Type v5; Type v7 ])
  | `Const tok ->
      let t = (* "const" *) token env tok in
      TypeTodo (("Const", t), [])

and import_attribute (env : env) ((v1, v2) : CST.import_attribute) =
  let v1 =
    match v1 with
    | `With tok -> (* "with" *) token env tok
    | `Assert tok -> (* "assert" *) token env tok
  in
  let v2 = object_ env v2 in
  (v1, v2)

and index_signature (env : env) ((v1, v2, v3, v4, v5) : CST.index_signature) =
  let _v1 =
    match v1 with
    | Some (v1, v2) ->
        let v1 =
          match v1 with
          | Some x -> Some (map_anon_choice_DASH_81d4819 env x)
          | None -> None
        in
        let v2 =
          token env v2
          (* "readonly" *)
        in
        Some (v1, v2)
        (* TODO add to AST *)
    | None -> None
  in
  let v2 =
    token env v2
    (* "[" *)
  in
  let v3 =
    match v3 with
    | `Choice_id_COLON_type (v2, v3, v4) ->
        let v2 = id_or_reserved_id env v2 in
        let v3 =
          token env v3
          (* ":" *)
        in
        let v4 = type_ env v4 in
        TypeTodo (("IndexKey", v3), [ Type (TyName [ v2 ]); Type v4 ])
    | `Mapped_type_clause x -> mapped_type_clause env x
  in
  let _v4 =
    token env v4
    (* "]" *)
  in
  let v5 =
    match v5 with
    | `Type_anno x -> type_annotation env x |> snd
    | `Omit_type_anno (v1, v2) ->
        let _v1_TODO =
          token env v1
          (* "-?:" *)
        in
        let v2 = type_ env v2 in
        v2
    | `Adding_type_anno (v1, v2) ->
        let _v1 = (* "+?:" *) token env v1 in
        let v2 = type_ env v2 in
        v2
    | `Opting_type_anno (v1, v2) ->
        let _v1_TODO =
          token env v1
          (* "?:" *)
        in
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
    | `Type_query_inst_exp x -> map_type_query_instantiation_expression env x
    | `Id tok ->
        let id = (* identifier *) str env tok in
        Id id
    | `This tok ->
        let tthis = (* "this" *) token env tok in
        IdSpecial (This, tthis)
  in
  TypeTodo (("Typeof", ttypeof), [ Expr e ])

and map_anon_choice_type_id_e96bf13 (env : env)
    (x : CST.anon_choice_type_id_e96bf13) : expr =
  match x with
  | `Id tok ->
      let id = (* identifier *) str env tok in
      idexp_or_special id
  | `This tok ->
      let tthis = (* "this" *) token env tok in
      IdSpecial (This, tthis)
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
  Apply (e, fb [], args)

and map_type_query_call_expression_in_type_annotation (env : env)
    ((v1, v2) : CST.type_query_call_expression_in_type_annotation) =
  let v1 =
    match v1 with
    | `Import tok -> (* import *) identifier env tok |> idexp
    | `Type_query_member_exp_in_type_anno x ->
        map_type_query_member_expression_in_type_annotation env x
  in
  let v2 = arguments env v2 in
  Apply (v1, fb [], v2)

and map_type_query_member_expression_in_type_annotation (env : env)
    ((v1, v2, v3) : CST.type_query_member_expression_in_type_annotation) : expr
    =
  let v1 =
    match v1 with
    (* I'm pretty sure that this is wrong. The listed example in `tree-sitter-typescript` involves
       `import('x').y.z;`
       which is fine, but this case is about a member expression. Allowing a token `import` here
       would mean an expression like `import.x.y`, which doesn't make a lot of sense.
       We're gonna just consider this `import` as an identifier.
     *)
    | `Import tok ->
        (* import *)
        identifier env tok |> idexp
    | `Type_query_member_exp_in_type_anno x ->
        map_type_query_member_expression_in_type_annotation env x
    | `Type_query_call_exp_in_type_anno x ->
        map_type_query_call_expression_in_type_annotation env x
  in
  let v2 = (* "." *) token env v2 in
  let v3 = map_anon_choice_priv_prop_id_89abb74 env v3 in
  ObjAccess (v1, (Dot, v2), PN v3)

and map_type_query_instantiation_expression (env : env)
    ((v1, v2) : CST.type_query_instantiation_expression) : expr =
  let v1 = map_anon_choice_import_c99ceb4 env v1 in
  let l, v2, r = type_arguments env v2 in
  Instantiation (v1, (l, v2, r))

and map_type_query_member_expression (env : env)
    ((v1, v2, v3) : CST.type_query_member_expression) : expr =
  let e = map_anon_choice_type_id_e96bf13 env v1 in
  let tdot = map_anon_choice_DOT_d88d0af env v2 in
  let fld = map_anon_choice_priv_prop_id_89abb74 env v3 in
  ObjAccess (e, (Dot, tdot), PN fld)

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

and unary_expression (env : env) ((v1, v2) : CST.unary_expression) =
  let v2 = expression env v2 in
  match v1 with
  | `BANG v1 ->
      let v1 =
        token env v1
        (* "!" *)
      in
      Apply (IdSpecial (ArithOp G.Not, v1), fb [], fb [ v2 ])
  | `TILDE v1 ->
      let v1 =
        token env v1
        (* "~" *)
      in
      Apply (IdSpecial (ArithOp G.BitNot, v1), fb [], fb [ v2 ])
  | `DASH v1 ->
      let v1 =
        token env v1
        (* "-" *)
      in
      Apply (IdSpecial (ArithOp G.Minus, v1), fb [], fb [ v2 ])
  | `PLUS v1 ->
      let v1 =
        token env v1
        (* "+" *)
      in
      Apply (IdSpecial (ArithOp G.Plus, v1), fb [], fb [ v2 ])
  | `Typeof v1 ->
      let v1 =
        token env v1
        (* "typeof" *)
      in
      Apply (IdSpecial (Typeof, v1), fb [], fb [ v2 ])
  | `Void v1 ->
      let v1 =
        token env v1
        (* "void" *)
      in
      Apply (IdSpecial (Void, v1), fb [], fb [ v2 ])
  | `Delete v1 ->
      let v1 =
        token env v1
        (* "delete" *)
      in
      Apply (IdSpecial (Delete, v1), fb [], fb [ v2 ])

and pat_or_assign_pat (env : env) (x : CST.anon_choice_pat_3297d92) :
    (a_ident, a_pattern) Either.t =
  match x with
  | `Pat x -> pattern env x
  | `Assign_pat (v1, v2, v3) ->
      let pat = pattern env v1 in
      let _eq =
        token env v2
        (* "=" *)
      in
      let _default = expression env v3 in
      pat

and formal_parameter (env : env) (x : CST.formal_parameter) : parameter =
  let parameter_of_id_or_pat id_or_pat opt_type opt_default =
    match id_or_pat with
    | Left (id, attrs) ->
        ParamClassic
          {
            p_name = id;
            p_default = opt_default;
            p_dots = None;
            p_type = opt_type;
            p_attrs = attrs;
          }
    | Right pat ->
        let pat =
          match opt_type with
          | None -> pat
          | Some type_ -> Cast (pat, Tok.unsafe_fake_tok ":", type_)
        in
        let pat =
          match opt_default with
          | None -> pat
          | Some expr -> Assign (pat, Tok.unsafe_fake_tok "=", expr)
        in
        ParamPattern pat
  in

  match x with
  | `Requ_param (v1, v2, v3) ->
      (* required_parameter *)
      let id_or_pat = parameter_name env v1 in
      let type_ =
        match v2 with
        | Some x -> Some (type_annotation env x |> snd)
        | None -> None
      in
      let default = initializer_opt env v3 in
      parameter_of_id_or_pat id_or_pat type_ default
  | `Opt_param (v1, v2, v3, v4) ->
      (* optional_parameter *)
      let id_or_pat = parameter_name env v1 in
      let _questionmark_TODO =
        token env v2
        (* "?" *)
      in
      let opt_type =
        match v3 with
        | Some x -> Some (type_annotation env x |> snd)
        | None -> None
      in
      let opt_default = initializer_opt env v4 in
      parameter_of_id_or_pat id_or_pat opt_type opt_default
  | `Semg_ellips v1 ->
      let tok = token env v1 in
      ParamEllipsis tok

and formal_parameters (env : env) ((v1, v2, v3) : CST.formal_parameters) :
    parameter list bracket =
  let open_ =
    token env v1
    (* "(" *)
  in
  let params =
    match v2 with
    | Some (v1, v2, v3) ->
        let params = map_sep_list env v1 v2 formal_parameter in
        let _trailing_comma =
          optional env v3 token
          (* "," *)
        in
        params
    | None -> []
  in
  let close =
    token env v3
    (* ")" *)
  in
  (open_, params, close)

and decorator_parenthesized_expression (env : env)
    ((v1, v2, v3) : CST.decorator_parenthesized_expression) =
  let _v1 = (* "(" *) token env v1 in
  let v2 =
    match v2 with
    | `Id tok ->
        let id = identifier env tok in
        ([ id ], None)
    | `Deco_member_exp x ->
        let ids = decorator_member_expression env x in
        (ids, None)
    | `Deco_call_exp x ->
        let ids, args = decorator_call_expression env x in
        (ids, Some args)
  in
  let _v3 = (* ")" *) token env v3 in
  v2

(* class Component<Props = any, State = any> { ... *)
and default_type (env : env) ((v1, v2) : CST.default_type) =
  let _v1 =
    token env v1
    (* "=" *)
  in
  let v2 = type_ env v2 in
  v2

and switch_body (env : env) ((v1, v2, v3) : CST.switch_body) =
  let _v1 =
    token env v1
    (* "{" *)
  in
  let v2 =
    List_.map
      (fun x ->
        match x with
        | `Switch_case x -> switch_case env x
        | `Switch_defa x -> switch_default env x)
      v2
  in
  let _v3 =
    token env v3
    (* "}" *)
  in
  v2

and mapped_type_clause (env : env) ((v1, v2, v3, v4) : CST.mapped_type_clause) =
  let id =
    str env v1
    (* identifier *)
  in
  let tin =
    token env v2
    (* "in" *)
  in
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
  | `Import_stmt (v1, v2, v3, v4, v5) ->
      let v1 =
        token env v1
        (* "import" *)
      in
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
      let _v4 =
        match v4 with
        | Some x -> Some (import_attribute env x)
        | None -> None
      in
      let _v5 = semicolon env v5 in
      v3 |> List_.map (fun m -> M m)
  | `Debu_stmt (v1, v2) ->
      let v1 =
        identifier env v1
        (* "debugger" *)
      in
      let v2 = semicolon env v2 in
      [ ExprStmt (idexp v1, v2) ]
  | `Exp_stmt x ->
      let e, t = expression_statement env x in
      [ ExprStmt (e, t) ]
  | `Decl x ->
      let vars = declaration env x in
      vars |> List_.map (fun x -> DefStmt x)
  | `Stmt_blk x -> [ statement_block env x ]
  | `If_stmt (v1, v2, v3, v4) ->
      let v1 =
        token env v1
        (* "if" *)
      in
      let v2 = parenthesized_expression env v2 in
      let v3 = statement1 env v3 in
      let v4 =
        match v4 with
        | Some (v1, v2) ->
            let _v1 =
              token env v1
              (* "else" *)
            in
            let v2 = statement1 env v2 in
            Some v2
        | None -> None
      in
      [ If (v1, v2, v3, v4) ]
  | `Switch_stmt (v1, v2, v3) ->
      let v1 =
        token env v1
        (* "switch" *)
      in
      let v2 = parenthesized_expression env v2 in
      let v3 = switch_body env v3 in
      [ Switch (v1, v2, v3) ]
  | `For_stmt (v1, v2, v3, v4, v5, v6, v7) ->
      let v1 =
        token env v1
        (* "for" *)
      in
      let _v2 =
        token env v2
        (* "(" *)
      in
      let v3 =
        match v3 with
        | `Choice_lexi_decl x -> (
            match x with
            | `Lexi_decl x ->
                let vars = lexical_declaration env x in
                Left vars
            | `Var_decl x ->
                let vars = variable_declaration env x in
                Left vars)
        | `Choice_exp_SEMI (v1, v2) ->
            let e = expressions env v1 in
            let _v2 = (* ";" *) token env v2 in
            Right e
        (* | `Exp_stmt x ->
            let e, _t = expression_statement env x in
            Right e *)
        | `Empty_stmt tok ->
            let _x =
              token env tok
              (* ";" *)
            in
            Left []
      in
      let v4 =
        match v4 with
        | `Choice_exp_SEMI (v1, v2) ->
            let v1 = expressions env v1 in
            let _v2 = (* ";" *) token env v2 in
            Some v1
        | `Empty_stmt tok ->
            let _x =
              token env tok
              (* ";" *)
            in
            None
      in
      let v5 =
        match v5 with
        | Some x -> Some (expressions env x)
        | None -> None
      in
      let _v6 =
        token env v6
        (* ")" *)
      in
      let v7 = statement1 env v7 in
      [ For (v1, ForClassic (v3, v4, v5), v7) ]
  | `For_in_stmt (v1, v2, v3, v4) ->
      let v1 =
        token env v1
        (* "for" *)
      in
      let _v2TODO =
        match v2 with
        | Some tok -> Some (token env tok) (* "await" *)
        | None -> None
      in
      let v3 = for_header env v3 in
      let v4 = statement1 env v4 in
      [ For (v1, v3, v4) ]
  | `While_stmt (v1, v2, v3) ->
      let v1 =
        token env v1
        (* "while" *)
      in
      let v2 = parenthesized_expression env v2 in
      let v3 = statement1 env v3 in
      [ While (v1, v2, v3) ]
  | `Do_stmt (v1, v2, v3, v4, v5) ->
      let v1 =
        token env v1
        (* "do" *)
      in
      let v2 = statement1 env v2 in
      let _v3 =
        token env v3
        (* "while" *)
      in
      let v4 = parenthesized_expression env v4 in
      let _v5 = Option.map (semicolon env) v5 in
      [ Do (v1, v2, v4) ]
  | `Try_stmt (v1, v2, v3, v4) ->
      let v1 =
        token env v1
        (* "try" *)
      in
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
      let v1 =
        token env v1
        (* "with" *)
      in
      let v2 = parenthesized_expression env v2 in
      let v3 = statement1 env v3 in
      [ With (v1, v2, v3) ]
  | `Brk_stmt (v1, v2, v3) ->
      let v1 =
        token env v1
        (* "break" *)
      in
      let v2 =
        match v2 with
        | Some tok -> Some (identifier env tok) (* identifier *)
        | None -> None
      in
      let v3 = semicolon env v3 in
      [ Break (v1, v2, v3) ]
  | `Cont_stmt (v1, v2, v3) ->
      let v1 =
        token env v1
        (* "continue" *)
      in
      let v2 =
        match v2 with
        | Some tok -> Some (identifier env tok) (* identifier *)
        | None -> None
      in
      let v3 = semicolon env v3 in
      [ Continue (v1, v2, v3) ]
  | `Ret_stmt (v1, v2, v3) ->
      let v1 =
        token env v1
        (* "return" *)
      in
      let v2 =
        match v2 with
        | Some x -> Some (expressions env x)
        | None -> None
      in
      let v3 = semicolon env v3 in
      [ Return (v1, v2, v3) ]
  | `Throw_stmt (v1, v2, v3) ->
      let v1 =
        token env v1
        (* "throw" *)
      in
      let v2 = expressions env v2 in
      let v3 = semicolon env v3 in
      [ Throw (v1, v2, v3) ]
  | `Empty_stmt tok -> [ empty_stmt env tok (* ";" *) ]
  | `Labe_stmt (v1, v2, v3) ->
      let v1 = id_or_reserved_id env v1 in
      let _v2 =
        token env v2
        (* ":" *)
      in
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
  let attrs = v1 @ v2 @ v2bis @ v3 @ v4 @ v5 @ v7 |> List_.map attr in
  let f_kind = (G.Method, fake) in
  let f =
    { f_attrs = []; f_params = v8; f_body = v9; f_rettype = tret; f_kind }
  in
  let e = Fun (f, None) in
  let ty = None in
  Field { fld_name = v6; fld_attrs = attrs; fld_type = ty; fld_body = Some e }

and class_declaration (env : env)
    ((v1, v2, v3, v4, v5, v6, v7) : CST.class_declaration) : definition =
  let v1 = List_.map (decorator env) v1 in
  let v2 =
    token env v2
    (* "class" *)
  in
  let v3 =
    identifier env v3
    (* identifier *)
  in
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
  let v1 =
    token env v1
    (* "[" *)
  in
  let v2 = anon_opt_opt_choice_exp_rep_COMMA_opt_choice_exp_208ebb4 env v2 in
  let v3 =
    token env v3
    (* "]" *)
  in
  Arr (v1, v2, v3)

and export_statement (env : env) (x : CST.export_statement) : stmt list =
  match x with
  | `Choice_export_choice_STAR_from_clause_choice_auto_semi x -> (
      match x with
      | `Export_choice_STAR_from_clause_choice_auto_semi (v1, v2, v3) ->
          let export_tok =
            token env v1
            (* "export" *)
          in
          let v2 =
            match v2 with
            | `STAR_from_clause (v1, v2) ->
                (* export * from 'foo'; *)
                let star =
                  token env v1
                  (* "*" *)
                in
                let from, path = from_clause env v2 in
                [ M (ReExportNamespace (export_tok, star, None, from, path)) ]
            | `Name_export_from_clause (v1, v2) ->
                (* export * as foo from "module"; *)
                let star, alias =
                  namespace_export env v1
                  (* * as foo *)
                in
                let from, path =
                  from_clause env v2
                  (* from "module" *)
                in
                [
                  M
                    (ReExportNamespace (export_tok, star, Some alias, from, path));
                ]
            | `Export_clause_from_clause (v1, v2) ->
                (* export { name1, name2, nameN } from 'foo'; *)
                let v1 = export_clause env v1 in
                let tok2, path = from_clause env v2 in
                v1
                |> List.concat_map (fun (n1, n2opt) ->
                       let tmpname = ("!tmp_" ^ fst n1, snd n1) in
                       let import =
                         Import (tok2, [ (n1, Some tmpname) ], path)
                       in
                       let e = idexp tmpname in
                       match n2opt with
                       | None ->
                           let v = Ast_js.mk_const_var n1 e in
                           [ M import; DefStmt v; M (Export (export_tok, n1)) ]
                       | Some n2 ->
                           let v = Ast_js.mk_const_var n2 e in
                           [ M import; DefStmt v; M (Export (export_tok, n2)) ])
            | `Export_clause x ->
                (* export { import1 as name1, import2 as name2, nameN } from 'foo'; *)
                let v1 = export_clause env x in
                v1
                |> List.concat_map (fun (n1, n2opt) ->
                       match n2opt with
                       | None -> [ M (Export (export_tok, n1)) ]
                       | Some n2 ->
                           let v = Ast_js.mk_const_var n2 (idexp n1) in
                           [ DefStmt v; M (Export (export_tok, n2)) ])
          in
          let _v3 = semicolon env v3 in
          v2
      | `Rep_deco_export_choice_decl (v1, v2, v3) ->
          let decorators = List_.map (decorator env) v1 in
          let export_tok =
            token env v2
            (* "export" *)
          in
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
                let tok_default (* TODO *) =
                  token env v1
                  (* "default" *)
                in
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
                           const default = foo
                           export default
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
  | `Export_type_export_clause_opt_from_clause_choice_auto_semi
      (v1, v2, v3, _v4, v5) ->
      let _export =
        token env v1
        (* "export" *)
      in
      let _type =
        token env v2
        (* "type" *)
      in
      let _exported_types = export_clause env v3 in
      (* TODO v4 from_clause *)
      let _sc = semicolon env v5 in
      (* TODO: 'export type { foo, type bar, typeof thing };' *)
      []
  | `Export_EQ_exp_choice_auto_semi (v1, v2, v3, v4) ->
      let _v1 =
        token env v1
        (* "export" *)
      in
      let _v2 =
        token env v2
        (* "=" *)
      in
      let _v3 = expression env v3 in
      let _v4 = semicolon env v4 in
      (* TODO 'export = ZipCodeValidator;' *)
      []
  | `Export_as_name_id_choice_auto_semi (v1, v2, v3, v4, v5) ->
      let _v1 =
        token env v1
        (* "export" *)
      in
      let _v2 =
        token env v2
        (* "as" *)
      in
      let _v3 =
        token env v3
        (* "namespace" *)
      in
      let _v4 =
        token env v4
        (* identifier *)
      in
      let _v5 = semicolon env v5 in
      (* TODO 'export as namespace mathLib;' *)
      []

and type_annotation (env : env) ((v1, v2) : CST.type_annotation) =
  let v1 =
    token env v1
    (* ":" *)
  in
  let v2 = type_ env v2 in
  (v1, v2)

and anon_rep_COMMA_opt_choice_exp_ca698a5 (env : env)
    (xs : CST.anon_rep_COMMA_opt_choice_exp_ca698a5) =
  List_.filter_map
    (fun (v1, v2) ->
      let _v1 =
        token env v1
        (* "," *)
      in
      let v2 =
        match v2 with
        | Some x -> Some (anon_choice_exp_9818c1b env x)
        | None -> None
      in
      v2)
    xs

and decorator_call_expression (env : env)
    ((v1, v2, v3) : CST.decorator_call_expression) =
  let v1 = anon_choice_type_id_b8f8ced env v1 in
  let _v2_TODO =
    match v2 with
    | Some x -> Some (type_arguments env x)
    | None -> None
  in
  let v3 = arguments env v3 in
  (v1, v3)

and update_expression (env : env) (x : CST.update_expression) =
  match x with
  | `Exp_choice_PLUSPLUS (v1, v2) ->
      let v1 = expression env v1 in
      let op, t = anon_choice_PLUSPLUS_e498e28 env v2 in
      Apply (IdSpecial (IncrDecr (op, G.Postfix), t), fb [], fb [ v1 ])
  | `Choice_PLUSPLUS_exp (v1, v2) ->
      let op, t = anon_choice_PLUSPLUS_e498e28 env v1 in
      let v2 = expression env v2 in
      Apply (IdSpecial (IncrDecr (op, G.Prefix), t), fb [], fb [ v2 ])

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
      let attrs = v1 @ v2 @ v2bis @ v3 @ v5 |> List_.map attr in
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
  | `Cons_sign (v0, v1, v2, v3, v4) ->
      let v0 = kwd_attr_opt_to_list env Abstract v0 in
      let v1 =
        token env v1
        (* "new" *)
      in
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
      let fld_name = PN ("new", v1) in
      let fld_attrs = v0 |> List_.map attr in
      let fld = { fld_name; fld_attrs; fld_type = Some ty; fld_body = None } in
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
  let decorators = List_.map (decorator env) v1 in
  let access_modif =
    match v2 with
    | Some x -> (
        match x with
        | `Decl_opt_acce_modi (v1, v2) ->
            let _v1 = (* "declare" *) token env v1 in
            let v2 = accessibility_modifier_opt_to_list env v2 in
            v2
        | `Acce_modi_opt_decl (v1, v2) ->
            let v1 = accessibility_modifier env v1 in
            let _v2 =
              match v2 with
              | Some tok -> Some ((* "declare" *) token env tok)
              | None -> None
            in
            [ v1 ])
    | None -> []
  in
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
    | `Opt_acce opt -> (
        (* New thing: https://www.typescriptlang.org/docs/handbook/release-notes/typescript-4-9.html#auto-accessors-in-classes *)
        match opt with
        | Some tok -> [ (Accessor, (* "accessor" *) token env tok) ]
        | None -> [])
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
  let opt_init = initializer_opt env v7 in
  let attrs = attributes |> List_.map attr in
  Field
    {
      fld_name = prop_name;
      fld_attrs = decorators @ attrs;
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

and extends_clause (env : env) ((v1, v2, v3) : CST.extends_clause) : parent list
    =
  let _textends = (* "extends" *) token env v1 in
  let e, tparams = extends_clause_single env v2 in
  let v3 =
    List_.map
      (fun (v1, v2) ->
        let _v1 = (* "," *) token env v1 in
        let e, tparams = extends_clause_single env v2 in
        tyname_or_expr_of_expr e tparams)
      v3
  in
  tyname_or_expr_of_expr e tparams :: v3

and extends_clause_single (env : env) ((v1, v2) : CST.extends_clause_single) =
  let v1 = expression env v1 in
  let v2 =
    match v2 with
    | Some x -> type_arguments env x |> Tok.unbracket
    | None -> []
  in
  (v1, v2)

and enum_body (env : env) ((v1, v2, v3) : CST.enum_body) =
  let v1 =
    token env v1
    (* "{" *)
  in
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
  let v3 =
    token env v3
    (* "}" *)
  in
  (v1, body, v3)

and class_heritage (env : env) (x : CST.class_heritage) :
    parent list * type_ list =
  match x with
  | `Extends_clause_opt_imples_clause (v1, v2) ->
      let v1 = extends_clause env v1 in
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
      let n =
        number_as_string env tok
        (* number *)
      in
      PN n
  | `Comp_prop_name (v1, v2, v3) ->
      let _v1 =
        token env v1
        (* "[" *)
      in
      let v2 = expression env v2 in
      let _v3 =
        token env v3
        (* "]" *)
      in
      PN_Computed v2

and switch_case (env : env) ((v1, v2, v3, v4) : CST.switch_case) =
  let v1 =
    token env v1
    (* "case" *)
  in
  let v2 = expressions env v2 in
  let v3 =
    token env v3
    (* ":" *)
  in
  let v4 = List.concat_map (statement env) v4 in
  Case (v1, v2, stmt1 v3 v4)

and spread_element (env : env) ((v1, v2) : CST.spread_element) =
  let v1 =
    token env v1
    (* "..." *)
  in
  let v2 = expression env v2 in
  (v1, v2)

and expressions (env : env) (x : CST.expressions) : expr =
  match x with
  | `Exp x -> expression env x
  | `Seq_exp x -> sequence_expression env x

and abstract_method_signature (env : env)
    ((v1, v2, v3, v4, v5, v6, v7) : CST.abstract_method_signature) =
  let v1 = accessibility_modifier_opt_to_list env v1 in
  let v2 =
    [ (Abstract, token env v2) ]
    (* "abstract" *)
  in
  let v3 =
    match v3 with
    | Some tok -> [ (Override, token env tok) ] (* "override" *)
    | None -> []
  in
  let v4 =
    match v4 with
    | Some x -> [ anon_choice_get_8fb02de env x ]
    | None -> []
  in
  let v5 = property_name env v5 in
  let v6 =
    match v6 with
    | Some tok -> [ (Optional, token env tok) ] (* "?" *)
    | None -> []
  in
  let attrs = v1 @ v2 @ v3 @ v4 @ v6 |> List_.map attr in
  let _tparams, x = call_signature env v7 in
  let t = mk_functype x in
  { fld_name = v5; fld_attrs = attrs; fld_type = Some t; fld_body = None }

and finally_clause (env : env) ((v1, v2) : CST.finally_clause) =
  let v1 =
    token env v1
    (* "finally" *)
  in
  let v2 = statement_block env v2 in
  (v1, v2)

and map_type_predicate (env : env) ((v1, v2, v3) : CST.type_predicate) =
  let v1 =
    match v1 with
    | `Id tok ->
        let id =
          identifier env tok
          (* identifier *)
        in
        idexp_or_special id
    | `This tok -> this env tok
    | `Pred_type v1 ->
        let id = predefined_type env v1 in
        idexp_or_special id
  in
  let tis =
    token env v2
    (* "is" *)
  in
  let ty = type_ env v3 in
  TypeTodo (("IsType", tis), [ Expr v1; Type ty ])

and map_asserts (env : env) ((v1, v2) : CST.asserts) : type_ =
  let asserts =
    token env v1
    (* "asserts" *)
  in
  let any =
    match v2 with
    | `Type_pred x -> Type (map_type_predicate env x)
    | `Id tok ->
        let id =
          identifier env tok
          (* identifier *)
        in
        Expr (idexp_or_special id)
    | `This tok -> Expr (this env tok)
  in
  TypeTodo (("Asserts", asserts), [ any ])

and call_signature (env : env) ((v1, v2, v3) : CST.call_signature) :
    a_type_parameter list * (parameter list bracket * type_ option) =
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
        | `Asserts_anno x ->
            let ty = asserts_annotation env x in
            Some ty
        | `Type_pred_anno (v1, v2) ->
            let _v1 =
              token env v1
              (* ":" *)
            in
            let v2 = map_type_predicate env v2 in
            Some v2)
    | None -> None
  in
  (v1, (v2, v3))

and object_ (env : env) ((v1, v2, v3) : CST.object_) : a_obj =
  let v1 =
    token env v1
    (* "{" *)
  in
  let properties =
    match v2 with
    | Some (v1, v2) ->
        map_sep_list env v1 v2 (fun env x ->
            match x with
            | Some x -> [ object_property env x ]
            | None -> [])
        |> List_.flatten
    | None -> []
  in
  let v3 =
    token env v3
    (* "}" *)
  in
  (v1, properties, v3)

and type_predicate (env : env) ((v1, v2, v3) : CST.type_predicate) : type_ =
  let _e =
    match v1 with
    | `Id tok -> (* identifier *) identifier env tok |> idexp
    | `This tok -> (* "this" *) this env tok
    | `Pred_type v1 -> predefined_type env v1 |> idexp
  in
  let _is =
    token env v2
    (* "is" *)
  in
  let type_ = type_ env v3 in
  type_

and type_ (env : env) (x : CST.type_) : type_ =
  match x with
  | `Prim_type x -> primary_type env x
  | `Func_type (v1, v2, v3, v4) ->
      let _tparams =
        match v1 with
        | Some x -> type_parameters env x
        | None -> []
      in
      let params = formal_parameters env v2 in
      let _arrow =
        token env v3
        (* "=>" *)
      in
      let type_ =
        match v4 with
        | `Type x -> type_ env x
        | `Asserts x -> map_asserts env x
        | `Type_pred x ->
            let type_ = type_predicate env x in
            type_
      in
      mk_functype (params, Some type_)
  | `Read_type (v1, v2) ->
      let _TODOreadonly =
        token env v1
        (* "readonly" *)
      in
      let type_ = type_ env v2 in
      type_
  | `Cons_type (v0, v1, v2, v3, v4, v5) ->
      let _v0TODO = kwd_attr_opt_to_list env Abstract v0 in
      let v1 =
        token env v1
        (* "new" *)
      in
      let _tparams =
        match v2 with
        | Some x -> type_parameters env x
        | None -> []
      in
      let v3 = formal_parameters env v3 in
      let _v4 =
        token env v4
        (* "=>" *)
      in
      let v5 = type_ env v5 in
      let ty = mk_functype (v3, Some v5) in
      TypeTodo (("New", v1), [ Type ty ])
  | `Infer_type x -> map_infer_type env x
  | `Type_query_member_exp_in_type_anno x ->
      let e = map_type_query_member_expression_in_type_annotation env x in
      TypeTodo
        (("TypeQueryMemberInTypeAnnot", Tok.unsafe_fake_tok ""), [ Expr e ])
  | `Type_query_call_exp_in_type_anno x ->
      let e = map_type_query_call_expression_in_type_annotation env x in
      TypeTodo (("TypeQueryCallInTypeAnnot", Tok.unsafe_fake_tok ""), [ Expr e ])

and map_infer_type env (v1, v2, v3) =
  let v1 =
    token env v1
    (* "infer" *)
  in
  let v2 =
    identifier env v2
    (* identifier *)
  in
  let v3 =
    match v3 with
    | Some (v1, v2) ->
        let _v1 = (* "extends" *) token env v1 in
        let v2 = type_ env v2 in
        [ Type v2 ]
    | None -> []
  in
  TypeTodo (("Infer", v1), Type (TyName [ v2 ]) :: v3)

and type_parameters (env : env) ((v1, v2, v3, v4, v5) : CST.type_parameters) :
    a_type_parameter list =
  let _v1 =
    token env v1
    (* "<" *)
  in
  let type_params = map_sep_list env v2 v3 type_parameter in
  let _v4 =
    match v4 with
    | Some tok -> Some (token env tok) (* "," *)
    | None -> None
  in
  let _v5 =
    token env v5
    (* ">" *)
  in
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
    (a_ident * attribute list, a_pattern) Either.t =
  let decorators = List_.map (decorator env) v1 in
  let accessibility =
    accessibility_modifier_opt_to_list env v2
    |> List_.map (fun attr -> KeywordAttr attr)
  in
  let override =
    kwd_attr_opt_to_list env Override v2bis
    |> List_.map (fun attr -> KeywordAttr attr)
  in
  let readonly =
    kwd_attr_opt_to_list env Readonly v3
    |> List_.map (fun attr -> KeywordAttr attr)
  in
  let id_or_pat =
    match v4 with
    | `Pat x -> pattern env x
    | `This tok ->
        (* treating 'this' as a regular identifier for now *)
        let id =
          identifier env tok
          (* "this" *)
        in
        Left id
  in
  id_or_pat
  |> Either.map_left (fun id ->
         (id, decorators @ accessibility @ override @ readonly))

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
  let v1 =
    token env v1
    (* "{" *)
  in
  let v2 = List.concat_map (statement env) v2 in
  let v3 =
    token env v3
    (* "}" *)
  in
  let _v4 = automatic_semicolon_opt env v4 in
  Block (v1, v2, v3)

and function_declaration (env : env)
    ((v1, v2, v3, v4, v5, v6) : CST.function_declaration) : definition =
  let v1 =
    match v1 with
    | Some tok -> [ attr (Async, token env tok) ] (* "async" *)
    | None -> []
  in
  let v2 =
    token env v2
    (* "function" *)
  in
  let v3 =
    identifier env v3
    (* identifier *)
  in
  let _tparams, (v4, tret) = call_signature env v4 in
  let v5 = statement_block env v5 in
  let _v6 = automatic_semicolon_opt env v6 in
  let f_kind = (G.Function, v2) in
  let f =
    { f_attrs = v1; f_params = v4; f_body = v5; f_rettype = tret; f_kind }
  in
  (basic_entity v3, FuncDef f)

and anon_choice_type_id_940079a (env : env)
    (x : CST.anon_choice_type_id_940079a) : (a_ident, a_pattern) Either.t =
  match x with
  | `Id tok -> Left (identifier env tok) (* identifier *)
  | `Dest_pat x -> Right (destructuring_pattern env x)

and id_or_destructuring_pattern env x : (a_ident, a_pattern) Either.t =
  anon_choice_type_id_940079a env x

(* TODO: change type signature, return just a_pattern *)
and rest_pattern (env : env) ((v1, v2) : CST.rest_pattern) :
    tok * (a_ident, a_pattern) Either.t =
  let dots =
    token env v1
    (* "..." *)
  in
  let e = lhs_expression env v2 in
  (dots, Right e)

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
      let _v2_TODO =
        token env v2
        (* "?" *)
      in
      let v3 = type_annotation env v3 |> snd in
      TyTupMember v3
  | `Opt_type (v1, v2) ->
      let v1 = type_ env v1 in
      let v2 =
        token env v2
        (* "?" *)
      in
      TyTupOpt (v1, v2)
  | `Rest_type (v1, v2) ->
      let v1 =
        token env v1
        (* "..." *)
      in
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
  let attrs = v1 @ v2 @ v2bis @ v3 @ v4 @ v5 @ v7 |> List_.map attr in
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
      let v2 =
        token env v2
        (* "function" *)
      in
      let v3 =
        identifier env v3
        (* identifier *)
      in
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
      let _v1_TODO = List_.map (decorator env) v1 in
      let v2 =
        attr (Abstract, token env v2)
        (* "abstract" *)
      in
      let v3 =
        token env v3
        (* "class" *)
      in
      let v4 =
        identifier env v4
        (* identifier *)
      in
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
      let _v1 =
        token env v1
        (* "module" *)
      in
      let _id, _opt_body = module__ env v2 in
      []
      (* TODO *)
  | `Inte_module x ->
      (* namespace *)
      let _x = internal_module env x in
      []
      (* TODO *)
  | `Type_alias_decl (v1, v2, v3, v4, v5, v6) ->
      let typekwd =
        token env v1
        (* "type" *)
      in
      let id =
        str env v2
        (* identifier *)
      in
      let _tparamsTODO =
        match v3 with
        | Some x -> type_parameters env x
        | None -> []
      in
      let _teq =
        token env v4
        (* "=" *)
      in
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
      let _v2 =
        token env v2
        (* "enum" *)
      in
      let _v3 =
        identifier env v3
        (* identifier *)
      in
      let _v4 = enum_body env v4 in
      []
      (* TODO *)
  | `Inte_decl (v1, v2, v3, v4, v5) ->
      let v1 =
        token env v1
        (* "interface" *)
      in
      let v2 =
        identifier env v2
        (* identifier *)
      in
      let _v3 =
        match v3 with
        | Some x -> type_parameters env x
        | None -> []
      in
      let v4 =
        match v4 with
        | Some x -> extends_type_clause env x
        | None -> []
      in
      let t1, xs, t2 = object_type env v5 in
      let xs =
        xs
        |> List_.filter_map (function
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
      let _v1 =
        token env v1
        (* "import" *)
      in
      let _v2 =
        identifier env v2
        (* identifier *)
      in
      let _v3 =
        token env v3
        (* "=" *)
      in
      let _v4 = id_or_nested_id env v4 in
      let _v5 = semicolon env v5 in
      []
      (* TODO *)
  | `Ambi_decl (v1, v2) ->
      let _v1 =
        token env v1
        (* "declare" *)
      in
      let v2 =
        match v2 with
        | `Decl x -> declaration env x
        | `Global_stmt_blk (v1, v2) ->
            let v1 =
              token env v1
              (* "global" *)
            in
            let v2 = statement_block env v2 in
            let name = ("!global!", v1) in
            let f_kind = (G.LambdaKind, fake) in
            let f =
              {
                f_attrs = [];
                f_params = fb [];
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
            let tok_module =
              token env v1
              (* "module" *)
            in
            let _dot =
              token env v2
              (* "." *)
            in
            let name =
              identifier env v3
              (* identifier *)
            in
            let _colon =
              token env v4
              (* ":" *)
            in
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

and extends_type_clause (env : env) ((v1, v2, v3) : CST.extends_type_clause) :
    parent list =
  let _textends = (* "extends" *) token env v1 in
  let v2 = map_anon_choice_type_id_a85f573 env v2 in
  let v3 =
    List_.map
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

let semgrep_pattern (env : env) (x : CST.semgrep_pattern) : any =
  match x with
  | `Exp x -> Expr (expression env x)
  | `Pair x -> (
      match x with
      | `Prop_name_COLON_exp (v1, v2, v3) -> (
          let v1 = property_name env v1 in
          let v2 =
            token env v2
            (* ":" *)
          in
          let v3 = expression env v3 in
          match v1 with
          | PN id -> Partial (PartialSingleField (id, v2, v3))
          (* This probably shouldn't happen. We expect any pattern like
           `foo: <expr>`
           to have just a simple ident on the left.
         *)
          | PN_Computed _e ->
              Partial
                (PartialSingleField
                   (("PN_Computed", Tok.unsafe_fake_tok "PN_Computed"), v2, v3))
          )
      | `Semg_ellips tok -> Expr (Ellipsis (token env tok)))

let program (env : env) (x : CST.program) : any =
  match x with
  | `Opt_hash_bang_line_rep_stmt (v1, v2) ->
      let _v1 =
        match v1 with
        | Some tok -> Some (token env tok) (* pattern #!.* *)
        | None -> None
      in
      let v2 = List.concat_map (toplevel env) v2 in
      Program v2
  | `Semg_exp (v1, v2) ->
      let _v1 = token env v1 in
      let v2 = semgrep_pattern env v2 in
      v2
  | `Switch_case v1 -> Partial (PartialSwitchCase (switch_case env v1))

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
        `TSX
      else `Typescript

type cst_result =
  ( CST_tree_sitter_typescript.program,
    CST_tree_sitter_typescript.extra )
  Tree_sitter_run.Parsing_result.t

let parse ?dialect file =
  let debug = false in
  H.wrap_parser
    (fun () ->
      let dialect = guess_dialect dialect !!file in
      match dialect with
      | `Typescript ->
          let cst = Tree_sitter_typescript.Parse.file !!file in
          (cst :> cst_result)
      | `TSX ->
          let cst = Tree_sitter_tsx.Parse.file !!file in
          (cst :> cst_result))
    (fun cst _extras ->
      let env = { H.file; conv = H.line_col_to_pos file; extra = Program } in

      if debug then (
        Printexc.record_backtrace true;
        Boilerplate_tree_sitter_typescript.dump_tree cst);

      match program env cst with
      | Program p -> p
      | _ -> failwith "not a program")

(* What's the deal with this?
 *
 * In the pattern-parsing world, we can either parse something which is a whole program,
 * or the rule-writer may want to pass us something which is just an expression, or some
 * other AST node which does not qualify as an entire standalone program.
 *
 * In which case, we use a special entry point, `__SEMGREP_EXPRESSION`, to signal to the
 * tree-sitter parser which case we would like to exercise.
 *
 * Usually it is straightforward to just run one and then keep the other for backup.
 * Unfortunately, there are cases of patterns which parse both with `__SEMGREP_EXPRESSION`
 * and without. This means that we have to pick one arbitrarily.
 *
 * We have decided that our canonical order will be to first try it without the prefix,
 * using the prefix as a fallback, and and then using a function
 * `is_semgrep_pattern_we_would_rather_parse_as_expression` for certain program forms
 * which we would prefer to add the prefix for.
 *
 * For instance, we would prefer to parse `{...}` and `foo: e` as expressions, but
 * we would prefer to parse `function foo(...) {...}` as a function definition, not
 * a lambda.

 * This is not a perfect solution, but it's fine for now.
 *)
let parse_expression_or_source_file str =
  let res = Tree_sitter_tsx.Parse.string str in
  match res with
  (* If there are errors, we must try to parse it the other way. *)
  | { errors = _ :: _; _ } ->
      let expr_str = "__SEMGREP_EXPRESSION " ^ str in
      Tree_sitter_tsx.Parse.string expr_str
  (* If we succeeded, we may want to still switch over to the expression case. *)
  | { program = Some cst; _ }
    when is_semgrep_pattern_we_would_rather_parse_as_expression cst -> (
      let expr_str = "__SEMGREP_EXPRESSION " ^ str in
      let res2 = Tree_sitter_tsx.Parse.string expr_str in
      match res2 with
      (* If this one works, we're all good. *)
      | { errors = []; _ } -> res2
      (* But if there were errors, just use the `res` we know succeeded. *)
      | _ -> res)
  | _ -> res

let parse_pattern str =
  H.wrap_parser
    (fun () -> (parse_expression_or_source_file str :> cst_result))
    (fun cst _extras ->
      let file = Fpath.v "<pattern>" in
      let env =
        { H.file; conv = H.line_col_to_pos_pattern str; extra = Pattern }
      in
      (* A pattern which produces this particular construct likely looks like
       * function foo(...)
       * which we prefer to parse as a partial function def, not a function signature.
       *)
      match program env cst with
      | Program
          [
            DefStmt
              ( ent,
                VarDef
                  {
                    v_kind = Const, v2;
                    v_init = None;
                    v_type = Some (TyFun (params, retty));
                  } );
          ] ->
          Partial
            (PartialDef
               ( ent,
                 FuncDef
                   {
                     f_kind = (G.Function, v2);
                     f_params = fb params;
                     f_body = Block (fb []);
                     f_rettype = retty;
                     f_attrs = [];
                   } ))
      (* Similarly, this is likely a partial `if`. It looks like `if(e)`.
       *)
      | Expr (Apply (Id ("if", tok), (_, [], _), (_, [ e ], _))) ->
          Partial (PartialIf (tok, e))
      | Program ss -> Stmts ss
      | other -> other)
