(* Yoann Padioleau
 *
 * Copyright (c) 2021 R2C
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * LICENSE for more details.
 *)
open Common
module CST = Tree_sitter_vue.CST
module H = Parse_tree_sitter_helpers
open AST_generic
module G = AST_generic

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Vue parser using tree-sitter-lang/semgrep-vue and converting
 * to ast_js.ml
 *
 * There are similarities with the code in Parse_html_tree_sitter.ml.
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
type extra = {
  (* todo: later we should also propagate parsing errors *)
  parse_js_program : string wrap -> AST_generic.program;
  parse_js_expr : string wrap -> AST_generic.expr;
}

type env = extra H.env

let fake = AST_generic.fake
let token = H.token
let str = H.str
let fb = Tok.unsafe_fake_bracket

(* val str_if_wrong_content_temporary_fix :
   'a env -> Tree_sitter_run.Token.t -> string * Parse_info.t
*)
(* This is a temporary fix until
 * https://github.com/returntocorp/ocaml-tree-sitter-core/issues/5
 * is fixed.
 *)
let str_if_wrong_content_temporary_fix env (tok : Tree_sitter_run.Token.t) =
  let loc, _wrong_str = tok in

  let file = env.H.file in
  let h = env.H.conv in

  let bytepos, line, column =
    let pos = loc.Tree_sitter_run.Loc.start in
    (* Parse_info is 1-line based and 0-column based, like Emacs *)
    let line = pos.Tree_sitter_run.Loc.row + 1 in
    let column = pos.Tree_sitter_run.Loc.column in
    try (Hashtbl.find h (line, column), line, column) with
    | Not_found ->
        failwith (spf "could not find line:%d x col:%d in %s" line column file)
  in
  let charpos2 =
    let pos = loc.Tree_sitter_run.Loc.end_ in
    (* Parse_info is 1-line based and 0-column based, like Emacs *)
    let line = pos.Tree_sitter_run.Loc.row + 1 in
    let column = pos.Tree_sitter_run.Loc.column in
    try Hashtbl.find h (line, column) with
    | Not_found ->
        failwith (spf "could not find line:%d x col:%d in %s" line column file)
  in
  (* Range.t is inclusive, so we need -1 to remove the char at the pos *)
  let charpos2 = charpos2 - 1 in
  let r = { Range.start = bytepos; end_ = charpos2 } in
  let str = Range.content_at_range file r in
  let pos = Pos.make ~file ~line ~column bytepos in
  let tok_loc = { Tok.str; pos } in
  (str, Tok.tok_of_loc tok_loc)

(*****************************************************************************)
(* Boilerplate converter *)
(*****************************************************************************)
(* This was started by copying tree-sitter-lang/semgrep-vue/Boilerplate.ml *)

(**
   Boilerplate to be used as a template when mapping the vue CST
   to another type of tree.
*)

let map_end_tag (env : env) ((v1, v2, v3) : CST.end_tag) : tok =
  let v1 = token env v1 (* "</" *) in
  let v2 = token env v2 (* end_tag_name *) in
  let v3 = token env v3 (* ">" *) in
  Tok.combine_toks v1 [ v2; v3 ]

let map_text (env : env) (x : CST.text) =
  match x with
  | `Text_frag tok -> str env tok (* text_fragment *)
  (* ?? not an interpolation? *)
  | `LCURLLCURL tok -> str env tok

(* "{{" *)

let map_quoted_attribute_value (env : env) (x : CST.quoted_attribute_value) :
    string wrap bracket =
  match x with
  | `SQUOT_opt_pat_58fbb2e_SQUOT (v1, v2, v3) ->
      let l = token env v1 (* "'" *) in
      let xs =
        match v2 with
        | Some tok -> [ str env tok ] (* pattern "[^']+" *)
        | None -> []
      in
      let r = token env v3 (* "'" *) in
      G.string_ (l, xs, r)
  | `DQUOT_opt_pat_98d585a_DQUOT (v1, v2, v3) ->
      let l = token env v1 (* "\"" *) in
      let xs =
        match v2 with
        | Some tok -> [ str env tok ] (* pattern "[^\"]+" *)
        | None -> []
      in
      let r = token env v3 (* "\"" *) in
      G.string_ (l, xs, r)

let map_directive_modifiers (env : env) (xs : CST.directive_modifiers) =
  Common.map
    (fun (v1, v2) ->
      let v1 = token env v1 (* "." *) in
      let v2 = str env v2 (* pattern "[^<>\"'/=\\s.]+" *) in
      (v1, v2))
    xs

let map_anon_choice_attr_value_5986531 (env : env)
    (x : CST.anon_choice_attr_value_5986531) : a_xml_attr_value =
  match x with
  | `Attr_value tok ->
      let x = str env tok (* pattern "[^<>\"'=\\s]+" *) in
      N (Id (x, G.empty_id_info ())) |> G.e
  | `Quoted_attr_value x ->
      let x = map_quoted_attribute_value env x in
      L (String x) |> G.e

let map_anon_choice_dire_arg_b33821e (env : env)
    (x : CST.anon_choice_dire_arg_b33821e) =
  match x with
  | `Dire_arg tok ->
      let id = str env tok (* pattern "[^<>\"'/=\\s.]+" *) in
      Left id
  | `Dire_dyna_arg (v1, v2, v3) ->
      let v1 = token env v1 (* "[" *) in
      let v2 =
        match v2 with
        | Some tok -> Some (str env tok) (* pattern "[^<>\"'/=\\s\\]]+" *)
        | None -> None
      in
      let v3 = token env v3 (* "]" *) in
      Right (v1, v2, v3)

let map_anon_choice_attr_a1991da (env : env) (x : CST.anon_choice_attr_a1991da)
    : xml_attribute =
  match x with
  | `Attr (v1, v2) ->
      let id = str env v1 (* pattern "[^<>\"'=/\\s]+" *) in
      let teq, v2 =
        match v2 with
        | Some (v1, v2) ->
            let v1 = token env v1 (* "=" *) in
            let v2 = map_anon_choice_attr_value_5986531 env v2 in
            (v1, v2)
        | None ->
            (* <foo a /> <=> <foo a=true>? That's what we do for JSX, but should
             * we instead introduce a XmlAttrNoValue in AST_generic?
             *)
            let v = L (Bool (true, fake "true")) |> G.e in
            (fake "=", v)
      in
      XmlAttr (id, teq, v2)
  | `Dire_attr (v1, v2, v3) ->
      let id =
        match v1 with
        | `Dire_name_opt_imm_tok_prec_p1_colon_choice_dire_arg (v1, v2) ->
            let v1 = str env v1 (* directive_name *) in
            let _v2TODO =
              match v2 with
              | Some (v1, v2) ->
                  let v1 = token env v1 (* ":" *) in
                  let v2 = map_anon_choice_dire_arg_b33821e env v2 in
                  Some (v1, v2)
              | None -> None
            in
            v1
        | `Dire_shor_choice_dire_arg (v1, v2) ->
            let v1 = str env v1 (* directive_shorthand *) in
            let _v2 = map_anon_choice_dire_arg_b33821e env v2 in
            v1
      in
      let _v2TODO =
        match v2 with
        | Some x -> map_directive_modifiers env x
        | None -> []
      in
      let teq, v3 =
        match v3 with
        | Some (v1, v2) ->
            let v1 = token env v1 (* "=" *) in
            let v2 = map_anon_choice_attr_value_5986531 env v2 in
            (v1, v2)
        | None ->
            (* <foo a /> <=> <foo a=true>? That's what we do for JSX, but should
             * we instead introduce a XmlAttrNoValue in AST_generic?
             *)
            let v = L (Bool (true, fake "true")) |> G.e in
            (fake "=", v)
      in
      (* TODO: XmlDynAttr? *)
      XmlAttr (id, teq, v3)

let map_start_tag (env : env) ((v1, v2, v3, v4) : CST.start_tag) =
  let v1 = token env v1 (* "<" *) in
  let v2 = str env v2 (* start_tag_name *) in
  let v3 = Common.map (map_anon_choice_attr_a1991da env) v3 in
  let v4 = token env v4 (* ">" *) in
  (v1, v2, v3, v4)

let map_template_start_tag (env : env)
    ((v1, v2, v3, v4) : CST.template_start_tag) =
  let v1 = token env v1 (* "<" *) in
  let v2 = str env v2 (* template_start_tag_name *) in
  let v3 = Common.map (map_anon_choice_attr_a1991da env) v3 in
  let v4 = token env v4 (* ">" *) in
  (v1, v2, v3, v4)

let map_style_start_tag (env : env) ((v1, v2, v3, v4) : CST.style_start_tag) =
  let v1 = token env v1 (* "<" *) in
  let v2 = str env v2 (* style_start_tag_name *) in
  let v3 = Common.map (map_anon_choice_attr_a1991da env) v3 in
  let v4 = token env v4 (* ">" *) in
  (v1, v2, v3, v4)

let map_script_start_tag (env : env) ((v1, v2, v3, v4) : CST.script_start_tag) =
  let v1 = token env v1 (* "<" *) in
  let v2 = str env v2 (* script_start_tag_name *) in
  let v3 = Common.map (map_anon_choice_attr_a1991da env) v3 in
  let v4 = token env v4 (* ">" *) in
  (v1, v2, v3, v4)

let map_style_element (env : env) ((v1, v2, v3) : CST.style_element) : xml =
  let l, id, attrs, r = map_style_start_tag env v1 in
  let v2 =
    match v2 with
    | Some tok -> [ XmlText (str env tok) (* raw_text *) ]
    | None -> []
  in
  let v3 = map_end_tag env v3 in
  { xml_kind = XmlClassic (l, id, r, v3); xml_attrs = attrs; xml_body = v2 }

let map_script_element (env : env) ((v1, v2, v3) : CST.script_element) =
  let l, id, attrs, r = map_script_start_tag env v1 in
  let v2 =
    match v2 with
    | Some tok ->
        (* TODO: https://github.com/returntocorp/ocaml-tree-sitter-core/issues/5 *)
        let v = str_if_wrong_content_temporary_fix env tok in
        Some v
        (* raw_text *)
    | None -> None
  in
  let v3 = map_end_tag env v3 in
  (l, id, r, v3, attrs, v2)

let rec map_element (env : env) (x : CST.element) : xml =
  match x with
  | `Start_tag_rep_node_choice_end_tag (v1, v2, v3) ->
      let l, id, attrs, r = map_start_tag env v1 in
      let v2 = List.concat_map (map_node env) v2 in
      let v3 =
        match v3 with
        | `End_tag x -> map_end_tag env x
        | `Impl_end_tag tok -> token env tok
        (* implicit_end_tag *)
      in
      { xml_kind = XmlClassic (l, id, r, v3); xml_attrs = attrs; xml_body = v2 }
  | `Self_clos_tag (v1, v2, v3, v4) ->
      let l = token env v1 (* "<" *) in
      let id = str env v2 (* start_tag_name *) in
      let attrs = Common.map (map_anon_choice_attr_a1991da env) v3 in
      let r = token env v4 (* "/>" *) in
      { xml_kind = XmlSingleton (l, id, r); xml_attrs = attrs; xml_body = [] }

and map_node (env : env) (x : CST.node) : xml_body list =
  match x with
  | `Comm tok ->
      let _x = token env tok (* comment *) in
      []
  | `Text x ->
      let x = map_text env x in
      [ XmlText x ]
  | `Interp (v1, v2, v3) ->
      let v1 = token env v1 (* "{{" *) in
      let v2 =
        match v2 with
        | Some tok ->
            let x = str env tok (* interpolation_text *) in
            (* TODO: parse as JS *)
            Some (L (String (fb x)) |> G.e)
        | None -> None
      in
      let v3 = token env v3 (* "}}" *) in
      [ XmlExpr (v1, v2, v3) ]
  | `Elem x ->
      let xml = map_element env x in
      [ XmlXml xml ]
  | `Temp_elem x ->
      let xml = map_template_element env x in
      [ XmlXml xml ]
  | `Script_elem x ->
      let l, id, r, rend, xml_attrs, body_opt = map_script_element env x in
      let xml_body =
        match body_opt with
        (* TODO: parse as JS *)
        | Some s -> [ XmlText s ]
        | None -> []
      in
      let xml =
        { xml_kind = XmlClassic (l, id, r, rend); xml_attrs; xml_body }
      in
      [ XmlXml xml ]
  (* less: parse as CSS *)
  | `Style_elem x ->
      let xml = map_style_element env x in
      [ XmlXml xml ]
  | `Errons_end_tag (v1, v2, v3) ->
      let l = token env v1 (* "</" *) in
      let id = str env v2 (* erroneous_end_tag_name *) in
      let r = token env v3 (* ">" *) in
      (* todo? raise an exn instead? *)
      let xml =
        { xml_kind = XmlSingleton (l, id, r); xml_attrs = []; xml_body = [] }
      in
      [ XmlXml xml ]

and map_template_element (env : env) ((v1, v2, v3) : CST.template_element) : xml
    =
  let l, id, attrs, r = map_template_start_tag env v1 in
  let v2 = List.concat_map (map_node env) v2 in
  let v3 = map_end_tag env v3 in
  { xml_kind = XmlClassic (l, id, r, v3); xml_attrs = attrs; xml_body = v2 }

let map_component (env : env) (xs : CST.component) : stmt list =
  List.concat_map
    (fun x ->
      match x with
      | `Comm tok ->
          let _x = token env tok (* comment *) in
          []
      | `Elem x ->
          let xml = map_element env x in
          [ G.exprstmt (Xml xml |> G.e) ]
      | `Temp_elem x ->
          let xml = map_template_element env x in
          [ G.exprstmt (Xml xml |> G.e) ]
      (* Note that right now the AST will not contain the enclosing
       * <script>, because XmlExpr contain single expressions, not
       * full programs, so it's simpler to just lift up the
       * program and remove the enclosing <script>. If at some point
       * people want to explicitly restrict their code search to
       * the <script> part we might revisit that.
       *)
      | `Script_elem x -> (
          let _l, _id, _r, _rend, _xml_attrs, body_opt =
            map_script_element env x
          in
          match body_opt with
          | Some s -> env.extra.parse_js_program s
          | None -> [])
      (* less: parse as CSS *)
      | `Style_elem x ->
          let xml = map_style_element env x in
          [ G.exprstmt (Xml xml |> G.e) ])
    xs

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

(* TODO: move in Parse_tree_sitter_helpers.ml *)
let parse_string_and_adjust_wrt_base content tbase fparse =
  Common2.with_tmp_file ~str:content ~ext:"js" (fun file ->
      let x = fparse file in

      let visitor =
        object (_self : 'self)
          inherit [_] AST_generic.map_legacy

          method! visit_tok _env t =
            let base_loc = Tok.unsafe_loc_of_tok tbase in
            Tok.adjust_tok_wrt_base base_loc t
        end
      in
      visitor#visit_program () x)

let parse parse_js file =
  H.wrap_parser
    (fun () -> Tree_sitter_vue.Parse.file file)
    (fun cst ->
      let extra =
        {
          parse_js_program =
            (fun (s, t) -> parse_string_and_adjust_wrt_base s t parse_js);
          parse_js_expr =
            (fun (s, t) ->
              let _xs = parse_string_and_adjust_wrt_base s t parse_js in
              failwith "TODO: extract expr");
        }
      in
      let env = { H.file; conv = H.line_col_to_pos file; extra } in

      let xs = map_component env cst in
      xs)
