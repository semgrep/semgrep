(* Yoann Padioleau
 *
 * Copyright (c) 2021 R2C
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file license.txt.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * license.txt for more details.
 *)
module CST = Tree_sitter_html.CST
module H = Parse_tree_sitter_helpers
open AST_generic
module G = AST_generic
module PI = Parse_info

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* HTML parser using tree-sitter-lang/semgrep-html and converting
 * directly to AST_generic.ml
 *
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
type env = unit H.env

let fake = AST_generic.fake
let token = H.token
let str = H.str

(*****************************************************************************)
(* Boilerplate converter *)
(*****************************************************************************)
(* This was started by copying tree-sitter-lang/semgrep-html/Boilerplate.ml *)

(**
   Boilerplate to be used as a template when mapping the html CST
   to another type of tree.
*)

let map_quoted_attribute_value (env : env) (x : CST.quoted_attribute_value) :
    string wrap =
  match x with
  | `SQUOT_opt_pat_58fbb2e_SQUOT (v1, v2, v3) ->
      let v1 = token env v1 (* "'" *) in
      let s, ts =
        match v2 with
        | Some tok ->
            let s, t = str env tok (* pattern "[^']+" *) in
            (s, [ t ])
        | None -> ("", [])
      in
      let v3 = token env v3 (* "'" *) in
      (s, PI.combine_infos v1 (ts @ [ v3 ]))
  | `DQUOT_opt_pat_98d585a_DQUOT (v1, v2, v3) ->
      let v1 = token env v1 (* "\"" *) in
      let s, ts =
        match v2 with
        | Some tok ->
            let s, t = str env tok (* pattern "[^\"]+" *) in
            (s, [ t ])
        | None -> ("", [])
      in
      let v3 = token env v3 (* "\"" *) in
      (s, PI.combine_infos v1 (ts @ [ v3 ]))

let map_end_tag (env : env) (x : CST.end_tag) : tok =
  match x with
  | `Semg_end_tag (v1, v2, v3)
  | `LTSLASH_end_tag_name_GT (v1, v2, v3) ->
      let v1 = token env v1 (* "</" *) in
      let v2 = token env v2 (* end_tag_name *) in
      let v3 = token env v3 (* ">" *) in
      PI.combine_infos v1 [ v2; v3 ]

let map_attribute (env : env) ((v1, v2) : CST.attribute) : xml_attribute =
  let id = str env v1 (* pattern "[^<>\"'/=\\s]+" *) in
  match v2 with
  | Some (v1, v2) ->
      let v1 = token env v1 (* "=" *) in
      let v2 =
        match v2 with
        | `Attr_value tok ->
            (* todo: remove quotes? *)
            let v = str env tok (* pattern "[^<>\"'=\\s]+" *) in
            L (String v) |> G.e
        | `Quoted_attr_value x ->
            let v = map_quoted_attribute_value env x in
            L (String v) |> G.e
      in
      XmlAttr (id, v1, v2)
  | None ->
      (* <foo a /> <=> <foo a=true>? That's what we do for JSX, but should
       * we instead introduce a XmlAttrNoValue in AST_generic?
       *)
      (* sgrep-ext: *)
      if fst id = "..." then XmlEllipsis (snd id)
      else
        let v = L (Bool (true, fake "true")) |> G.e in
        XmlAttr (id, fake "=", v)

let map_script_start_tag (env : env) ((v1, v2, v3, v4) : CST.script_start_tag) =
  let v1 = token env v1 (* "<" *) in
  let v2 = str env v2 (* script_start_tag_name *) in
  let v3 = Common.map (map_attribute env) v3 in
  let v4 = token env v4 (* ">" *) in
  (v1, v2, v3, v4)

let map_style_start_tag (env : env) ((v1, v2, v3, v4) : CST.style_start_tag) =
  let v1 = token env v1 (* "<" *) in
  let v2 = str env v2 (* style_start_tag_name *) in
  let v3 = Common.map (map_attribute env) v3 in
  let v4 = token env v4 (* ">" *) in
  (v1, v2, v3, v4)

let map_start_tag (env : env) (x : CST.start_tag) =
  match x with
  | `Semg_start_tag (v1, v2, v3, v4)
  | `LT_start_tag_name_rep_attr_GT (v1, v2, v3, v4) ->
      let v1 = token env v1 (* "<" *) in
      let v2 = str env v2 (* start_tag_name *) in
      let v3 = Common.map (map_attribute env) v3 in
      let v4 = token env v4 (* ">" *) in
      (v1, v2, v3, v4)

let rec map_element (env : env) (x : CST.element) : xml =
  match x with
  | `Start_tag_rep_node_choice_end_tag (v1, v2, v3) ->
      let l, id, attrs, r = map_start_tag env v1 in
      let v2 = map_fragment env v2 in
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
      let attrs = Common.map (map_attribute env) v3 in
      let r = token env v4 (* "/>" *) in
      { xml_kind = XmlSingleton (l, id, r); xml_attrs = attrs; xml_body = [] }

and map_fragment (env : env) (xs : CST.fragment) : xml_body list =
  Common.map (map_node env) xs

and map_node (env : env) (x : CST.node) : xml_body =
  match x with
  | `Doct_ (v1, v2, v3, v4) ->
      let l = token env v1 (* "<!" *) in
      let id = str env v2 (* pattern [Dd][Oo][Cc][Tt][Yy][Pp][Ee] *) in
      let _misc = token env v3 (* pattern [^>]+ *) in
      let r = token env v4 (* ">" *) in
      let xml =
        {
          xml_kind = XmlSingleton (l, id, r);
          xml_attrs = [];
          (* less: use misc? *)
          xml_body = [];
        }
      in
      XmlXml xml
  | `Text tok ->
      let v1 = str env tok (* pattern [^<>]+ *) in
      XmlText v1
  | `Elem x ->
      let v1 = map_element env x in
      XmlXml v1
  | `Script_elem (v1, v2, v3) ->
      let l, id, attrs, r = map_script_start_tag env v1 in
      let v2 =
        match v2 with
        | Some tok -> [ XmlText (str env tok) ] (* raw_text *)
        | None -> []
      in
      let v3 = map_end_tag env v3 in
      let xml =
        {
          xml_kind = XmlClassic (l, id, r, v3);
          xml_attrs = attrs;
          xml_body = v2;
        }
      in
      XmlXml xml
  | `Style_elem (v1, v2, v3) ->
      let l, id, attrs, r = map_style_start_tag env v1 in
      let v2 =
        match v2 with
        | Some tok -> [ XmlText (str env tok) ] (* raw_text *)
        | None -> []
      in
      let v3 = map_end_tag env v3 in
      let xml =
        {
          xml_kind = XmlClassic (l, id, r, v3);
          xml_attrs = attrs;
          xml_body = v2;
        }
      in
      XmlXml xml
  | `Errons_end_tag (v1, v2, v3) ->
      let l = token env v1 (* "</" *) in
      let id = str env v2 (* erroneous_end_tag_name *) in
      let r = token env v3 (* ">" *) in
      (* todo? raise an exception instead? *)
      let xml =
        { xml_kind = XmlSingleton (l, id, r); xml_attrs = []; xml_body = [] }
      in
      XmlXml xml

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let parse file =
  H.wrap_parser
    (fun () -> Tree_sitter_html.Parse.file file)
    (fun cst ->
      let env = { H.file; conv = H.line_col_to_pos file; extra = () } in

      let xs = map_fragment env cst in
      let xml =
        {
          xml_kind = XmlFragment (fake "", fake "");
          xml_attrs = [];
          xml_body = xs;
        }
      in
      let e = Xml xml |> G.e in
      let st = G.exprstmt e in
      [ st ])

let parse_pattern str =
  H.wrap_parser
    (fun () -> Tree_sitter_html.Parse.string str)
    (fun cst ->
      let file = "<pattern>" in
      let env = { H.file; conv = Hashtbl.create 0; extra = () } in

      let xs = map_fragment env cst in
      match xs with
      (* TODO: not sure why the parser adds thos enclosing XmlText ""
       * Is this still needed after
       * https://github.com/tree-sitter/tree-sitter-html/commit/af9339f3deb131ab99acfac906713b81dbcc41c9 ?
       *)
      | [ XmlText ("", _); XmlXml xml; XmlText ("", _) ] ->
          G.E (G.Xml xml |> G.e)
      | [ XmlXml xml ] -> G.E (G.Xml xml |> G.e)
      | _ ->
          let xml =
            {
              xml_kind = XmlFragment (fake "", fake "");
              xml_attrs = [];
              xml_body = xs;
            }
          in
          let e = Xml xml |> G.e in
          G.E e)
