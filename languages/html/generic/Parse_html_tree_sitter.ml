(* Yoann Padioleau
 *
 * Copyright (c) 2021, 2023 R2C
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
open Fpath_.Operators
module CST = Tree_sitter_html.CST
module H = Parse_tree_sitter_helpers
open AST_generic
module G = AST_generic

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* HTML parser using tree-sitter-lang/semgrep-html and converting
 * directly to AST_generic.ml.
 *
 * This is also now used to parse XML!
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
type env = unit H.env

let fake = AST_generic.fake
let token = H.token
let str = H.str
let fb = Tok.unsafe_fake_bracket

(*****************************************************************************)
(* Boilerplate converter *)
(*****************************************************************************)
(* This was started by copying tree-sitter-lang/semgrep-html/Boilerplate.ml *)

(**
   Boilerplate to be used as a template when mapping the html CST
   to another type of tree.
*)

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

let map_end_tag (env : env) (x : CST.end_tag) : tok =
  match x with
  | `Semg_end_tag (v1, v2, v3)
  | `LTSLASH_end_tag_name_GT (v1, v2, v3) ->
      let v1 = token env v1 (* "</" *) in
      let v2 = token env v2 (* end_tag_name *) in
      let v3 = token env v3 (* ">" *) in
      Tok.combine_toks v1 [ v2; v3 ]

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
            L (String (fb v)) |> G.e
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
  let v2 = Id (str env v2 (* script_start_tag_name *), G.empty_id_info ()) in
  let v3 = List_.map (map_attribute env) v3 in
  let v4 = token env v4 (* ">" *) in
  (v1, v2, v3, v4)

let map_style_start_tag (env : env) ((v1, v2, v3, v4) : CST.style_start_tag) =
  let v1 = token env v1 (* "<" *) in
  let v2 = Id (str env v2 (* style_start_tag_name *), G.empty_id_info ()) in
  let v3 = List_.map (map_attribute env) v3 in
  let v4 = token env v4 (* ">" *) in
  (v1, v2, v3, v4)

let map_start_tag (env : env) (x : CST.start_tag) =
  match x with
  | `Semg_start_tag (v1, v2, v3, v4)
  | `LT_start_tag_name_rep_attr_GT (v1, v2, v3, v4) ->
      let v1 = token env v1 (* "<" *) in
      let v2 = Id (str env v2 (* start_tag_name *), G.empty_id_info ()) in
      let v3 = List_.map (map_attribute env) v3 in
      let v4 = token env v4 (* ">" *) in
      (v1, v2, v3, v4)

let rec map_element (env : env) (x : CST.element) : xml =
  match x with
  | `Start_tag_rep_node_choice_end_tag (v1, v2, v3) ->
      let l, id, attrs, r = map_start_tag env v1 in
      let v2 = List_.map (map_node env) v2 in
      let v3 =
        match v3 with
        | `End_tag x -> map_end_tag env x
        | `Impl_end_tag tok -> token env tok
        (* implicit_end_tag *)
      in
      { xml_kind = XmlClassic (l, id, r, v3); xml_attrs = attrs; xml_body = v2 }
  | `Self_clos_tag (v1, v2, v3, v4) ->
      let l = token env v1 (* "<" *) in
      let id = Id (str env v2 (* start_tag_name *), G.empty_id_info ()) in
      let attrs = List_.map (map_attribute env) v3 in
      let r = token env v4 (* "/>" *) in
      { xml_kind = XmlSingleton (l, id, r); xml_attrs = attrs; xml_body = [] }

and map_node (env : env) (x : CST.node) : xml_body =
  match x with
  | `Doct_ (v1, v2, v3, v4) ->
      let l = token env v1 (* "<!" *) in
      let id =
        Id
          ( str env v2 (* pattern [Dd][Oo][Cc][Tt][Yy][Pp][Ee] *),
            G.empty_id_info () )
      in
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
  | `Entity tok ->
      (* e.g. "&lt;" *)
      XmlText (str env tok)
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
      let id =
        Id (str env v2 (* erroneous_end_tag_name *), G.empty_id_info ())
      in
      let r = token env v3 (* ">" *) in
      (* todo? raise an exception instead? *)
      let xml =
        { xml_kind = XmlSingleton (l, id, r); xml_attrs = []; xml_body = [] }
      in
      XmlXml xml

let map_toplevel_node (env : env) (x : CST.toplevel_node) : xml_body =
  match x with
  | `Xmld (v1, v2, v3) ->
      let l = (* "<?xml" *) token env v1 in
      let xml_attrs = List_.map (map_attribute env) v2 in
      let r = (* "?>" *) token env v3 in
      let xml =
        {
          xml_kind = XmlSingleton (l, Id (("xml", l), G.empty_id_info ()), r);
          xml_attrs;
          xml_body = [];
        }
      in
      XmlXml xml
  | `Doct_ (v1, v2, v3, v4) -> map_node env (`Doct_ (v1, v2, v3, v4))
  | `Elem x -> map_node env (`Elem x)
  | `Script_elem (v1, v2, v3) -> map_node env (`Script_elem (v1, v2, v3))
  | `Style_elem (v1, v2, v3) -> map_node env (`Style_elem (v1, v2, v3))
  | `Errons_end_tag (v1, v2, v3) -> map_node env (`Errons_end_tag (v1, v2, v3))

let map_document (env : env) (x : CST.document) :
    (xml_body list, xml_attribute) Either.t =
  match x with
  | `Rep_topl_node v1 ->
      let xml_bodys = List_.map (map_toplevel_node env) v1 in
      Left xml_bodys
  | `Topl_attr (v1, v2, v3) ->
      let id = str env v1 (* pattern "[^<>\"'/=\\s]+" *) in
      let teq = token env v2 (* "=" *) in
      let attr_val =
        match v3 with
        | `Attr_value tok ->
            (* todo: remove quotes? *)
            let v = str env tok (* pattern "[^<>\"'=\\s]+" *) in
            L (String (fb v)) |> G.e
        | `Quoted_attr_value x ->
            let v = map_quoted_attribute_value env x in
            L (String v) |> G.e
      in
      let xattr = XmlAttr (id, teq, attr_val) in
      Right xattr

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let parse file =
  H.wrap_parser
    (fun () -> Tree_sitter_html.Parse.file !!file)
    (fun cst _extras ->
      let env = { H.file; conv = H.line_col_to_pos file; extra = () } in
      match map_document env cst with
      | Left xs ->
          let xml =
            {
              xml_kind = XmlFragment (fake "", fake "");
              xml_attrs = [];
              xml_body = xs;
            }
          in
          let e = Xml xml |> G.e in
          let st = G.exprstmt e in
          [ st ]
      (* TODO? should convert in an XmlText? *)
      | Right _ -> failwith "not a program")

let parse_pattern str =
  H.wrap_parser
    (fun () -> Tree_sitter_html.Parse.string str)
    (fun cst _extras ->
      let file = Fpath.v "<pattern>" in
      let env = { H.file; conv = H.line_col_to_pos_pattern str; extra = () } in

      match map_document env cst with
      | Left xs -> (
          match xs with
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
      | Right xattr -> G.XmlAt xattr)
