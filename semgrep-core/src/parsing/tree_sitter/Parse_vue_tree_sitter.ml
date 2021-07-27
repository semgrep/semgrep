(* Yoann Padioleau
 *
 * Copyright (c) 2021 R2C
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License (GPL)
 * version 2 as published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * file license.txt for more details.
 *)
open Common
module CST = Tree_sitter_vue.CST
module H = Parse_tree_sitter_helpers
module PI = Parse_info
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
  PI.combine_infos v1 [ v2; v3 ]

let map_text (env : env) (x : CST.text) =
  match x with
  | `Text_frag tok -> str env tok (* text_fragment *)
  (* ?? not an interpolation? *)
  | `LCURLLCURL tok -> str env tok

(* "{{" *)

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

let map_directive_modifiers (env : env) (xs : CST.directive_modifiers) =
  List.map
    (fun (v1, v2) ->
      let v1 = token env v1 (* "." *) in
      let v2 = str env v2 (* pattern "[^<>\"'/=\\s.]+" *) in
      (v1, v2))
    xs

let map_anon_choice_attr_value_5986531 (env : env)
    (x : CST.anon_choice_attr_value_5986531) : xml_attr_value =
  match x with
  | `Attr_value tok ->
      let x = str env tok (* pattern "[^<>\"'=\\s]+" *) in
      N (Id (x, G.empty_id_info ()))
  | `Quoted_attr_value x ->
      let x = map_quoted_attribute_value env x in
      L (String x)

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
            let v = L (Bool (true, fake "true")) in
            (fake "=", v)
      in
      XmlAttr (id, teq, v2)
  | `Dire_attr (v1, v2, v3) ->
      let id =
        match v1 with
        | `Dire_name_opt_COLON_choice_dire_arg (v1, v2) ->
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
        match v2 with Some x -> map_directive_modifiers env x | None -> []
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
            let v = L (Bool (true, fake "true")) in
            (fake "=", v)
      in
      (* TODO: XmlDynAttr? *)
      XmlAttr (id, teq, v3)

let map_start_tag (env : env) ((v1, v2, v3, v4) : CST.start_tag) =
  let v1 = token env v1 (* "<" *) in
  let v2 = str env v2 (* start_tag_name *) in
  let v3 = List.map (map_anon_choice_attr_a1991da env) v3 in
  let v4 = token env v4 (* ">" *) in
  (v1, v2, v3, v4)

let map_template_start_tag (env : env)
    ((v1, v2, v3, v4) : CST.template_start_tag) =
  let v1 = token env v1 (* "<" *) in
  let v2 = str env v2 (* template_start_tag_name *) in
  let v3 = List.map (map_anon_choice_attr_a1991da env) v3 in
  let v4 = token env v4 (* ">" *) in
  (v1, v2, v3, v4)

let map_style_start_tag (env : env) ((v1, v2, v3, v4) : CST.style_start_tag) =
  let v1 = token env v1 (* "<" *) in
  let v2 = str env v2 (* style_start_tag_name *) in
  let v3 = List.map (map_anon_choice_attr_a1991da env) v3 in
  let v4 = token env v4 (* ">" *) in
  (v1, v2, v3, v4)

let map_script_start_tag (env : env) ((v1, v2, v3, v4) : CST.script_start_tag) =
  let v1 = token env v1 (* "<" *) in
  let v2 = str env v2 (* script_start_tag_name *) in
  let v3 = List.map (map_anon_choice_attr_a1991da env) v3 in
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
        let v = H.str_if_wrong_content_temporary_fix env tok in
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
      let v2 = List.map (map_node env) v2 |> List.flatten in
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
      let attrs = List.map (map_anon_choice_attr_a1991da env) v3 in
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
            Some (L (String x))
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
  let v2 = List.map (map_node env) v2 |> List.flatten in
  let v3 = map_end_tag env v3 in
  { xml_kind = XmlClassic (l, id, r, v3); xml_attrs = attrs; xml_body = v2 }

let map_component (env : env) (xs : CST.component) : stmt list =
  List.map
    (fun x ->
      match x with
      | `Comm tok ->
          let _x = token env tok (* comment *) in
          []
      | `Elem x ->
          let xml = map_element env x in
          [ G.exprstmt (Xml xml) ]
      | `Temp_elem x ->
          let xml = map_template_element env x in
          [ G.exprstmt (Xml xml) ]
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
          [ G.exprstmt (Xml xml) ])
    xs
  |> List.flatten

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

(* TODO: move in Parse_tree_sitter_helpers.ml *)
let parse_string_and_adjust_wrt_base content tbase fparse =
  Common2.with_tmp_file ~str:content ~ext:"js" (fun file ->
      let x = fparse file in

      let visitor =
        Map_AST.mk_visitor
          {
            Map_AST.default_visitor with
            Map_AST.kinfo =
              (fun (_, _) t ->
                let base_loc = PI.token_location_of_info tbase in
                PI.adjust_info_wrt_base base_loc t);
          }
      in
      visitor.Map_AST.vprogram x)

let parse parse_js file =
  H.wrap_parser
    (fun () ->
      Parallel.backtrace_when_exn := false;
      Parallel.invoke Tree_sitter_vue.Parse.file file ())
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

      try
        let xs = map_component env cst in
        xs
      with Failure "not implemented" as exn ->
        let s = Printexc.get_backtrace () in
        pr2 "Some constructs are not handled yet";
        pr2 "CST was:";
        CST.dump_tree cst;
        pr2 "Original backtrace:";
        pr2 s;
        raise exn)
