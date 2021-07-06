(* Colleen Dai
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
 *)

[@@@warning "-32"]

(* Disable warnings against unused variables *)
[@@@warning "-26-27"]

(* Disable warning against unused 'rec' *)
[@@@warning "-39"]

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
type env = unit H.env

let _fake = AST_generic.fake

let token = H.token

let str = H.str

let fb = fake_bracket

let sc = PI.fake_info ";"

(*****************************************************************************)
(* Boilerplate converter *)
(*****************************************************************************)
(* This was started by copying tree-sitter-lang/semgrep-kotlin/Boilerplate.ml *)

(**
   Boilerplate to be used as a template when mapping the vue CST
   to another type of tree.
*)

let todo (env : env) _ = failwith "not implemented"

let map_end_tag_name (env : env) (tok : CST.end_tag_name) = token env tok

(* end_tag_name *)

let map_pat_98d585a (env : env) (tok : CST.pat_98d585a) = token env tok

(* pattern "[^\"]+" *)

let map_interpolation_text (env : env) (tok : CST.interpolation_text) =
  token env tok

(* interpolation_text *)

let map_text_fragment (env : env) (tok : CST.text_fragment) = token env tok

(* text_fragment *)

let map_attribute_name (env : env) (tok : CST.attribute_name) = token env tok

(* pattern "[^<>\"'=/\\s]+" *)

let map_attribute_value (env : env) (tok : CST.attribute_value) = token env tok

(* pattern "[^<>\"'=\\s]+" *)

let map_script_start_tag_name (env : env) (tok : CST.script_start_tag_name) =
  token env tok

(* script_start_tag_name *)

let map_pat_58fbb2e (env : env) (tok : CST.pat_58fbb2e) = token env tok

(* pattern "[^']+" *)

let map_directive_name (env : env) (tok : CST.directive_name) = token env tok

(* directive_name *)

let map_implicit_end_tag (env : env) (tok : CST.implicit_end_tag) =
  token env tok

(* implicit_end_tag *)

let map_start_tag_name (env : env) (tok : CST.start_tag_name) = token env tok

(* start_tag_name *)

let map_style_start_tag_name (env : env) (tok : CST.style_start_tag_name) =
  token env tok

(* style_start_tag_name *)

let map_comment (env : env) (tok : CST.comment) = token env tok

(* comment *)

let map_directive_shorthand (env : env) (tok : CST.directive_shorthand) =
  token env tok

(* directive_shorthand *)

let map_raw_text (env : env) (tok : CST.raw_text) = token env tok

(* raw_text *)

let map_template_start_tag_name (env : env) (tok : CST.template_start_tag_name)
    =
  token env tok

(* template_start_tag_name *)

let map_directive_dynamic_argument_value (env : env)
    (tok : CST.directive_dynamic_argument_value) =
  token env tok

(* pattern "[^<>\"'/=\\s\\]]+" *)

let map_directive_modifier (env : env) (tok : CST.directive_modifier) =
  token env tok

(* pattern "[^<>\"'/=\\s.]+" *)

let map_directive_argument (env : env) (tok : CST.directive_argument) =
  token env tok

(* pattern "[^<>\"'/=\\s.]+" *)

let map_erroneous_end_tag_name (env : env) (tok : CST.erroneous_end_tag_name) =
  token env tok

(* erroneous_end_tag_name *)

let map_end_tag (env : env) ((v1, v2, v3) : CST.end_tag) =
  let v1 = token env v1 (* "</" *) in
  let v2 = token env v2 (* end_tag_name *) in
  let v3 = token env v3 (* ">" *) in
  todo env (v1, v2, v3)

let map_text (env : env) (x : CST.text) =
  match x with
  | `Text_frag tok -> token env tok (* text_fragment *)
  | `LCURLLCURL tok -> token env tok

(* "{{" *)

let map_quoted_attribute_value (env : env) (x : CST.quoted_attribute_value) =
  match x with
  | `SQUOT_opt_pat_58fbb2e_SQUOT (v1, v2, v3) ->
      let v1 = token env v1 (* "'" *) in
      let v2 =
        match v2 with
        | Some tok -> token env tok (* pattern "[^']+" *)
        | None -> todo env ()
      in
      let v3 = token env v3 (* "'" *) in
      todo env (v1, v2, v3)
  | `DQUOT_opt_pat_98d585a_DQUOT (v1, v2, v3) ->
      let v1 = token env v1 (* "\"" *) in
      let v2 =
        match v2 with
        | Some tok -> token env tok (* pattern "[^\"]+" *)
        | None -> todo env ()
      in
      let v3 = token env v3 (* "\"" *) in
      todo env (v1, v2, v3)

let map_directive_modifiers (env : env) (xs : CST.directive_modifiers) =
  List.map
    (fun (v1, v2) ->
      let v1 = token env v1 (* "." *) in
      let v2 = token env v2 (* pattern "[^<>\"'/=\\s.]+" *) in
      todo env (v1, v2))
    xs

let map_anon_choice_attr_value_5986531 (env : env)
    (x : CST.anon_choice_attr_value_5986531) =
  match x with
  | `Attr_value tok -> token env tok (* pattern "[^<>\"'=\\s]+" *)
  | `Quoted_attr_value x -> map_quoted_attribute_value env x

let map_anon_choice_dire_arg_b33821e (env : env)
    (x : CST.anon_choice_dire_arg_b33821e) =
  match x with
  | `Dire_arg tok -> token env tok (* pattern "[^<>\"'/=\\s.]+" *)
  | `Dire_dyna_arg (v1, v2, v3) ->
      let v1 = token env v1 (* "[" *) in
      let v2 =
        match v2 with
        | Some tok -> token env tok (* pattern "[^<>\"'/=\\s\\]]+" *)
        | None -> todo env ()
      in
      let v3 = token env v3 (* "]" *) in
      todo env (v1, v2, v3)

let map_anon_choice_attr_a1991da (env : env) (x : CST.anon_choice_attr_a1991da)
    =
  match x with
  | `Attr (v1, v2) ->
      let v1 = token env v1 (* pattern "[^<>\"'=/\\s]+" *) in
      let v2 =
        match v2 with
        | Some (v1, v2) ->
            let v1 = token env v1 (* "=" *) in
            let v2 = map_anon_choice_attr_value_5986531 env v2 in
            todo env (v1, v2)
        | None -> todo env ()
      in
      todo env (v1, v2)
  | `Dire_attr (v1, v2, v3) ->
      let v1 =
        match v1 with
        | `Dire_name_opt_COLON_choice_dire_arg (v1, v2) ->
            let v1 = token env v1 (* directive_name *) in
            let v2 =
              match v2 with
              | Some (v1, v2) ->
                  let v1 = token env v1 (* ":" *) in
                  let v2 = map_anon_choice_dire_arg_b33821e env v2 in
                  todo env (v1, v2)
              | None -> todo env ()
            in
            todo env (v1, v2)
        | `Dire_shor_choice_dire_arg (v1, v2) ->
            let v1 = token env v1 (* directive_shorthand *) in
            let v2 = map_anon_choice_dire_arg_b33821e env v2 in
            todo env (v1, v2)
      in
      let v2 =
        match v2 with
        | Some x -> map_directive_modifiers env x
        | None -> todo env ()
      in
      let v3 =
        match v3 with
        | Some (v1, v2) ->
            let v1 = token env v1 (* "=" *) in
            let v2 = map_anon_choice_attr_value_5986531 env v2 in
            todo env (v1, v2)
        | None -> todo env ()
      in
      todo env (v1, v2, v3)

let map_start_tag (env : env) ((v1, v2, v3, v4) : CST.start_tag) =
  let v1 = token env v1 (* "<" *) in
  let v2 = token env v2 (* start_tag_name *) in
  let v3 = List.map (map_anon_choice_attr_a1991da env) v3 in
  let v4 = token env v4 (* ">" *) in
  todo env (v1, v2, v3, v4)

let map_template_start_tag (env : env)
    ((v1, v2, v3, v4) : CST.template_start_tag) =
  let v1 = token env v1 (* "<" *) in
  let v2 = token env v2 (* template_start_tag_name *) in
  let v3 = List.map (map_anon_choice_attr_a1991da env) v3 in
  let v4 = token env v4 (* ">" *) in
  todo env (v1, v2, v3, v4)

let map_style_start_tag (env : env) ((v1, v2, v3, v4) : CST.style_start_tag) =
  let v1 = token env v1 (* "<" *) in
  let v2 = token env v2 (* style_start_tag_name *) in
  let v3 = List.map (map_anon_choice_attr_a1991da env) v3 in
  let v4 = token env v4 (* ">" *) in
  todo env (v1, v2, v3, v4)

let map_script_start_tag (env : env) ((v1, v2, v3, v4) : CST.script_start_tag) =
  let v1 = token env v1 (* "<" *) in
  let v2 = token env v2 (* script_start_tag_name *) in
  let v3 = List.map (map_anon_choice_attr_a1991da env) v3 in
  let v4 = token env v4 (* ">" *) in
  todo env (v1, v2, v3, v4)

let map_style_element (env : env) ((v1, v2, v3) : CST.style_element) =
  let v1 = map_style_start_tag env v1 in
  let v2 =
    match v2 with
    | Some tok -> token env tok (* raw_text *)
    | None -> todo env ()
  in
  let v3 = map_end_tag env v3 in
  todo env (v1, v2, v3)

let map_script_element (env : env) ((v1, v2, v3) : CST.script_element) =
  let v1 = map_script_start_tag env v1 in
  let v2 =
    match v2 with
    | Some tok -> token env tok (* raw_text *)
    | None -> todo env ()
  in
  let v3 = map_end_tag env v3 in
  todo env (v1, v2, v3)

let rec map_element (env : env) (x : CST.element) =
  match x with
  | `Start_tag_rep_node_choice_end_tag (v1, v2, v3) ->
      let v1 = map_start_tag env v1 in
      let v2 = List.map (map_node env) v2 in
      let v3 =
        match v3 with
        | `End_tag x -> map_end_tag env x
        | `Impl_end_tag tok -> token env tok
        (* implicit_end_tag *)
      in
      todo env (v1, v2, v3)
  | `Self_clos_tag (v1, v2, v3, v4) ->
      let v1 = token env v1 (* "<" *) in
      let v2 = token env v2 (* start_tag_name *) in
      let v3 = List.map (map_anon_choice_attr_a1991da env) v3 in
      let v4 = token env v4 (* "/>" *) in
      todo env (v1, v2, v3, v4)

and map_node (env : env) (x : CST.node) =
  match x with
  | `Comm tok -> token env tok (* comment *)
  | `Text x -> map_text env x
  | `Interp (v1, v2, v3) ->
      let v1 = token env v1 (* "{{" *) in
      let v2 =
        match v2 with
        | Some tok -> token env tok (* interpolation_text *)
        | None -> todo env ()
      in
      let v3 = token env v3 (* "}}" *) in
      todo env (v1, v2, v3)
  | `Elem x -> map_element env x
  | `Temp_elem x -> map_template_element env x
  | `Script_elem x -> map_script_element env x
  | `Style_elem x -> map_style_element env x
  | `Errons_end_tag (v1, v2, v3) ->
      let v1 = token env v1 (* "</" *) in
      let v2 = token env v2 (* erroneous_end_tag_name *) in
      let v3 = token env v3 (* ">" *) in
      todo env (v1, v2, v3)

and map_template_element (env : env) ((v1, v2, v3) : CST.template_element) =
  let v1 = map_template_start_tag env v1 in
  let v2 = List.map (map_node env) v2 in
  let v3 = map_end_tag env v3 in
  todo env (v1, v2, v3)

let map_component (env : env) (xs : CST.component) =
  List.map
    (fun x ->
      match x with
      | `Comm tok -> token env tok (* comment *)
      | `Elem x -> map_element env x
      | `Temp_elem x -> map_template_element env x
      | `Script_elem x -> map_script_element env x
      | `Style_elem x -> map_style_element env x)
    xs

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)
let parse file =
  H.wrap_parser
    (fun () ->
      Parallel.backtrace_when_exn := false;
      Parallel.invoke Tree_sitter_vue.Parse.file file ())
    (fun cst ->
      let env = { H.file; conv = H.line_col_to_pos file; extra = () } in

      try
        let xs = map_component env cst in
        failwith "TODO"
      with Failure "not implemented" as exn ->
        let s = Printexc.get_backtrace () in
        pr2 "Some constructs are not handled yet";
        pr2 "CST was:";
        CST.dump_tree cst;
        pr2 "Original backtrace:";
        pr2 s;
        raise exn)
