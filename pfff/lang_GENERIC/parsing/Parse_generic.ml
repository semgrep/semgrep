(*s: pfff/lang_GENERIC/parsing/Parse_generic.ml *)
(*s: pad/r2c copyright *)
(* Yoann Padioleau
 *
 * Copyright (C) 2019-2020 r2c
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
(*e: pad/r2c copyright *)
open Common

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Wrappers around many languages to transform them in a generic AST
 * (see AST_generic.ml)
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
(*s: function [[Parse_generic.lang_to_python_parsing_mode]] *)
let lang_to_python_parsing_mode = function
  | Lang.Python -> Parse_python.Python
  | Lang.Python2 -> Parse_python.Python2
  | Lang.Python3 -> Parse_python.Python3
  | s -> failwith (spf "not a python language:%s" (Lang.string_of_lang s))
(*e: function [[Parse_generic.lang_to_python_parsing_mode]] *)

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

(*s: function [[Parse_generic.parse_with_lang]] *)
let parse_with_lang lang file = 
  match lang with
  | Lang.Python | Lang.Python2 | Lang.Python3 ->
    let parsing_mode = lang_to_python_parsing_mode lang in
    let ast = Parse_python.parse_program ~parsing_mode file in
    (* old: Resolve_python.resolve ast; 
     * switched to call Naming_AST.ml in sgrep to correct def and use tagger
     *)
    Python_to_generic.program ast
  | Lang.Typescript (* abusing Js parser for now here *)
  | Lang.Javascript ->
    let cst = Parse_js.parse_program file in
    (* does also the job of a Resolve_js.resolve ast
     * but Js_to_generic does not use the tags
     *)
    let ast = Ast_js_build.program cst in
    Js_to_generic.program ast
  | Lang.JSON ->
    let ast = Parse_json.parse_program file in
    Json_to_generic.program ast
  | Lang.C ->
    (* this internally uses the CST for c++ *)
    let ast = Parse_c.parse_program file in
    C_to_generic.program ast
  | Lang.Java ->
    let ast = Parse_java.parse_program file in
    Java_to_generic.program ast
  | Lang.Go ->
    let ast = Parse_go.parse_program file in
    (* old: Resolve_go.resolve ast;
     * switched to call Naming_AST.ml in sgrep to correct def and use tagger
     *)
    Go_to_generic.program ast
  | Lang.OCaml ->
     let cst = Parse_ml.parse_program file in
     let ast = Ast_ml_build.program cst in
     Ml_to_generic.program ast
  | Lang.Ruby ->
      let ast = Parse_ruby.parse_program file in
      Ruby_to_generic.program ast
  | Lang.Csharp ->
      failwith "No C# parser in pfff; use the one in tree-sitter"
(*e: function [[Parse_generic.parse_with_lang]] *)

(*s: function [[Parse_generic.parse_program]] *)
let parse_program file =
  match Lang.langs_of_filename file with
  | [x] -> parse_with_lang x file
  | x::_xs -> 
      (* print a warning? that we default to one? *)
      parse_with_lang x file
  | [] -> failwith (spf "unsupported file for AST generic: %s" file)
(*e: function [[Parse_generic.parse_program]] *)


(*s: function [[Parse_generic.parse_pattern]] *)
let parse_pattern lang str =
  Common.save_excursion Flag_parsing.sgrep_mode true (fun () ->
  match lang with
  | Lang.Python | Lang.Python2 | Lang.Python3 ->
      let parsing_mode = lang_to_python_parsing_mode lang in
      let any = Parse_python.any_of_string ~parsing_mode str in
      Python_to_generic.any any
  (* abusing JS parser so no need extend tree-sitter grammar*)
  | Lang.Typescript 
  | Lang.Javascript ->
      let any_cst = Parse_js.any_of_string str in
      let any = Ast_js_build.any any_cst in
      Js_to_generic.any any
  | Lang.JSON ->
      let any = Parse_json.any_of_string str in
      Json_to_generic.any any
  | Lang.C ->
      let any = Parse_c.any_of_string str in
      C_to_generic.any any
  | Lang.Java ->
      let any = Parse_java.any_of_string str in
      Java_to_generic.any any
  | Lang.Go ->
      let any = Parse_go.any_of_string str in
      Go_to_generic.any any
  | Lang.OCaml -> 
      let any_cst = Parse_ml.any_of_string str in
      let any_ast = Ast_ml_build.any any_cst in
      Ml_to_generic.any any_ast
  | Lang.Ruby ->
      let any = Parse_ruby.any_of_string str in
      Ruby_to_generic.any any
  | Lang.Csharp ->
      failwith "No C# parser in pfff; use the one in tree-sitter"
  )
(*e: function [[Parse_generic.parse_pattern]] *)
(*e: pfff/lang_GENERIC/parsing/Parse_generic.ml *)
