(*s: pfff/lang_GENERIC/parsing/Parse_generic.ml *)
(*s: pad/r2c copyright *)
(* Yoann Padioleau
 *
 * Copyright (C) 2019-2021 r2c
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
      let {Parse_info. ast; stat; _} = Parse_python.parse ~parsing_mode file in
      (* old: Resolve_python.resolve ast;
       * switched to call Naming_AST.ml in sgrep to correct def and use tagger
      *)
      Python_to_generic.program ast, stat
  | Lang.Typescript (* abusing Js parser for now here *)
  | Lang.Javascript ->
      let {Parse_info. ast; stat; _} = Parse_js.parse file in
      Js_to_generic.program ast, stat
  | Lang.JSON ->
      let ast = Parse_json.parse_program file in
      Json_to_generic.program ast, Parse_info.correct_stat file
  | Lang.C ->
      (* this internally uses the CST for c++ *)
      let {Parse_info. ast; stat; _} = Parse_c.parse file in
      C_to_generic.program ast, stat
  | Lang.Cplusplus ->
      failwith "TODO"
  | Lang.Java ->
      let {Parse_info. ast; stat; _} = Parse_java.parse file in
      Java_to_generic.program ast, stat
  | Lang.Go ->
      let {Parse_info. ast; stat; _} = Parse_go.parse file in
      (* old: Resolve_go.resolve ast;
       * switched to call Naming_AST.ml in sgrep to correct def and use tagger
      *)
      Go_to_generic.program ast, stat
  | Lang.OCaml ->
      let {Parse_info. ast; stat; _} = Parse_ml.parse file in
      Ml_to_generic.program ast, stat
  | Lang.Ruby ->
      let {Parse_info. ast; stat; _} = Parse_ruby.parse file in
      Ruby_to_generic.program ast, stat
  | Lang.PHP ->
      let {Parse_info. ast = cst; stat; _} = Parse_php.parse file in
      let ast = Ast_php_build.program cst in
      Php_to_generic.program ast, stat
  | Lang.Csharp ->
      failwith "No C# parser in pfff; use the one in tree-sitter"
  | Lang.Kotlin ->
      failwith "No Kotlin parser in pfff; use the one in tree-sitter"
  | Lang.Lua ->
      failwith "No Lua parser in pfff; use the one in tree-sitter"
  | Lang.Rust ->
      failwith "No Rust parser in pfff; use the one in tree-sitter"
  | Lang.R ->
      failwith "No R parser in pfff; use the one in tree-sitter"
  | Lang.Yaml ->
      failwith "No Yaml parser yet, parsed only for semgrep in Parse_rule.ml"

(*e: function [[Parse_generic.parse_with_lang]] *)

(*s: function [[Parse_generic.parse_program]] *)
let parse_program file =
  match Lang.langs_of_filename file with
  | [x] -> parse_with_lang x file |> fst
  | x::_xs ->
      (* print a warning? that we default to one? *)
      parse_with_lang x file |> fst
  | [] -> failwith (spf "unsupported file for AST generic: %s" file)
(*e: function [[Parse_generic.parse_program]] *)


(*s: function [[Parse_generic.parse_pattern]] *)
let parse_pattern lang str =
  match lang with
  | Lang.Python | Lang.Python2 | Lang.Python3 ->
      let parsing_mode = lang_to_python_parsing_mode lang in
      let any = Parse_python.any_of_string ~parsing_mode str in
      Python_to_generic.any any
  (* abusing JS parser so no need extend tree-sitter grammar*)
  | Lang.Typescript
  | Lang.Javascript ->
      let any = Parse_js.any_of_string str in
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
      let any = Parse_ml.any_of_string str in
      Ml_to_generic.any any
  | Lang.Ruby ->
      let any = Parse_ruby.any_of_string str in
      Ruby_to_generic.any any
  | Lang.PHP ->
      let any_cst = Parse_php.any_of_string str in
      let any = Ast_php_build.any any_cst in
      Php_to_generic.any any
  | Lang.Csharp ->
      failwith "No C# parser in pfff; use the one in tree-sitter"
  | Lang.Kotlin ->
      failwith "No Kotlin parser in pfff; use the one in tree-sitter"
  | Lang.Cplusplus ->
      failwith "No C++ generic parser in pfff; use the one in tree-sitter"
  | Lang.Lua ->
      failwith "No Lua generic parser in pfff; use the one in tree-sitter"
  | Lang.Rust ->
      failwith "No Rust generic parser in pfff; use the one in tree-sitter"
  | Lang.R ->
      failwith "No R generic parser in pfff; use the one in tree-sitter"
  | Lang.Yaml ->
      failwith "No Yaml parser yet, parsed only for semgrep in Parse_rule.ml"
(*e: function [[Parse_generic.parse_pattern]] *)
(*e: pfff/lang_GENERIC/parsing/Parse_generic.ml *)
