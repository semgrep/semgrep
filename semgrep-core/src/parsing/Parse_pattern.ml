(*s: semgrep/parsing/Parse_pattern.ml *)
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
(* Mostly a wrapper around pfff Parse_generic.
*)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let dump_and_print_errors dumper (res : 'a Tree_sitter_run.Parsing_result.t) =
  ( match res.program with
  | Some cst -> dumper cst
  | None -> failwith "unknown error from tree-sitter parser" );
  res.errors
  |> List.iter (fun err ->
         pr2 (Tree_sitter_run.Tree_sitter_error.to_string ~color:true err))

let extract_pattern_from_tree_sitter_result
    (res : 'a Tree_sitter_run.Parsing_result.t) (print_errors : bool) =
  match (res.Tree_sitter_run.Parsing_result.program, res.errors) with
  | None, _ -> failwith "no pattern found"
  | Some x, [] -> x
  | Some _, _ :: _ ->
      if print_errors then
        res.errors
        |> List.iter (fun err ->
               pr2 (Tree_sitter_run.Tree_sitter_error.to_string ~color:true err));
      failwith "error parsing the pattern"

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)
let parse_pattern lang ?(print_errors = false) str =
  let any =
    match lang with
    | Lang.Csharp ->
        let res = Parse_csharp_tree_sitter.parse_pattern str in
        extract_pattern_from_tree_sitter_result res print_errors
    | Lang.Lua ->
        let res = Parse_lua_tree_sitter.parse_pattern str in
        extract_pattern_from_tree_sitter_result res print_errors
    | Lang.Rust ->
        let res = Parse_rust_tree_sitter.parse_pattern str in
        extract_pattern_from_tree_sitter_result res print_errors
    | Lang.Kotlin ->
        let res = Parse_kotlin_tree_sitter.parse_pattern str in
        extract_pattern_from_tree_sitter_result res print_errors
    (* use pfff *)
    | Lang.Python | Lang.Python2 | Lang.Python3 ->
        let parsing_mode = Parse_target.lang_to_python_parsing_mode lang in
        let any = Parse_python.any_of_string ~parsing_mode str in
        Python_to_generic.any any
    (* abusing JS parser so no need extend tree-sitter grammar*)
    | Lang.Typescript | Lang.Javascript ->
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
    | Lang.Scala ->
        let any = Parse_scala.any_of_string str in
        Scala_to_generic.any any
    | Lang.Ruby ->
        let any = Parse_ruby.any_of_string str in
        Ruby_to_generic.any any
    | Lang.PHP ->
        let any_cst = Parse_php.any_of_string str in
        let any =
          Common.save_excursion Flag_parsing.sgrep_mode true (fun () ->
              Ast_php_build.any any_cst)
        in
        Php_to_generic.any any
    | Lang.Hack ->
        let any = Parse_hack_tree_sitter.any_of_string `Pattern str in
        Php_to_generic.any any
    | Lang.Cplusplus -> failwith "No C++ generic parser yet"
    | Lang.R -> failwith "No R generic parser yet"
    | Lang.Yaml -> Yaml_to_generic.any str
  in

  Caching.prepare_pattern any;
  Check_pattern.check lang any;
  any

let dump_tree_sitter_pattern_cst lang file =
  match lang with
  | Lang.Csharp ->
      Tree_sitter_c_sharp.Parse.file file
      |> dump_and_print_errors Tree_sitter_c_sharp.CST.dump_tree
  | Lang.Lua ->
      Tree_sitter_lua.Parse.file file
      |> dump_and_print_errors Tree_sitter_lua.CST.dump_tree
  | Lang.Rust ->
      Tree_sitter_rust.Parse.file file
      |> dump_and_print_errors Tree_sitter_rust.CST.dump_tree
  | Lang.Kotlin ->
      Tree_sitter_kotlin.Parse.file file
      |> dump_and_print_errors Tree_sitter_kotlin.CST.dump_tree
  | _ -> ()

(*e: semgrep/parsing/Parse_pattern.ml *)
