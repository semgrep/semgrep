(* Yoann Padioleau
 *
 * Copyright (C) 2019-2023 Semgrep Inc.
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
open Pfff_or_tree_sitter

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Most of the code here used to be in Parse_pattern.ml, but was moved
 * here to make the engine/ language-independent so that we can generate
 * a smaller engine.js file.
 *)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)
let parse_pattern options lang str =
  (* coupling: update the files semgrep/js/languages/<lang>/Parser.ml
     when updating this function.
     TODO: Share the logic of which parser to try for each language to
     remove this coupling. https://github.com/semgrep/semgrep/issues/8331
  *)
  match lang with
  (* use adhoc parser (neither menhir nor tree-sitter) *)
  | Lang.Yaml -> Yaml_to_generic.any str
  | Lang.Scala ->
      let any = Parse_scala.any_of_string str in
      Scala_to_generic.any any
  (* Use menhir and tree-sitter *)
  | Lang.Go ->
      let any = Parse_go.any_of_string str in
      Go_to_generic.any any
  | Lang.Php ->
      let any_cst = Parse_php.any_of_string str in
      let any = Ast_php_build.any any_cst in
      Php_to_generic.any any
  | Lang.Ocaml ->
      let any = Parse_ml.any_of_string str in
      Ocaml_to_generic.any any
  | Lang.Python
  | Lang.Python2
  | Lang.Python3 ->
      let any =
        str
        |> run_pattern
             (* coupling: semgrep/js/languages/python/Parser.ml *)
             [
               PfffPat
                 (let parsing_mode =
                    Parse_target2.lang_to_python_parsing_mode lang
                  in
                  Parse_python.any_of_string ~parsing_mode);
               TreeSitterPat Parse_python_tree_sitter.parse_pattern;
             ]
      in

      Python_to_generic.any any
  (* Use menhir and tree-sitter *)
  | Lang.C
  | Lang.Cpp ->
      let any =
        str
        |> run_pattern
             (* coupling: semgrep/js/languages/cpp/Parser.ml *)
             [
               PfffPat
                 (fun x -> Parse_cpp.any_of_string Flag_parsing_cpp.Cplusplus x);
               TreeSitterPat Parse_cpp_tree_sitter.parse_pattern;
             ]
      in
      Cpp_to_generic.any
        ?parsing_opt:
          (Option.map (fun x -> x.Rule_options_t.cpp_parsing_pref) options)
        any
  | Lang.Java ->
      let any =
        str
        |> run_pattern
             (* coupling: semgrep/js/languages/java/Parser.ml *)
             [
               (* TODO: we should switch to TreeSitterPat first, but
                * we get regressions on generic_args.sgrep because
                * typed metavariables are not parsed correctly then
                *)
               PfffPat Parse_java.any_of_string;
               TreeSitterPat Parse_java_tree_sitter.parse_pattern;
             ]
      in
      Java_to_generic.any any
  (* abusing JS parser so no need extend tree-sitter grammar*)
  | Lang.Ts
  | Lang.Js
  | Lang.Vue ->
      let js_ast =
        str
        |> run_pattern
             (* coupling: semgrep/js/languages/typescript/Parser.ml *)
             [
               PfffPat Parse_js.any_of_string;
               TreeSitterPat Parse_typescript_tree_sitter.parse_pattern;
             ]
      in
      Js_to_generic.any js_ast
  | Lang.Json ->
      let any = Parse_json.any_of_string str in
      Json_to_generic.any any
  (* Tree-sitter only and use intermediate AST *)
  | Lang.Bash ->
      let res = Parse_bash_tree_sitter.parse_pattern str in
      let program = extract_pattern_from_tree_sitter_result res in
      Bash_to_generic.any program
  | Lang.Jsonnet ->
      let res = Parse_jsonnet_tree_sitter.parse_pattern str in
      let pattern = extract_pattern_from_tree_sitter_result res in
      Jsonnet_to_generic.any pattern
  | Lang.Terraform ->
      let res = Parse_terraform_tree_sitter.parse_pattern str in
      let pattern = extract_pattern_from_tree_sitter_result res in
      Terraform_to_generic.any pattern
  | Lang.Ql ->
      let res = Parse_ql_tree_sitter.parse_pattern str in
      let pattern = extract_pattern_from_tree_sitter_result res in
      QL_to_generic.any pattern
  (* Tree-sitter only and directly to generic AST *)
  | Lang.Csharp ->
      let parse_pattern =
        if Parsing_plugin.Csharp.is_available () then
          Parsing_plugin.Csharp.parse_pattern
        else Parse_csharp_tree_sitter.parse_pattern
      in
      let res = parse_pattern str in
      extract_pattern_from_tree_sitter_result res
  | Lang.Cairo ->
      let res = Parse_cairo_tree_sitter.parse_pattern str in
      extract_pattern_from_tree_sitter_result res
  | Lang.Dart ->
      let res = Parse_dart_tree_sitter.parse_pattern str in
      extract_pattern_from_tree_sitter_result res
  | Lang.Dockerfile ->
      let res = Parse_dockerfile_tree_sitter.parse_docker_or_bash_pattern str in
      extract_pattern_from_tree_sitter_result res
  | Lang.Hack ->
      let res = Parse_hack_tree_sitter.parse_pattern str in
      extract_pattern_from_tree_sitter_result res
  | Lang.Html
  | Lang.Xml ->
      let res = Parse_html_tree_sitter.parse_pattern str in
      extract_pattern_from_tree_sitter_result res
  | Lang.Lisp
  | Lang.Scheme
  | Lang.Clojure ->
      let res = Parse_clojure_tree_sitter.parse_pattern str in
      extract_pattern_from_tree_sitter_result res
  | Lang.Lua ->
      let res = Parse_lua_tree_sitter.parse_pattern str in
      extract_pattern_from_tree_sitter_result res
  | Lang.Promql ->
      let res = Parse_promql_tree_sitter.parse_pattern str in
      extract_pattern_from_tree_sitter_result res
  | Lang.Protobuf ->
      let res = Parse_protobuf_tree_sitter.parse_pattern str in
      extract_pattern_from_tree_sitter_result res
  | Lang.Rust ->
      let res = Parse_rust_tree_sitter.parse_pattern str in
      extract_pattern_from_tree_sitter_result res
  | Lang.Kotlin ->
      let res = Parse_kotlin_tree_sitter.parse_pattern str in
      extract_pattern_from_tree_sitter_result res
  | Lang.Julia ->
      let res = Parse_julia_tree_sitter.parse_pattern str in
      extract_pattern_from_tree_sitter_result res
  | Lang.Ruby ->
      let res = Parse_ruby_tree_sitter.parse_pattern str in
      let program = extract_pattern_from_tree_sitter_result res in
      Ruby_to_generic.any program
  | Lang.R ->
      let res = Parse_r_tree_sitter.parse_pattern str in
      extract_pattern_from_tree_sitter_result res
  | Lang.Solidity ->
      let res = Parse_solidity_tree_sitter.parse_pattern str in
      extract_pattern_from_tree_sitter_result res
  | Lang.Swift ->
      let res = Parse_swift_tree_sitter.parse_pattern str in
      extract_pattern_from_tree_sitter_result res
  (* external plugins *)
  | Lang.Apex ->
      let res = Parsing_plugin.Apex.parse_pattern str in
      extract_pattern_from_tree_sitter_result res
  | Lang.Elixir ->
      let res = Parsing_plugin.Elixir.parse_pattern str in
      extract_pattern_from_tree_sitter_result res
  | Lang.Move_on_sui ->
      let res = Parse_move_on_sui_tree_sitter.parse_pattern str in
      extract_pattern_from_tree_sitter_result res
  | Lang.Move_on_aptos ->
      let res = Parse_move_on_aptos_tree_sitter.parse_pattern str in
      extract_pattern_from_tree_sitter_result res
  | Lang.Circom ->
      let res = Parse_circom_tree_sitter.parse_pattern str in
      extract_pattern_from_tree_sitter_result res
(* TODO *)

let dump_tree_sitter_pattern_cst (lang : Lang.t) (path : Fpath.t) : unit =
  let file = !!path in
  match lang with
  | Lang.Csharp ->
      Tree_sitter_c_sharp.Parse.file file
      |> dump_and_print_errors Tree_sitter_c_sharp.Boilerplate.dump_tree
  | Lang.Lua ->
      Tree_sitter_lua.Parse.file file
      |> dump_and_print_errors Tree_sitter_lua.Boilerplate.dump_tree
  | Lang.Rust ->
      Tree_sitter_rust.Parse.file file
      |> dump_and_print_errors Tree_sitter_rust.Boilerplate.dump_tree
  | Lang.Kotlin ->
      Tree_sitter_kotlin.Parse.file file
      |> dump_and_print_errors Tree_sitter_kotlin.Boilerplate.dump_tree
  | __else__ -> ()
