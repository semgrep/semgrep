(* Yoann Padioleau
 *
 * Copyright (C) 2019-2023 r2c
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
open File.Operators
open Pfff_or_tree_sitter

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Most of the code here used to be in Parse_target.ml, but was moved
 * to make the engine/ language independent so that we can generate
 * a smaller engine.js file.
 *
 * TODO: at some point maybe leverage Parsing_plugin instead of
 * modifying refs as currently done in Parsing_init.ml
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let lang_to_python_parsing_mode = function
  | Lang.Python -> Parse_python.Python
  | Lang.Python2 -> Parse_python.Python2
  | Lang.Python3 -> Parse_python.Python3
  | s -> failwith (spf "not a python language:%s" (Lang.to_string s))

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let just_parse_with_lang lang file =
  if lang =*= Lang.C && Sys.file_exists !!(!Flag_parsing_cpp.macros_h) then
    Parse_cpp.init_defs !Flag_parsing_cpp.macros_h;

  match lang with
  | Lang.Ruby ->
      (* for Ruby we start with the tree-sitter parser because the pfff parser
       * is not great and some of the token positions may be wrong.
       *)
      run file
        [
          TreeSitter Parse_ruby_tree_sitter.parse;
          (* right now the parser is verbose and the token positions
           * may be wrong, but better than nothing. *)
          Pfff (throw_tokens Parse_ruby.parse);
        ]
        Ruby_to_generic.program
  | Lang.Java ->
      run file
        [
          (* we used to start with the pfff one; it was quite good and faster
           * than tree-sitter (because we need to wrap tree-sitter inside
           * an invoke because of a segfault/memory-leak), but when both parsers
           * fail, it's better to give the tree-sitter parsing error now.
           *)
          TreeSitter Parse_java_tree_sitter.parse;
          Pfff (throw_tokens Parse_java.parse);
        ]
        Java_to_generic.program
  | Lang.Go ->
      run file
        [
          TreeSitter Parse_go_tree_sitter.parse;
          Pfff (throw_tokens Parse_go.parse);
        ]
        Go_to_generic.program
  | Lang.Js ->
      (* we start directly with tree-sitter here, because
       * the pfff parser is slow on minified files due to its (slow) error
       * recovery strategy.
       *)
      run file
        [
          TreeSitter (Parse_typescript_tree_sitter.parse ~dialect:`TSX);
          Pfff (throw_tokens Parse_js.parse);
        ]
        Js_to_generic.program
  | Lang.Ts ->
      run file
        [ TreeSitter (Parse_typescript_tree_sitter.parse ?dialect:None) ]
        Js_to_generic.program
  (* there is no pfff parsers for C#/Kotlin/... so let's go directly to
   *  tree-sitter, and there's no ast_xxx.ml either so we directly generate
   * a generic AST (no xxx_to_generic here)
   *)
  | Lang.Csharp ->
      run file [ TreeSitter Parse_csharp_tree_sitter.parse ] (fun x -> x)
  | Lang.Kotlin ->
      run file [ TreeSitter Parse_kotlin_tree_sitter.parse ] (fun x -> x)
  | Lang.Solidity ->
      run file [ TreeSitter Parse_solidity_tree_sitter.parse ] (fun x -> x)
  | Lang.Swift ->
      run file [ TreeSitter Parse_swift_tree_sitter.parse ] (fun x -> x)
  | Lang.Elixir ->
      run file [ TreeSitter Parse_elixir_tree_sitter.parse ] (fun x -> x)
  | Lang.Dart ->
      run file [ TreeSitter Parse_dart_tree_sitter.parse ] (fun x -> x)
  | Lang.Julia ->
      run file [ TreeSitter Parse_julia_tree_sitter.parse ] (fun x -> x)
  | Lang.Lua -> run file [ TreeSitter Parse_lua_tree_sitter.parse ] (fun x -> x)
  | Lang.Bash ->
      run file
        [ TreeSitter Parse_bash_tree_sitter.parse ]
        Bash_to_generic.program
  | Lang.Dockerfile ->
      run file
        [ TreeSitter Parse_dockerfile_tree_sitter.parse ]
        Dockerfile_to_generic.program
  | Lang.Rust ->
      run file [ TreeSitter Parse_rust_tree_sitter.parse ] (fun x -> x)
  | Lang.C ->
      run file
        [
          (* this internally uses the CST for c++ *)
          Pfff (throw_tokens (fun file -> Parse_c.parse (Fpath.v file)));
          TreeSitter Parse_c_tree_sitter.parse;
        ]
        C_to_generic.program
  (* use pfff *)
  | Lang.Python
  | Lang.Python2
  | Lang.Python3 ->
      let parsing_mode = lang_to_python_parsing_mode lang in
      run file
        [
          Pfff (throw_tokens (Parse_python.parse ~parsing_mode));
          TreeSitter Parse_python_tree_sitter.parse;
        ]
        Python_to_generic.program
  | Lang.Json ->
      run file
        [
          Pfff
            (fun file ->
              (Parse_json.parse_program file, Parsing_stat.correct_stat file));
        ]
        Json_to_generic.program
  | Lang.Jsonnet ->
      run file
        [
          TreeSitter
            (fun file -> Parse_jsonnet_tree_sitter.parse (Fpath.v file));
        ]
        Jsonnet_to_generic.program
  | Lang.Lisp
  | Lang.Scheme
  | Lang.Clojure ->
      run file [ TreeSitter Parse_clojure_tree_sitter.parse ] (fun x -> x)
  | Lang.Cpp ->
      run file
        [
          TreeSitter Parse_cpp_tree_sitter.parse;
          Pfff (throw_tokens (fun file -> Parse_cpp.parse (Fpath.v file)));
        ]
        Cpp_to_generic.program
  | Lang.Ocaml ->
      run file
        [
          Pfff (throw_tokens Parse_ml.parse);
          TreeSitter Parse_ocaml_tree_sitter.parse;
        ]
        Ml_to_generic.program
  | Lang.Scala ->
      run file
        [ Pfff (throw_tokens Parse_scala.parse) ]
        Scala_to_generic.program
  | Lang.Php ->
      run file
        [
          Pfff
            (fun file ->
              (* TODO: at some point parser_php.mly should go directly
               * to ast_php.ml and we should get rid of cst_php.ml
               *)
              let cst, stat = throw_tokens Parse_php.parse file in
              (Ast_php_build.program cst, stat));
          (* TODO: can't put TreeSitter first, because we still use Pfff
           * to parse the pattern, and there must be mismatch between the
           * AST generated by Ast_php_build and Parse_php_tree_sitter.parse.
           *)
          TreeSitter Parse_php_tree_sitter.parse;
        ]
        Php_to_generic.program
  | Lang.Hack ->
      run file [ TreeSitter Parse_hack_tree_sitter.parse ] (fun x -> x)
  | Lang.R -> run file [ TreeSitter Parse_r_tree_sitter.parse ] (fun x -> x)
  | Lang.Yaml ->
      {
        ast = Yaml_to_generic.program file;
        skipped_tokens = [];
        stat = Parsing_stat.default_stat file;
      }
  | Lang.Html
  | Lang.Xml ->
      (* less: there is an html parser in pfff too we could use as backup *)
      run file [ TreeSitter Parse_html_tree_sitter.parse ] (fun x -> x)
  | Lang.Vue ->
      let parse_embedded_js file =
        let { Parsing_result2.ast; skipped_tokens; stat = _ } =
          Parse_target.just_parse_with_lang Lang.Js file
        in
        (* TODO: pass the errors down to Parse_vue_tree_sitter.parse
         * and accumulate with other vue parse errors
         *)
        if skipped_tokens <> [] then failwith "parse error in embedded JS";
        ast
      in
      run file
        [ TreeSitter (Parse_vue_tree_sitter.parse parse_embedded_js) ]
        (fun x -> x)
  | Lang.Terraform ->
      run file
        [ TreeSitter Parse_terraform_tree_sitter.parse ]
        Terraform_to_generic.program
  | Lang.Apex ->
      (* Proprietary. The actual parser needs to register itself for
         parsing to take place. *)
      run_external_parser file Parsing_plugin.Apex.parse_target
