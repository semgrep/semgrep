(* Yoann Padioleau
 *
 * Copyright (C) 2020 r2c
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
module Flag = Flag_semgrep

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Mostly a wrapper around pfff Parse_generic but which can also use
 * tree-sitter parsers if needed.
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let unless_tree_sitter f file =
  if !Flag.force_tree_sitter
  then failwith "Forced tree sitter"
  else Common.save_excursion Flag_parsing.show_parsing_error false (fun () ->
       f file
    )

let just_parse_with_lang lang file =
  match lang with
  | Lang.Ruby ->
      (* for Ruby we start with the tree-sitter parser because the pfff parser
       * is not great and some of the token positions may be wrong.
       *)
      let ast =
        try
          Parse_ruby_tree_sitter.parse file
        with _exn ->
          (* right now the parser is verbose and the token positions
           * may be wrong, but better than nothing.
           *)
          Parse_ruby.parse_program file
      in
      Ruby_to_generic.program ast
  | Lang.Java ->
      let ast =
        (* let's start with a pfff one; it's quite good and currently faster
         * than the tree-sitter one because we need to wrap that one inside
         * an invoke because of a segfault/memory-leak.
         *)
        try unless_tree_sitter Parse_java.parse_program file
        with _exn -> Parse_java_tree_sitter.parse file
       in
       Java_to_generic.program ast
  | Lang.Go ->
      let ast =
        try unless_tree_sitter Parse_go.parse_program file
        with _exn -> Parse_go_tree_sitter.parse file
      in
      Go_to_generic.program ast

  | Lang.Javascript ->
      (* TODO: we should start directly with tree-sitter here, because
       * the pfff parser is slow on minified fiels due to its (slow) error
       * recovery strategy.
       *)
      let ast =
        try unless_tree_sitter
            (fun file ->
              let cst = Parse_js.parse_program file in
              Ast_js_build.program cst
            )
            file
        with _exn -> Parse_javascript_tree_sitter.parse file
      in
      Js_to_generic.program ast

  | Lang.Csharp ->
      (* there is no pfff parser for C# so let's go directly to tree-sitter,
       * and there's no ast_csharp.ml either so we directly generate
       * a generic AST (no csharp_to_generic here)
       *)
      Parse_csharp_tree_sitter.parse file

  (* default to the one in pfff for the other languages *)
  | _ -> Parse_generic.parse_with_lang lang file

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let parse_and_resolve_name_use_pfff_or_treesitter lang file =
  let ast = just_parse_with_lang lang file in

  (* to be deterministic, reset the gensym; anyway right now semgrep is
   * used only for local per-file analysis, so no need to have a unique ID
   * among a set of files in a project like codegraph.
   *)
  AST_generic.gensym_counter := 0;
  Naming_AST.resolve lang ast;
  Constant_propagation.propagate lang ast;
  ast
