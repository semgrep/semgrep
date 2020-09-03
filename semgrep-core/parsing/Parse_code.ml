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
open Common
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

type parser_kind = Pfff | TreeSitter
 and 'ast parser = parser_kind * (Common.filename -> 'ast)

let rec (run_either: Common.filename -> 'ast parser list ->
 ('ast, exn) Common.either)
 = fun file xs ->
   match xs with
   | [] -> Right (Failure (spf "no parser found for %s" file))
   | (_kind, f)::xs ->
      let f file =
          if xs <> []
          then Common.save_excursion Flag_parsing.show_parsing_error false
               (fun () -> f file)
          else f file
      in
      let res =
        try Left (f file)
        with exn -> Right exn
      in
      match res with
      | Left ast -> Left ast
      | Right exn ->
        let res = run_either file xs in
        (match res with
        | Left ast -> Left ast
        (* prefer the first error *)
        | Right _exn2 -> Right exn
        )

let run file xs =
  let xs =
    if !Flag.tree_sitter_only
    then xs |> Common.exclude (fun (a, _b) -> a = Pfff)
    else xs
   in
   (match run_either file xs with
   | Left ast -> ast
   | Right exn -> raise exn
   )



let just_parse_with_lang lang file =
  match lang with
  | Lang.Ruby ->
      (* for Ruby we start with the tree-sitter parser because the pfff parser
       * is not great and some of the token positions may be wrong.
       *)
      let ast =
        run file [
          TreeSitter, Parse_ruby_tree_sitter.parse;
          (* right now the parser is verbose and the token positions
           * may be wrong, but better than nothing. *)
          Pfff, Parse_ruby.parse_program
        ]
      in
      Ruby_to_generic.program ast
  | Lang.Java ->
      let ast =
        (* let's start with a pfff one; it's quite good and currently faster
         * than the tree-sitter one because we need to wrap that one inside
         * an invoke because of a segfault/memory-leak.
         *)
        run file [
          Pfff, Parse_java.parse_program;
          TreeSitter, Parse_java_tree_sitter.parse;
          ]
       in
       Java_to_generic.program ast
  | Lang.Go ->
      let ast =
        run file [
        Pfff, Parse_go.parse_program;
        TreeSitter, Parse_go_tree_sitter.parse;
        ]
      in
      Go_to_generic.program ast

  | Lang.Javascript ->
      (* TODO: we should start directly with tree-sitter here, because
       * the pfff parser is slow on minified fiels due to its (slow) error
       * recovery strategy.
       *)
      let ast =
        run file [
          TreeSitter, Parse_javascript_tree_sitter.parse;
          Pfff, (fun file ->
              let cst = Parse_js.parse_program file in
              Ast_js_build.program cst
            );
          ]
      in
      Js_to_generic.program ast

  | Lang.Typescript ->
      let ast =
        run file [
          TreeSitter, Parse_typescript_tree_sitter.parse
        ]
      in
      Js_to_generic.program ast

  | Lang.Csharp ->
      (* there is no pfff parser for C# so let's go directly to tree-sitter,
       * and there's no ast_csharp.ml either so we directly generate
       * a generic AST (no csharp_to_generic here)
       *)
      run file [TreeSitter, Parse_csharp_tree_sitter.parse]

  (* default to the one in pfff for the other languages *)
  | _ ->
      run file [Pfff, (Parse_generic.parse_with_lang lang)]

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
