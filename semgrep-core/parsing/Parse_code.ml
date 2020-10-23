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
module PI = Parse_info

let logger = Logging.get_logger [__MODULE__]

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Mostly a wrapper around pfff Parse_generic, but which can also use
 * tree-sitter parsers when possible.
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

type 'ast parser =
 | Pfff of (Common.filename -> 'ast)
 | TreeSitter of (Common.filename ->
                   'ast option * Tree_sitter_run.Tree_sitter_error.t list)

type 'ast common_parse_result =
  | Ok of 'ast
  | Error of exn

let mk_tree_sitter_error (err : Tree_sitter_run.Tree_sitter_error.t) =
  let start = err.start_pos in
  let loc = {
    PI.str = err.substring;
    charpos = 0; (* fake *)
    line = start.row + 1;
    column = start.column;
    file = err.file.name;
  } in
  loc

let (run_parser: 'ast parser -> Common.filename -> 'ast common_parse_result) =
  fun parser file ->
  match parser with
  | Pfff f ->
     Common.save_excursion Flag_parsing.show_parsing_error false (fun () ->
       logger#info "trying to parse with Pfff parser %s" file;
       try
         Ok (f file)
       with
       | Timeout -> raise Timeout
       | exn ->
          logger#debug "exn (%s) with Pfff parser" (Common.exn_to_s exn);
          Error exn
     )
  | TreeSitter f ->
       logger#info "trying to parse with TreeSitter parser %s" file;
       (try
         let astopt, errors = f file in
         (match astopt, errors with
         | None, [] -> raise Impossible
         | Some ast, [] -> Ok ast
         | _, ts_error::_xs ->
            let loc = mk_tree_sitter_error ts_error in
            let info = { PI.token = PI.OriginTok loc; transfo = PI.NoTransfo }
            in
            Error (PI.Parsing_error info)
         )
       with
       | Timeout -> raise Timeout
       | exn ->
          logger#debug "exn (%s) with TreeSitter parser" (Common.exn_to_s exn);
          Error exn
       )

let rec (run_either: Common.filename -> 'ast parser list ->
  'ast common_parse_result)
 = fun file xs ->
   match xs with
   | [] -> Error (Failure (spf "no parser found for %s" file))
   | p::xs ->
      let res = run_parser p file in
      match res with
      | Ok ast -> Ok ast
      | Error exn ->
        let res = run_either file xs in
        (match res with
        | Ok ast -> Ok ast
        | Error exn2 ->
           logger#debug "exn again (%s) but return original exn (%s)"
                (Common.exn_to_s exn2) (Common.exn_to_s exn);
          (* prefer the first error *)
           Error exn
        )

let run file xs =
  let xs =
    if !Flag.tree_sitter_only
    then xs |> Common.exclude (function | Pfff _ -> true | _ -> false)
    else xs
   in
   (match run_either file xs with
   | Ok ast -> ast
   | Error exn -> raise exn
   )



let just_parse_with_lang lang file =
  match lang with
  | Lang.Ruby ->
      (* for Ruby we start with the tree-sitter parser because the pfff parser
       * is not great and some of the token positions may be wrong.
       *)
      let ast =
        run file [
          TreeSitter Parse_ruby_tree_sitter.parse;
          (* right now the parser is verbose and the token positions
           * may be wrong, but better than nothing. *)
          Pfff Parse_ruby.parse_program
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
          Pfff Parse_java.parse_program;
          TreeSitter Parse_java_tree_sitter.parse;
          ]
       in
       Java_to_generic.program ast
  | Lang.Go ->
      let ast =
        run file [
        Pfff Parse_go.parse_program;
        TreeSitter Parse_go_tree_sitter.parse;
        ]
      in
      Go_to_generic.program ast

  | Lang.Javascript ->
      (* we start directly with tree-sitter here, because
       * the pfff parser is slow on minified files due to its (slow) error
       * recovery strategy.
       *)
      let ast =
        run file [
          TreeSitter Parse_javascript_tree_sitter.parse;
          Pfff (fun file ->
              let f () =
                Parse_js.parse_program file
              in
              (* timeout already set in caller, then good to go *)
              if !Flag.timeout <> 0.
              then f ()
              else begin
                try
                  Common.timeout_function 5 (fun () ->
                    logger#info "running the pfff JS parser with 5s timeout";
                    f ()
                   )
                 with Timeout ->
                    logger#debug "Timeout, transforming in parse error";
                    raise Parsing.Parse_error
               end
            );
          ]
      in
      Js_to_generic.program ast

  | Lang.Typescript ->
      let ast =
        run file [
          TreeSitter (Parse_typescript_tree_sitter.parse ?dialect:None)
        ]
      in
      Js_to_generic.program ast

  | Lang.Csharp ->
      (* there is no pfff parser for C# so let's go directly to tree-sitter,
       * and there's no ast_csharp.ml either so we directly generate
       * a generic AST (no csharp_to_generic here)
       *)
      run file [TreeSitter Parse_csharp_tree_sitter.parse]

  (* default to the one in pfff for the other languages *)
  | _ ->
      run file [Pfff (Parse_generic.parse_with_lang lang)]

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
