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
module E = Error_code

let logger = Logging.get_logger [__MODULE__]

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Mostly a wrapper around pfff Parse_generic, but which can also use
 * tree-sitter parsers when possible.
*)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type parsing_result = {
  ast: AST_generic.program;
  errors: Error_code.error list;
  stat: Parse_info.parsing_stat;
}

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

type 'ast parser =
  | Pfff of (Common.filename -> ('ast * Parse_info.parsing_stat))
  | TreeSitter of (Common.filename ->
                   'ast Tree_sitter_run.Parsing_result.t)

type 'ast internal_result =
  | Ok of ('ast * Parse_info.parsing_stat)
  | Partial of 'ast * Error_code.error list * Parse_info.parsing_stat
  | Error of exn

let error_of_tree_sitter_error (err : Tree_sitter_run.Tree_sitter_error.t) =
  let start = err.start_pos in
  let loc = {
    PI.str = err.substring;
    charpos = 0; (* fake *)
    line = start.row + 1;
    column = start.column;
    file = err.file.name;
  } in
  let info = { PI.token = PI.OriginTok loc; transfo = PI.NoTransfo } in
  PI.Parsing_error info

let stat_of_tree_sitter_stat file (stat: Tree_sitter_run.Parsing_result.stat) =
  { Parse_info.
    filename = file;
    total_line_count = stat.total_line_count;
    error_line_count = stat.error_line_count;
    have_timeout = false;
    commentized = 0; problematic_lines = [];
  }


let (run_parser: 'ast parser -> Common.filename -> 'ast internal_result) =
  fun parser file ->
  match parser with
  | Pfff f ->
      Common.save_excursion Flag_parsing.show_parsing_error false (fun () ->
        logger#info "trying to parse with Pfff parser %s" file;
        try
          let res = f file in
          Ok res
        with
        | Timeout -> raise Timeout
        | exn ->
            logger#debug "exn (%s) with Pfff parser" (Common.exn_to_s exn);
            Error exn
      )
  | TreeSitter f ->
      logger#info "trying to parse with TreeSitter parser %s" file;
      (try
         let res = f file in
         let stat = stat_of_tree_sitter_stat file res.stat in
         (match res.program, res.errors with
          | None, [] -> raise Impossible
          | Some ast, [] -> Ok (ast, stat)
          | None, ts_error::_xs ->
              let exn = error_of_tree_sitter_error ts_error in
              logger#info "non-recoverable error (%s) with TreeSitter parser"
                (Common.exn_to_s exn);
              Error exn
          | Some ast, x::_xs ->
              (* let's just return the first one for now; the following one
               * may be due to cascading effect of the first error *)
              let exn = error_of_tree_sitter_error x in
              logger#info "partial error (%s) with TreeSitter parser"
                (Common.exn_to_s exn);
              let err = E.exn_to_error file exn in
              Partial (ast, [err], stat)
         )
       with
       | Timeout -> raise Timeout
       | exn ->
           logger#debug "exn (%s) with TreeSitter parser" (Common.exn_to_s exn);
           Error exn
      )

let rec (run_either:
           Common.filename -> 'ast parser list -> 'ast internal_result)
  = fun file xs ->
    match xs with
    | [] -> Error (Failure (spf "no parser found for %s" file))
    | p::xs ->
        let res = run_parser p file in
        match res with
        | Ok ast -> Ok ast
        | Partial (ast, errs, stat) ->
            let res = run_either file xs in
            (match res with
             | Ok res -> Ok res
             | Error exn2 ->
                 logger#debug "exn again (%s) but return Partial"
                   (Common.exn_to_s exn2);
                 (* prefer a Partial to an Error *)
                 Partial (ast, errs, stat)
             | Partial _ ->
                 logger#debug "Partial again but return first Partial";
                 Partial (ast, errs, stat)
            )
        | Error exn ->
            let res = run_either file xs in
            (match res with
             | Ok res -> Ok res
             | Partial (ast, errs, stat) ->
                 logger#debug "Got now a Partial, better than exn (%s)"
                   (Common.exn_to_s exn);
                 Partial (ast, errs, stat)

             | Error exn2 ->
                 logger#debug "exn again (%s) but return original exn (%s)"
                   (Common.exn_to_s exn2) (Common.exn_to_s exn);
                 (* prefer the first error *)
                 Error exn
            )

let (run: Common.filename -> 'ast parser list -> ('ast -> AST_generic.program)
     -> parsing_result) =
  fun file xs fconvert ->
  let xs =
    if !Flag.tree_sitter_only
    then xs |> Common.exclude (function | Pfff _ -> true | _ -> false)
    else xs
  in
  (match run_either file xs with
   | Ok (ast, stat) -> { ast = fconvert ast; errors =  []; stat }
   | Partial (ast, errs, stat) -> { ast = fconvert ast; errors = errs; stat }
   | Error exn -> raise exn
  )

let throw_tokens f =
  (fun file ->
     let res = f file in
     res.PI.ast, res.PI.stat
  )

let just_parse_with_lang lang file =
  match lang with
  | Lang.Ruby ->
      (* for Ruby we start with the tree-sitter parser because the pfff parser
       * is not great and some of the token positions may be wrong.
      *)
      run file [
        TreeSitter Parse_ruby_tree_sitter.parse;
        (* right now the parser is verbose and the token positions
         * may be wrong, but better than nothing. *)
        Pfff (throw_tokens Parse_ruby.parse);
      ]
        Ruby_to_generic.program
  | Lang.Java ->
      (* let's start with a pfff one; it's quite good and currently faster
       * than the tree-sitter one because we need to wrap that one inside
       * an invoke because of a segfault/memory-leak.
      *)
      run file [
        Pfff (throw_tokens Parse_java.parse);
        TreeSitter Parse_java_tree_sitter.parse;
      ]
        Java_to_generic.program
  | Lang.Go ->
      run file [
        Pfff (throw_tokens Parse_go.parse);
        TreeSitter Parse_go_tree_sitter.parse;
      ]
        Go_to_generic.program

  | Lang.Javascript ->
      (* we start directly with tree-sitter here, because
       * the pfff parser is slow on minified files due to its (slow) error
       * recovery strategy.
      *)
      run file [
        TreeSitter Parse_javascript_tree_sitter.parse;
        Pfff (throw_tokens (fun file ->
          let f () =
            Parse_js.parse file
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
        ));
      ]
        Js_to_generic.program

  | Lang.Typescript ->
      run file [
        TreeSitter (Parse_typescript_tree_sitter.parse ?dialect:None)
      ]
        Js_to_generic.program

  | Lang.Csharp ->
      (* there is no pfff parser for C# so let's go directly to tree-sitter,
       * and there's no ast_csharp.ml either so we directly generate
       * a generic AST (no csharp_to_generic here)
      *)
      run file [TreeSitter Parse_csharp_tree_sitter.parse] (fun x -> x)

  | Lang.Kotlin ->
      (* there is no pfff parser for Kotlin so let's go directly to tree-sitter,
       * and there's no ast_kotlin.ml either so we directly generate
       * a generic AST (no kotlin_to_generic here)
      *)
      run file [TreeSitter Parse_kotlin_tree_sitter.parse] (fun x -> x)

  | Lang.C ->
      run file [
        Pfff (throw_tokens Parse_c.parse);
        TreeSitter Parse_c_tree_sitter.parse;
      ]
        C_to_generic.program

  (* default to the one in pfff for the other languages *)
  | _ ->
      run file [Pfff ((fun file ->
        Parse_generic.parse_with_lang lang file))] (fun x -> x)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let parse_and_resolve_name_use_pfff_or_treesitter lang file =
  let { ast; errors; stat } = just_parse_with_lang lang file in

  (* to be deterministic, reset the gensym; anyway right now semgrep is
   * used only for local per-file analysis, so no need to have a unique ID
   * among a set of files in a project like codegraph.
  *)
  AST_generic_helpers.gensym_counter := 0;
  Naming_AST.resolve lang ast;
  Constant_propagation.propagate lang ast;
  { ast; errors; stat }
