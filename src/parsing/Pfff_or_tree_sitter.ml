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
module Flag = Flag_semgrep

let logger = Logging.get_logger [ __MODULE__ ]

(* To get a better backtrace, to better debug parse errors *)
let debug_exn = ref false

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Run pfff-based or tree-sitter-based parsers, or both, with
 * error recovery between the two to get the best results.
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* TODO: switch to Fpath.t *)
type 'ast parser =
  | Pfff of (Common.filename -> 'ast * Parsing_stat.t)
  | TreeSitter of (Common.filename -> 'ast Tree_sitter_run.Parsing_result.t)

type 'ast internal_result =
  | Ok of ('ast * Parsing_stat.t)
  | Partial of 'ast * Tok.location list * Parsing_stat.t
  | Error of Exception.t

(* TODO: factorize with previous type *)
type 'ast pattern_parser =
  | PfffPat of (string -> 'ast)
  | TreeSitterPat of (string -> 'ast Tree_sitter_run.Parsing_result.t)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let loc_of_tree_sitter_error (err : Tree_sitter_run.Tree_sitter_error.t) =
  let start = err.start_pos in
  {
    Tok.str = err.substring;
    pos =
      {
        charpos = 0;
        (* fake *)
        line = start.row + 1;
        column = start.column;
        file = err.file.name;
      };
  }

let exn_of_loc loc =
  let info = Tok.OriginTok loc in
  Parsing_error.Syntax_error info |> Exception.trace

(* used by Parse_jsonnet *)
let error_of_tree_sitter_error (err : Tree_sitter_run.Tree_sitter_error.t) =
  let loc = loc_of_tree_sitter_error err in
  exn_of_loc loc

let stat_of_tree_sitter_stat file (stat : Tree_sitter_run.Parsing_result.stat) =
  {
    Parsing_stat.filename = file;
    total_line_count = stat.total_line_count;
    error_line_count = stat.error_line_count;
    have_timeout = false;
    commentized = 0;
    problematic_lines = [];
    ast_stat = None;
  }

let dump_and_print_errors dumper (res : 'a Tree_sitter_run.Parsing_result.t) =
  (match res.program with
  | Some cst -> dumper cst
  | None -> failwith "unknown error from tree-sitter parser");
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
(* Run target parsers *)
(*****************************************************************************)

let (run_parser : 'ast parser -> Common.filename -> 'ast internal_result) =
 fun parser file ->
  match parser with
  | Pfff f ->
      Common.save_excursion Flag_parsing.show_parsing_error false (fun () ->
          logger#trace "trying to parse with Pfff parser %s" file;
          try
            let res = f file in
            Ok res
          with
          | Time_limit.Timeout _ as e -> Exception.catch_and_reraise e
          | exn ->
              let e = Exception.catch exn in
              (* TODO: print where the exception was raised or reraise *)
              logger#error "exn (%s) with Pfff parser" (Common.exn_to_s exn);
              Error e)
  | TreeSitter f -> (
      logger#trace "trying to parse with TreeSitter parser %s" file;
      try
        let res = f file in
        let stat = stat_of_tree_sitter_stat file res.stat in
        match (res.program, res.errors) with
        | None, [] ->
            let msg =
              "internal error: failed to recover typed tree from tree-sitter's \
               untyped tree"
            in
            Error (Exception.trace (Failure msg))
        | Some ast, [] -> Ok (ast, stat)
        | None, ts_error :: _xs ->
            let e = error_of_tree_sitter_error ts_error in
            logger#error "non-recoverable error with TreeSitter parser:\n%s"
              (Exception.to_string e);
            Error e
        | Some ast, x :: xs ->
            (* Note that the first error is probably the most important;
             * the following one may be due to cascading effects *)
            logger#error "partial errors (%d) with TreeSitter parser"
              (List.length (x :: xs));
            let locs = x :: xs |> Common.map loc_of_tree_sitter_error in
            Partial (ast, locs, stat)
      with
      | Time_limit.Timeout _ as e -> Exception.catch_and_reraise e
      (* to get correct stack trace on parse error *)
      | exn when !debug_exn -> Exception.catch_and_reraise exn
      | exn ->
          let e = Exception.catch exn in
          logger#error "exn (%s) with TreeSitter parser" (Common.exn_to_s exn);
          Error e)

let rec (run_either :
          Common.filename -> 'ast parser list -> 'ast internal_result) =
 fun file xs ->
  match xs with
  | [] -> Error (Exception.trace (Failure (spf "no parser found for %s" file)))
  | p :: xs -> (
      let res = run_parser p file in
      match res with
      | Ok ast -> Ok ast
      | Partial (ast, errs, stat) -> (
          let res = run_either file xs in
          match res with
          | Ok res -> Ok res
          | Error e2 ->
              logger#debug "exn again but return Partial:\n%s"
                (Exception.to_string e2);
              (* prefer a Partial to an Error *)
              Partial (ast, errs, stat)
          | Partial _ ->
              logger#debug "Partial again but return first Partial";
              Partial (ast, errs, stat))
      | Error e1 -> (
          let res = run_either file xs in
          match res with
          | Ok res -> Ok res
          | Partial (ast, errs, stat) ->
              logger#debug "Got now a Partial, better than exn:\n%s"
                (Exception.to_string e1);
              Partial (ast, errs, stat)
          | Error e2 ->
              logger#debug
                "exn again but return original exn:\n\
                 --- new exn (ignored) ---\n\
                 %s\n\
                 --- original exn (retained) ---\n\
                 %s"
                (Exception.to_string e2) (Exception.to_string e1);
              (* prefer the first error *)
              Error e1))

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let (run :
      Common.filename ->
      'ast parser list ->
      ('ast -> AST_generic.program) ->
      Parsing_result2.t) =
 fun file xs fconvert ->
  let xs =
    match () with
    | () when !Flag.tree_sitter_only ->
        xs
        |> Common.exclude (function
             | Pfff _ -> true
             | TreeSitter _ -> false)
    | () when !Flag.pfff_only ->
        xs
        |> Common.exclude (function
             | TreeSitter _ -> true
             | Pfff _ -> false)
    | () -> xs
  in
  match run_either file xs with
  | Ok (ast, stat) -> { ast = fconvert ast; skipped_tokens = []; stat }
  | Partial (ast, skipped_tokens, stat) ->
      { ast = fconvert ast; skipped_tokens; stat }
  | Error e -> Exception.reraise e

(*****************************************************************************)
(* Similar to run, but for pattern parsing *)
(*****************************************************************************)

let run_parser_pat ~print_errors p str =
  let parse () =
    match p with
    | PfffPat f ->
        logger#trace "trying to parse with Pfff parser the pattern";
        f str
    | TreeSitterPat f ->
        logger#trace "trying to parse with Tree-sitter parser the pattern";
        let res = f str in
        extract_pattern_from_tree_sitter_result res print_errors
  in
  try Stdlib.Ok (parse ()) with
  | Time_limit.Timeout _ as e -> Exception.catch_and_reraise e
  | exn -> Error (Exception.catch exn)

(* This is a simplified version of run_either. We don't need most of the
 * logic there when we're parsing patterns, so it doesn't make sense
 * to reuse it.
 *)
let run_pattern ~print_errors parsers program =
  let rec f parsers =
    match parsers with
    | [] ->
        Stdlib.Error
          (Exception.trace
             (Failure "internal error: No pattern parser available"))
    | p :: xs -> (
        match run_parser_pat ~print_errors p program with
        | Stdlib.Ok res -> Stdlib.Ok res
        | Stdlib.Error e -> (
            match f xs with
            | Stdlib.Ok res -> Stdlib.Ok res
            | Stdlib.Error _ ->
                (* Return the error from the first parser. *)
                Stdlib.Error e))
  in
  match f parsers with
  | Stdlib.Ok res -> res
  | Stdlib.Error e -> Exception.reraise e

(*****************************************************************************)
(* Other helpers *)
(*****************************************************************************)

(* Simplified version of 'run' that allows for plugins to hide the
   intermediate AST type. *)
let run_external_parser file
    (parse :
      Common.filename -> AST_generic.program Tree_sitter_run.Parsing_result.t) :
    Parsing_result2.t =
  run file [ TreeSitter parse ] (fun ast -> ast)

let throw_tokens f file =
  let res = f file in
  (res.Parsing_result.ast, res.Parsing_result.stat)
