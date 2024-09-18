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
open Common
open Fpath_.Operators
module Flag = Flag_semgrep
module Log = Log_parsing.Log

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
  | Pfff of (Fpath.t -> 'ast * Parsing_stat.t)
  | TreeSitter of (Fpath.t -> ('ast, unit) Tree_sitter_run.Parsing_result.t)

(*
   This type is parametrized by the AST type because we don't always
   generate directly generic ASTs. We sometimes generate intermediate
   ASTs hence the need for polymorphic type (so you can have
   Ast_cpp.program internal_result, or Ast_php.program
   internal_result).
*)
type 'ast internal_result =
  | ResOk of
      (* Some errors are tolerated. We need them for tests. *)
      ('ast * Parsing_stat.t * Tree_sitter_run.Tree_sitter_error.t list)
  | ResPartial of
      ('ast * Parsing_stat.t * Tree_sitter_run.Tree_sitter_error.t list)
  | ResError of Exception.t

(* TODO: factorize with previous type *)
type 'ast pattern_parser =
  | PfffPat of (string -> 'ast)
  | TreeSitterPat of (string -> ('ast, unit) Tree_sitter_run.Parsing_result.t)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let exn_of_loc loc =
  let info = Tok.OriginTok loc in
  Parsing_error.Syntax_error info |> Exception.trace

(* used by Parse_jsonnet *)
let error_of_tree_sitter_error (err : Tree_sitter_run.Tree_sitter_error.t) =
  let loc = Parsing_result2.loc_of_tree_sitter_error err in
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

let dump_and_print_errors dumper
    (res : ('a, _) Tree_sitter_run.Parsing_result.t) =
  (match res.program with
  | Some cst -> dumper cst
  | None -> failwith "unknown error from tree-sitter parser");
  res.errors
  |> List.iter (fun err ->
         Log.err (fun m ->
             m "%s"
               (Tree_sitter_run.Tree_sitter_error.to_string ~style:Auto err)))

(*
   Serious error = any parsing error that causes us to resort to an
   alternate parser. Missing nodes aren't considered serious enough to
   warrant another parsing attempt. Otherwise, doing so would result
   in slowdowns due to the other parser being usually slower
   (observed: 3 s -> 30 s parsing time on big JS file when retrying
   due to missing/inserted tokens and falling back to the menhir
   parser).
*)
let is_serious_error (err : Tree_sitter_run.Tree_sitter_error.t) =
  match err.kind with
  | Internal
  | Error_node ->
      true
  | Missing_node -> false

let has_serious_error (res : _ Tree_sitter_run.Parsing_result.t) =
  List.exists is_serious_error res.errors

(* Return the first serious error of the list to show as the reason
   for failure. *)
let get_serious_error (res : _ Tree_sitter_run.Parsing_result.t) =
  List.find_opt (fun err -> is_serious_error err) res.errors

let extract_pattern_from_tree_sitter_result
    (res : ('a, unit) Tree_sitter_run.Parsing_result.t) =
  match res.program with
  | None -> failwith "no pattern found"
  | Some pat ->
      (* TODO: treat missing tokens as errors once we're confident that
         these new errors won't affect users negatively on a large scale. *)
      if has_serious_error res then (
        res.errors
        |> List.iter (fun err ->
               Log.err (fun m ->
                   m "%s"
                     (Tree_sitter_run.Tree_sitter_error.to_string ~style:Auto
                        err)));
        (* to be backward compatible with what we do in PfffPat *)
        raise Parsing.Parse_error)
      else pat

(*****************************************************************************)
(* Run target parsers *)
(*****************************************************************************)

let (run_parser : 'ast parser -> Fpath.t -> 'ast internal_result) =
 fun parser file ->
  match parser with
  | Pfff f ->
      Common.save_excursion Flag_parsing.show_parsing_error false (fun () ->
          Log.info (fun m -> m "trying to parse with Pfff parser %s" !!file);
          try
            let ast, stat = f file in
            ResOk (ast, stat, [])
          with
          | Time_limit.Timeout _ as e -> Exception.catch_and_reraise e
          | exn ->
              let e = Exception.catch exn in
              (* TODO: print where the exception was raised or reraise *)
              Log.warn (fun m ->
                  m "exn (%s) with Pfff parser" (Common.exn_to_s exn));
              ResError e)
  | TreeSitter f -> (
      Log.info (fun m -> m "trying to parse with TreeSitter parser %s" !!file);
      try
        let res = f file in
        let stat = stat_of_tree_sitter_stat !!file res.stat in
        match (res.program, get_serious_error res) with
        | None, None ->
            let msg =
              "internal error: failed to recover typed tree from tree-sitter's \
               untyped tree"
            in
            ResError (Exception.trace (Failure msg))
        | Some ast, None -> ResOk (ast, stat, res.errors)
        | None, Some ts_error ->
            let e = error_of_tree_sitter_error ts_error in
            Log.err (fun m ->
                m "non-recoverable error with TreeSitter parser:\n%s"
                  (Exception.to_string e));
            ResError e
        | Some ast, Some _error ->
            (* Note that the first error is probably the most important;
             * the following one may be due to cascading effects *)
            Log.warn (fun m ->
                m "partial errors (%d) with TreeSitter parser"
                  (List.length res.errors));
            ResPartial (ast, stat, res.errors)
      with
      | Time_limit.Timeout _ as e -> Exception.catch_and_reraise e
      (* to get correct stack trace on parse error *)
      | exn when !debug_exn -> Exception.catch_and_reraise exn
      | exn ->
          let e = Exception.catch exn in
          Log.err (fun m ->
              m "exn (%s) with TreeSitter parser" (Common.exn_to_s exn));
          ResError e)

let rec (run_either : Fpath.t -> 'ast parser list -> 'ast internal_result) =
 fun file xs ->
  match xs with
  | [] ->
      ResError (Exception.trace (Failure (spf "no parser found for %s" !!file)))
  | p :: xs -> (
      let res = run_parser p file in
      match res with
      | ResOk ast -> ResOk ast
      | ResPartial _ as partial -> (
          let res = run_either file xs in
          match res with
          | ResOk res -> ResOk res
          | ResError e2 ->
              Log.debug (fun m ->
                  m "exn again but return Partial:\n%s" (Exception.to_string e2));
              (* prefer a Partial to an Error *)
              partial
          | ResPartial _ ->
              Log.debug (fun m -> m "Partial again but return first Partial");
              partial)
      | ResError e1 -> (
          let res = run_either file xs in
          match res with
          | ResOk res -> ResOk res
          | ResPartial _ as partial ->
              Log.debug (fun m ->
                  m "Got now a Partial, better than exn:\n%s"
                    (Exception.to_string e1));
              partial
          | ResError e2 ->
              Log.debug (fun m ->
                  m
                    "exn again but return original exn:\n\
                     --- new exn (ignored) ---\n\
                     %s\n\
                     --- original exn (retained) ---\n\
                     %s"
                    (Exception.to_string e2) (Exception.to_string e1));
              (* prefer the first error *)
              ResError e1))

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let (run :
      Fpath.t ->
      'ast parser list ->
      ('ast -> AST_generic.program) ->
      Parsing_result2.t) =
 fun file xs fconvert ->
  let xs =
    match () with
    | () when !Flag.tree_sitter_only ->
        xs
        |> List_.exclude (function
             | Pfff _ -> true
             | TreeSitter _ -> false)
    | () when !Flag.pfff_only ->
        xs
        |> List_.exclude (function
             | TreeSitter _ -> true
             | Pfff _ -> false)
    | () -> xs
  in
  match run_either file xs with
  | ResOk (ast, stat, tolerable_errors) ->
      Parsing_result2.ok (fconvert ast) stat tolerable_errors
  | ResPartial (ast, stat, errors) ->
      Parsing_result2.partial (fconvert ast) stat errors
  | ResError e -> Exception.reraise e

(*****************************************************************************)
(* Similar to run, but for pattern parsing *)
(*****************************************************************************)

let run_parser_pat p str =
  let parse () =
    match p with
    | PfffPat f ->
        Log.info (fun m -> m "trying to parse with Pfff parser the pattern");
        f str
    | TreeSitterPat f ->
        Log.info (fun m ->
            m "trying to parse with Tree-sitter parser the pattern");
        let res = f str in
        extract_pattern_from_tree_sitter_result res
  in
  try Ok (parse ()) with
  | Time_limit.Timeout _ as e -> Exception.catch_and_reraise e
  | exn -> Error (Exception.catch exn)

(* This is a simplified version of run_either. We don't need most of the
 * logic there when we're parsing patterns, so it doesn't make sense
 * to reuse it.
 *)
let run_pattern parsers program =
  let rec f parsers =
    match parsers with
    | [] ->
        Error
          (Exception.trace
             (Failure "internal error: No pattern parser available"))
    | p :: xs -> (
        match run_parser_pat p program with
        | Ok res -> Stdlib.Ok res
        | Error e -> (
            match f xs with
            | Ok res -> Stdlib.Ok res
            | Error _ ->
                (* Return the error from the first parser. *)
                Error e))
  in
  match f parsers with
  | Ok res -> res
  | Error e -> Exception.reraise e

(*****************************************************************************)
(* Other helpers *)
(*****************************************************************************)

(* Simplified version of 'run' that allows for plugins to hide the
   intermediate AST type. *)
let run_external_parser (file : Fpath.t)
    (parse :
      Fpath.t -> (AST_generic.program, unit) Tree_sitter_run.Parsing_result.t) :
    Parsing_result2.t =
  run file [ TreeSitter parse ] (fun ast -> ast)

let throw_tokens f file =
  let res = f file in
  (res.Parsing_result.ast, res.Parsing_result.stat)
