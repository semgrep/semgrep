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
module Res = Parsing_result2
module Flag = Flag_semgrep
module E = Core_error
module OutJ = Semgrep_output_v1_t
module Log = Log_parsing.Log

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Parsing a target, using menhir or tree-sitter parsers or both
 * depending on the language.
 *
 * update: most of the code is now in ../parsing_languages/Parse_target2.ml
 * to remove dependencies to languages/ here, to generate a smaller
 * engine.js file.
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* used by Match_search_mode and Match_tainting_mode *)
let errors_from_skipped_tokens xs =
  match xs with
  | [] -> Core_error.ErrorSet.empty
  | x :: _ ->
      let e = exn_of_loc x in
      let err = E.exn_to_error ~file:x.Tok.pos.file e in
      let locs =
        xs |> List_.map Semgrep_output_utils.location_of_token_location
      in
      Core_error.ErrorSet.singleton { err with typ = OutJ.PartialParsing locs }

let undefined_just_parse_with_lang _lang _file =
  failwith "just_parse_with_lang_ref unset"

(* TODO: factorize with Parsing_plugin mechanism
 * hack to reduce the engine.js file. Set in Parsing_init.init().
 *)
let just_parse_with_lang_ref = ref undefined_just_parse_with_lang

let just_parse_with_lang (lang : Lang.t) (file : Fpath.t) : Res.t =
  match lang with
  (* TODO: ideally this should also be in Parse_target2.ml, but we
   * still have a few dependencies to the Js parser because of
   * Parse_json.parse used in Parse_rule, so at this point
   * we can add support for JSON/JS also here. This does
   * not increase the size of the engine.js file.
   *)
  | Lang.Json ->
      run file
        [
          Pfff
            (fun file ->
              (Parse_json.parse_program file, Parsing_stat.correct_stat !!file));
        ]
        Json_to_generic.program
  | Lang.Js
    when Stdlib.( == ) !just_parse_with_lang_ref undefined_just_parse_with_lang
    ->
      (* skip tree-sitter for parsing JS if just_parse_with_lang hasn't been initialized yet *)
      run file [ Pfff (throw_tokens Parse_js.parse) ] Js_to_generic.program
  | _ -> !just_parse_with_lang_ref lang file
[@@profiling]

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let run_analyses_after_name_resolution lang ast =
  Typing.check_program lang ast;

  Implicit_return.mark_implicit_return lang ast;

  (* Flow-insensitive constant propagation. *)
  Constant_propagation.propagate_basic lang ast;

  (* Flow-sensitive constant propagation. *)
  Constant_propagation.propagate_dataflow lang ast

let just_resolve_name lang ast =
  (* to be deterministic, reset the gensym; anyway right now semgrep is
   * used only for local per-file analysis, so no need to have a unique ID
   * among a set of files in a project like codegraph.
   *)
  AST_generic.SId.unsafe_reset_counter ();
  Naming_AST.resolve lang ast;
  run_analyses_after_name_resolution lang ast

let parse_and_resolve_name lang file =
  let res = just_parse_with_lang lang file in
  let ast = res.ast in
  just_resolve_name lang ast;
  Log.info (fun m -> m "Parse_target.parse_and_resolve_name done");
  res
[@@profiling]

(* used in test files *)
let parse_and_resolve_name_warn_if_partial lang file =
  let { ast; errors; _ } : Res.t = parse_and_resolve_name lang file in
  (match errors with
  | [] -> ()
  | _ ->
      (* nosemgrep: no-logs-in-library *)
      Logs.warn (fun m -> m "fail to fully parse %s" !!file));
  ast

let fail_on_errors errors =
  match errors with
  | [] -> ()
  | errors ->
      let error_strs =
        List_.map (fun (Tree_sitter_error err : Res.error) -> err.msg) errors
      in
      failwith (String.concat "\n" error_strs)

let parse_and_resolve_name_fail_if_partial lang file =
  let { ast; errors; _ } : Res.t = parse_and_resolve_name lang file in
  fail_on_errors errors;
  ast

let parse_and_resolve_name_strict lang file =
  let { ast; errors; tolerated_errors; _ } : Res.t =
    parse_and_resolve_name lang file
  in
  fail_on_errors (errors @ tolerated_errors);
  ast

(*****************************************************************************)
(* For testing purpose *)
(*****************************************************************************)
let parse_program file =
  let lang = Lang.lang_of_filename_exn file in
  let res = just_parse_with_lang lang file in
  res.ast
