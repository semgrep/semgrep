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
open Parsing_result2
module Flag = Flag_semgrep
module E = Semgrep_error_code
module Out = Output_from_core_t
module OutH = Output_from_core_util

let logger = Logging.get_logger [ __MODULE__ ]

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
  | [] -> Report.ErrorSet.empty
  | x :: _ ->
      let e = exn_of_loc x in
      let err = E.exn_to_error x.Tok.pos.file e in
      let locs = xs |> Common.map OutH.location_of_token_location in
      Report.ErrorSet.singleton { err with typ = Out.PartialParsing locs }

let undefined_just_parse_with_lang _lang _file =
  failwith "just_parse_with_lang_ref unset"

(* TODO: factorize with Parsing_plugin mechanism
 * hack to reduce the engine.js file. Set in Parsing_init.init().
 *)
let just_parse_with_lang_ref = ref undefined_just_parse_with_lang

let just_parse_with_lang lang file =
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
              (Parse_json.parse_program file, Parsing_stat.correct_stat file));
        ]
        Json_to_generic.program
  | Lang.Js
    when Stdlib.( == ) !just_parse_with_lang_ref undefined_just_parse_with_lang
    ->
      (* we start directly with tree-sitter here, because
       * the pfff parser is slow on minified files due to its (slow) error
       * recovery strategy.
       *)
      run file
        [
          (* adding TreeSitter adds 400K in engine.js (30k in .js.br) *)
          TreeSitter (Parse_typescript_tree_sitter.parse ~dialect:`TSX);
          Pfff (throw_tokens Parse_js.parse);
        ]
        Js_to_generic.program
  | Lang.Ts
    when Stdlib.( == ) !just_parse_with_lang_ref undefined_just_parse_with_lang
    ->
      run file
        [ TreeSitter (Parse_typescript_tree_sitter.parse ?dialect:None) ]
        Js_to_generic.program
  | _else_ -> !just_parse_with_lang_ref lang file
  [@@profiling]

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let parse_and_resolve_name lang file =
  let res = just_parse_with_lang lang file in
  let ast = res.ast in
  (* to be deterministic, reset the gensym; anyway right now semgrep is
   * used only for local per-file analysis, so no need to have a unique ID
   * among a set of files in a project like codegraph.
   *)
  AST_generic.SId.unsafe_reset_counter ();
  Naming_AST.resolve lang ast;
  Constant_propagation.propagate_basic lang ast;
  Constant_propagation.propagate_dataflow lang ast;
  logger#info "Parse_target.parse_and_resolve_name done";
  res
  [@@profiling]

(* used in test files *)
let parse_and_resolve_name_warn_if_partial lang file =
  let { ast; skipped_tokens; _ } = parse_and_resolve_name lang file in
  if skipped_tokens <> [] (* nosemgrep *) then
    pr2 (spf "WARNING: fail to fully parse %s" file);
  ast

let parse_and_resolve_name_fail_if_partial lang file =
  let { ast; skipped_tokens; _ } = parse_and_resolve_name lang file in
  if skipped_tokens <> [] then failwith (spf "fail to fully parse %s" file);
  ast

(*****************************************************************************)
(* For testing purpose *)
(*****************************************************************************)
let parse_program file =
  let file = Fpath.v file in
  let lang = Lang.lang_of_filename_exn file in
  let res = just_parse_with_lang lang !!file in
  res.ast
