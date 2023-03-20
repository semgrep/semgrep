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
module Flag = Flag_semgrep
module PI = Parse_info
module E = Semgrep_error_code
module Out = Output_from_core_t
module OutH = Output_from_core_util

let logger = Logging.get_logger [ __MODULE__ ]

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Parsing a target, using Pfff-based or Tree-sitter-based parsers.
 *
 * update: most of the code is now in ../parsing_languages/Parse_target_bis.ml
 * to remove dependencies to languages/ here, to generate a small
 * JS file for the whole engine.
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type parsing_result = {
  ast : AST_generic.program;
  (* partial errors tree-sitter was able to recover from *)
  skipped_tokens : PI.token_location list;
  stat : Parsing_stat.t;
}

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let exn_of_loc loc =
  let info = { PI.token = PI.OriginTok loc; transfo = PI.NoTransfo } in
  PI.Parsing_error info |> Exception.trace

let loc_of_tree_sitter_error (err : Tree_sitter_run.Tree_sitter_error.t) =
  let start = err.start_pos in
  {
    PI.str = err.substring;
    charpos = 0;
    (* fake *)
    line = start.row + 1;
    column = start.column;
    file = err.file.name;
  }

(* used by Parse_jsonnet *)
let error_of_tree_sitter_error (err : Tree_sitter_run.Tree_sitter_error.t) =
  let loc = loc_of_tree_sitter_error err in
  exn_of_loc loc

let errors_from_skipped_tokens xs =
  match xs with
  | [] -> Report.ErrorSet.empty
  | x :: _ ->
      let e = exn_of_loc x in
      let err = E.exn_to_error x.PI.file e in
      let locs = xs |> Common.map OutH.location_of_token_location in
      Report.ErrorSet.singleton { err with typ = Out.PartialParsing locs }

(* TODO: factorize with Parsing_plugin mechanism *)
let just_parse_with_lang_ref =
  ref (fun _lang _file -> failwith "just_parse_with_lang_ref unset")

let just_parse_with_lang lang file = !just_parse_with_lang_ref lang file
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
  if !Flag.use_bloom_filter then Bloom_annotation.annotate_program ast;
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
  let lang = List.hd (Lang.langs_of_filename file) in
  let res = just_parse_with_lang lang !!file in
  res.ast
