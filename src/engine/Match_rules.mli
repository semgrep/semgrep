(*
   Copyright (c) 2021-2025 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
(* this can be raised when timeout_threshold is set *)
exception File_timeout of Rule_ID.t list

type timeout_config = {
  timeout : float;
  threshold : int;
  sigalrm_timeout : bool;
}

(* The per-rule "boilerplate" wrapper used by [check] (and passed to
 * [*_match_tainting_mode.check_rules]): it runs each rule's computation under
 * the configured timeout, returns a dummy result carrying a Timeout error when
 * the rule times out, and raises [File_timeout] once too many rules time out.
 * Exposed so Run_taint_once can reuse it. *)
val per_rule_boilerplate_fn :
  timeout_config option ->
  Fpath.t ->
  Rule.t ->
  (unit -> Core_profiling.rule_profiling Core_result.match_result) ->
  Core_profiling.rule_profiling Core_result.match_result

(* Matches many rules against one target. This function is called from
 * Test_engine.ml, Test_subcommand.ml, and of course Core_scan.ml
 * (and also Match_extract_mode.ml now).
 *
 * Return matches, errors, and match time.
 *
 * This will run the search-mode and taint-mode rules.
 * !This can also raise File_timeout!
 *)
val check :
  matches_hook:(Core_match.t list -> Core_match.t list) ->
  timeout:timeout_config option ->
  Match_env.xconfig ->
  Rule.rules ->
  Xtarget.t ->
  Core_result.matches_single_file

(* for osemgrep interactive *)
val group_relevant_rules :
  Rule.t list ->
  Match_env.xconfig ->
  Xtarget.t ->
  Core_quick_profiling.t ->
  [ `Relevant of Rule.t list ]
  * [ `Irrelevant of Rule.t list ]
  * Core_quick_profiling.t
