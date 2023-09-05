(* Yoann Padioleau
 *
 * Copyright (C) 2019-2022 r2c
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
open File.Operators
module E = Semgrep_error_code
module Out = Semgrep_output_v1_t
module PM = Pattern_match

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* Id of a single pattern in a formula. This will be used to generate
 * mini rules with this id, and later when we evaluate the formula, find
 * the matching results corresponding to this id.
 *)
type pattern_id = Xpattern.pattern_id

(* !This hash table uses the Hashtbl.find_all property! *)
type id_to_match_results = (pattern_id, Pattern_match.t) Hashtbl.t

(* eXtended config.
 * less: we might want to get rid of equivalences at some point as
 * they are not exposed to the user anymore.
 *)
type xconfig = {
  config : Rule_options.t; (* corresponds to rule `options` key *)
  equivs : Equivalence.equivalences;
  nested_formula : bool;
  (* ^^^ i.e. we are evaluating a nested formula within `metavariable-pattern`. *)
  (* Fields coming from Runner_config.t used by the engine.
   * We could just include the whole Runner_config.t, but it's
   * cleaner to explicitely state what the engine depends on
   * (there's lots of fields in Runner_config.t).
   *)
  matching_explanations : bool;
  filter_irrelevant_rules : bool;
}

type env = {
  xconf : xconfig;
  pattern_matches : id_to_match_results;
  (* used by metavariable-pattern to recursively call evaluate_formula *)
  xtarget : Xtarget.t;
  rule : Rule.t;
  (* problems found during evaluation, one day these may be caught earlier by
   * the meta-checker *)
  errors : Report.ErrorSet.t ref;
}

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* Report errors during evaluation to the user rather than just logging them
 * as we did before. *)
let error env msg =
  (* We are not supposed to report errors in the config file for several reasons
   * (one being that it's often a temporary file anyways), so we report them on
   * the target file. *)
  let loc = Tok.first_loc_of_file !!(env.xtarget.Xtarget.file) in
  (* TODO: warning or error? MatchingError or ... ? *)
  let err =
    E.mk_error (Some (fst env.rule.Rule.id)) loc msg Out.MatchingError
  in
  env.errors := Report.ErrorSet.add err !(env.errors)

(* this will be adjusted later in range_to_pattern_match_adjusted *)
let fake_rule_id (id, str) =
  {
    PM.id = Rule_ID.of_string (string_of_int id);
    pattern_string = str;
    message = "";
    fix = None;
    languages = [];
  }

let adjust_xconfig_with_rule_options xconf options =
  let config = Common.( ||| ) options xconf.config in
  { xconf with config }

let default_xconfig =
  {
    config = Rule_options.default_config;
    equivs = [];
    nested_formula = false;
    matching_explanations = false;
    (* TODO: set to true by default?
     * Anyway it's set to true in Runner_config.default so it will default to
     * true when running as part of the regular code path (not testing code)
     *)
    filter_irrelevant_rules = false;
  }
