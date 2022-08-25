(* Yoann Padioleau
 *
 * Copyright (C) 2019-2022 r2c
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file license.txt.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * license.txt for more details.
 *)
module E = Semgrep_error_code
module PI = Parse_info
module Out = Output_from_core_t
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
  config : Config_semgrep.t;
  equivs : Equivalence.equivalences;
  (* field(s) coming from Runner_config.t used by the engine *)
  matching_explanations : bool;
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
  let loc = PI.first_loc_of_file env.xtarget.Xtarget.file in
  (* TODO: warning or error? MatchingError or ... ? *)
  let err =
    E.mk_error ~rule_id:(Some (fst env.rule.Rule.id)) loc msg Out.MatchingError
  in
  env.errors := Report.ErrorSet.add err !(env.errors)

(* this will be adjusted later in range_to_pattern_match_adjusted *)
let fake_rule_id (id, str) =
  { PM.id = string_of_int id; pattern_string = str; message = "" }

let adjust_xconfig_with_rule_options xconf options =
  let config = Common.( ||| ) options xconf.config in
  { xconf with config }
