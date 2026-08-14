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
module E = Core_error
module Out = Semgrep_output_v1_t
module PM = Core_match
module Log = Log_engine.Log

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
type id_to_match_results = (pattern_id, Core_match.t list ref) Hashtbl.t

type prefilter_policy =
  (* The policy for when we wish to prefilter (conditional on whether we are
   * doing interfile analysis or not) *)
  | CachedPrefilter of (interfile:bool -> Rule.t -> Prefiltering.File.t option)
  (* The policy for when we do not wish to prefilter at all. *)
  | NoPrefiltering

(* Build a prefilter policy whose per-rule prefilters are precomputed
   eagerly, then served read-only to the matching engine across multiple
   domains.  Produced an immutable [ROHashtbl] view; safe to share between
   threads.

   The [intrafile] table is always built (every scan does an intrafile
   matching pass).  The [interfile] table is built only when
   [~need_interfile] is true — Pro deep/interfile scans need it; OSS
   scans, Pro intrafile-only scans, and LSP search never query it, so
   we'd otherwise be paying a full per-rule prefilter compute pass for
   nothing.  If a caller queries [~interfile:true] when the table
   wasn't built, we return [None] (no prefilter, file is scanned).

   When [~par:(conf, domain_count)] is provided, the per-rule prefilter
   computation is fanned out across [domain_count] domains via
   [Concurrent.map].
*)
let make_prefilter ~(rules : Rule.t list) ?(need_interfile = false)
    ?(par : (Parallelism_config.eio_state * int) option) () =
  let mk ~interfile =
    let h = Hashtbl.create (List.length rules) in
    let compute_kv (r : Rule.t) =
      (fst r.id, Prefiltering.File.of_rule ~interfile r)
    in
    (match par with
    | None ->
        List.iter
          (fun r ->
            let k, v = compute_kv r in
            Hashtbl.replace h k v)
          rules
    | Some (conf, domain_count) ->
        (* The serial path lets exceptions propagate; the parallel path
           captures them per-rule into [Error (rule, exn)].

           Lookups for a failing rule will fail open and produce [None], which
           the matcher treats as "no prefilter needed; scan the target".
        *)
        Concurrent.map ~conf ~domain_count compute_kv rules
        |> List.iter (function
          | Ok (k, v) -> Hashtbl.replace h k v
          | Error ((r : Rule.t), exn) ->
              Log.warn (fun m ->
                  m "Prefilter creation failed for %a: %s" Rule_ID.pp (fst r.id)
                    (Printexc.to_string exn))));
    ROHashtbl.of_hashtbl h
  in
  let intrafile_table = mk ~interfile:false in
  let interfile_table =
    if need_interfile then Some (mk ~interfile:true) else None
  in
  CachedPrefilter
    (fun ~interfile (r : Rule.t) ->
      let key = fst r.id in
      match (interfile, interfile_table) with
      | true, Some t -> ROHashtbl.find_opt t key |> Option.join
      | true, None ->
          (* Caller asked for interfile but didn't request the build.
             Treat as "no prefilter": file is scanned. *)
          None
      | false, _ -> ROHashtbl.find_opt intrafile_table key |> Option.join)

(* eXtended config.*)
type xconfig = {
  config : Rule_options.t; (* corresponds to rule `options` key *)
  nested_formula : bool;
  (* ^^^ i.e. we are evaluating a nested formula within `metavariable-pattern`. *)
  (* Fields coming from Runner_config.t used by the engine.
   * We could just include the whole Runner_config.t, but it's
   * cleaner to explicitely state what the engine depends on
   * (there's lots of fields in Runner_config.t).
   *)
  matching_explanations : bool;
  filter_irrelevant_rules : prefilter_policy;
}

type env = {
  xconf : xconfig;
  pattern_matches : id_to_match_results;
  (* used by metavariable-pattern to recursively call evaluate_formula *)
  xtarget : Xtarget.t;
  rule : Rule.t;
  (* as-metavariable: This is here so we can easily pass down
     `has_as_metavariable` to `evaluate_formula`, which will dictate
     whether  we should set the `ast_node` field when focusing, as this is
     only needed for rules  making use of the `as-metavariable` feature.
  *)
  has_as_metavariable : bool;
  (* problems found during evaluation, one day these may be caught earlier by
   * the meta-checker *)
  errors : Core_error.ErrorSet.t ref;
}

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* Report errors during evaluation to the user rather than just logging them
 * as we did before. *)
let error (env : env) msg =
  (* We are not supposed to report errors in the config file for several reasons
   * (one being that it's often a temporary file anyways), so we report them on
   * the target file. *)
  let loc = Loc.first_loc_of_file env.xtarget.path.internal_path_to_content in
  (* TODO: warning or error? MatchingError or ... ? *)
  let err = E.mk_error ~rule_id:(fst env.rule.id) ~msg ~loc Out.MatchingError in
  env.errors := Core_error.ErrorSet.add err !(env.errors)

(* this will be adjusted later in range_to_pattern_match_adjusted *)
let fake_rule_id (id, str) =
  {
    PM.id = Rule_ID.of_string_exn (string_of_int id);
    pattern_string = str;
    message = "";
    metadata = None;
    fix = None;
    fix_regexp = None;
    langs = [];
  }

let adjust_xconfig_with_rule_options xconf options =
  let config = Common.( ||| ) options xconf.config in
  { xconf with config }

let default_xconfig =
  {
    config = Rule_options.default;
    nested_formula = false;
    matching_explanations = false;
    (* TODO: set to true by default?
     * Anyway it's set to true in Runner_config.default so it will default to
     * true when running as part of the regular code path (not testing code)
     *)
    filter_irrelevant_rules = NoPrefiltering;
  }
