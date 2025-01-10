(* Matthew McQuaid
 *
 * Copyright (C) 2024 Semgrep Inc.
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

module R = Rule
module Out = Semgrep_output_v1_t
module Log = Log_engine.Log

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* SCA matching code for both parity rules (lockfile only findings)
 * and reachable rules.
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* TODO? could move to SCA_match.ml *)
type dependency_match_table = (Rule_ID.t, SCA_match.t list) Hashtbl.t

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

type cmp = [ `EQ | `GT | `LT ]

let compare_version_core (c1 : SCA_version.core) (c2 : SCA_version.core) =
  let cmp n m : cmp = if n > m then `GT else if n < m then `LT else `EQ in
  let rec check = function
    | (i, j) :: is -> (
        match cmp i j with
        | `EQ -> check is
        | `GT -> `GT
        | `LT -> `LT)
    | [] -> `EQ
  in
  (c1.major, c2.major) :: (c1.minor, c2.minor)
  :: Common2.zip c1.incrementals c2.incrementals
  |> check

let check_constraint SCA_pattern.{ version; op } v' =
  match (version, v') with
  | SCA_version.V v, SCA_version.V v' -> (
      match op with
      | SCA_pattern.Eq -> compare_version_core v' v = `EQ
      | Gte -> compare_version_core v' v <> `LT
      | Lte -> compare_version_core v' v <> `GT
      | Gt -> compare_version_core v' v = `GT
      | Lt -> compare_version_core v' v = `LT)
  | _ -> false

let match_dependency_pattern (deps : SCA_dependency.t list)
    (pat : SCA_pattern.t) : SCA_match.t list =
  deps
  |> List_.filter_map (fun (dep : SCA_dependency.t) ->
         if
           String.equal dep.package_name pat.package_name
           && pat.version_constraints |> fun (SCA_pattern.SCA_And cs) ->
              cs
              |> List.for_all (fun constr ->
                     let res = check_constraint constr dep.package_version in
                     Log.debug (fun m ->
                         m "match for %s on %s = %b (package = %s)"
                           (SCA_pattern.show_version_constraint constr)
                           (SCA_version.show dep.package_version)
                           res dep.package_name);
                     res)
           (* the kind can be adjusted later in annotate_pattern_match *)
         then Some SCA_match.{ dep; pat; kind = LockfileOnlyMatch }
         else None)

(* Return the set of dependency/pattern pairs that matched *)
let match_dependency_formula :
    Lockfile_xtarget.t -> Rule.sca_dependency_formula -> SCA_match.t list =
 fun { lazy_dependencies; _ } ->
  List.concat_map (fun pat ->
      match_dependency_pattern (Lazy.force lazy_dependencies) pat)

let match_dependencies lockfile_target rule =
  match rule.Rule.dependency_formula with
  | Some f -> Some (match_dependency_formula lockfile_target f)
  | _ -> None

let match_all_dependencies lockfile_target =
  List_.map (fun rule -> (rule, match_dependencies lockfile_target rule))

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

let check_rule (rule : Rule.t) (xtarget : Lockfile_xtarget.t)
    (dependency_formula : Rule.sca_dependency_formula) :
    Core_profiling.rule_profiling Core_result.match_result =
  let _, parse_time =
    Common.with_time (fun () -> Lazy.force xtarget.lazy_dependencies)
  in
  let matches, match_time =
    Common.with_time (fun () ->
        match_dependency_formula xtarget dependency_formula)
  in
  Log.info (fun m -> m "found %d lockfile matches" (List.length matches));
  let matches =
    matches
    |> List_.map (fun (sca_match : SCA_match.t) ->
           let dep = sca_match.dep in
           Core_match.
             {
               rule_id =
                 {
                   id = fst rule.R.id;
                   message = rule.R.message;
                   metadata = rule.R.metadata;
                   fix = rule.R.fix;
                   fix_regexp = rule.R.fix_regexp;
                   langs = Analyzer.to_langs rule.R.target_analyzer;
                   (* TODO: What should this be? *)
                   pattern_string = "";
                 };
               path = Target.path_of_origin (Origin.File xtarget.target.path);
               (* TODO: should be pro if the pro engine is used in the match *)
               engine_of_match = `OSS;
               range_loc = dep.loc;
               (* TODO? *)
               ast_node = None;
               tokens = dep.tokens;
               env = [];
               taint_trace = None;
               (* TODO: What if I have a secrets rule with a dependency pattern *)
               validation_state = `No_validator;
               severity_override = None;
               metadata_override = None;
               sca_match = Some sca_match;
               fix_text = None;
               facts = [];
             })
  in
  Core_result.mk_match_result matches Core_error.ErrorSet.empty
    {
      Core_profiling.rule_parse_time = parse_time;
      rule_match_time = match_time;
      rule_id = fst rule.R.id;
    }

let annotate_pattern_match (dep_matches : SCA_match.t list option)
    (pm : Core_match.t) : Core_match.t list =
  match dep_matches with
  | None -> [ pm ]
  | Some dep_matches ->
      (* If there are two, transitive copies of a library, and no direct copies,
       * and it's used in code, we produce TWO reachable matches *)
      dep_matches
      |> List_.filter_map (fun (dm : SCA_match.t) ->
             (* TODO: Make this not quadratic
                If the match is on a transitive dep and there's also a match on
                a direct copy of the dep, then do not include it, only use the
                direct one this is what the python code does
             *)
             if
               SCA_dependency.(
                 Out.equal_transitivity dm.dep.transitivity `Transitive)
               && dep_matches
                  |> List.exists (fun ({ dep; _ } : SCA_match.t) ->
                         SCA_dependency.(
                           Out.equal_transitivity dep.transitivity `Direct
                           && String.equal dep.package_name dm.dep.package_name))
             then None
             else
               Some
                 {
                   pm with
                   Core_match.sca_match =
                     Some { dm with kind = SCA_match.CodeAndLockfileMatch };
                 })
