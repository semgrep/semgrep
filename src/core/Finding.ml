(* Yoann Padioleau
 *
 * Copyright (C) 2019-2024 Semgrep Inc.
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

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Type to represent a finding.
 *
 * old: used to be all part of Pattern_match.t
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* The locations of variables which taint propagates through *)
type tainted_tokens = Tok.t list [@@deriving show, eq]

(* The tokens associated with a single pattern match involved in a taint trace *)
type pattern_match_tokens = Tok.t list [@@deriving show, eq]

(* Simplified version of Taint.source_to_sink meant for finding reporting *)
type taint_call_trace =
  (* A direct match *)
  | Toks of pattern_match_tokens
  (* An indirect match through a function call *)
  | Call of {
      call_toks : pattern_match_tokens;
      intermediate_vars : tainted_tokens;
      call_trace : taint_call_trace;
    }
[@@deriving show, eq]

(* The trace of a single source of taint, to the sink.
   There may be many of these, taking different paths. For a single
   sink, the fact that it produces a finding might be the product of
   many taints, due to labels.
   These taints may also take their own paths, because they might arrive
   via different variables.
*)
type taint_trace_item = {
  source_trace : taint_call_trace;
      (** This is the path that the taint takes, from the source, to get to
        the current function in which the taint finding is reported. *)
  tokens : tainted_tokens;
      (** This is the path taken within the current function, to link the
        taint source obtained earlier with a sink. Both of these might
        be done through a chain of function calls. *)
  sink_trace : taint_call_trace;
      (** This is the path that the taint takes, from the function context,
        to get to the sink. *)
}
[@@deriving show, eq]

type taint_trace = taint_trace_item list [@@deriving show, eq]

(* ! main type ! *)
type t = {
  pm : Pattern_match.t;
  (* Indicates whether this match was produced during a run
   * of Semgrep PRO. This will be overrided later by the Pro engine, on any
   * matches which are produced from a Pro run.
   * TODO? do we want to consider the same match but with different engine
   * as separate matches? or better make them equal for dedup purpose?
   *)
  engine_of_match : Engine_kind.engine_of_finding; [@equal fun _a _b -> true]
  (* Lazy since construction involves forcing lazy token lists. *)
  (* We used to have `[@equal fun _a _b -> true]` here, but this causes issues with
     multiple findings to the same sink (but different sources) being removed
     in deduplication.
     We now rely on equality of taint traces, which in turn relies on equality of `Parse_info.t`.
  *)
  taint_trace : taint_trace Lazy.t option; (* secrets stuff *)
  (* Indicates whether a postprocessor ran and validated this result. *)
  validation_state : Rule.validation_state;
  (* Indicates if the rule default severity should be modified to a different
     severity. Currently this is just used by secrets validators in order to
     modify severity based on information from the validation step. (E.g.,
     validity, scope information) *)
  severity_override : Rule.severity option;
  (* Indicates if the rule default metadata should be modified. Currently this
     is just used by secrets validators in order to
     modify metadata based on information from the validation step. (E.g.,
     validity, scope information)
     NOTE: The whole metadata blob is _not_ changed; rather, fields present in
     the override is applied on top of the default and only changes the fields
     present in the override. *)
  metadata_override : JSON.t option;
  dependency : dependency option;
}

and dependency =
  (* Rule had both code patterns and dependency patterns, got matches on *both*, the Pattern Match is in code, annotated with this dependency match *)
  | CodeAndLockfileMatch of dependency_match
  (* Rule had dependency patterns, they matched, the Pattern Match is in a lockfile *)
  (* So the range_loc of the Dependency.t in this dependency_match should be *the same* as the range_loc in the PatternMatch.t *)
  | LockfileOnlyMatch of dependency_match

and dependency_match = Dependency.t * Rule.dependency_pattern
[@@deriving show, eq]

(*****************************************************************************)
(* API *)
(*****************************************************************************)

let of_pm pm =
  {
    pm;
    taint_trace = None;
    engine_of_match = `OSS;
    validation_state = `No_validator;
    severity_override = None;
    metadata_override = None;
    dependency = None;
  }

(* Deduplicate findings *)
let uniq (findings : t list) : t list =
  let eq = AST_generic_equals.with_structural_equal equal in
  let tbl = Hashtbl.create 1_024 in
  findings
  |> List.iter (fun finding ->
         let loc = finding.pm.range_loc in
         let matches_at_loc = Hashtbl_.get_stack tbl loc in
         match
           List.find_opt (fun finding' -> eq finding finding') matches_at_loc
         with
         | Some _equal_match_at_loc -> ()
         | None -> Hashtbl_.push tbl loc finding);
  Hashtbl.fold (fun _loc stack acc -> List.rev_append !stack acc) tbl []
[@@profiling]

let range finding = Pattern_match.range finding.pm

(* Is [pm1] a submatch of [pm2] ? *)
let submatch finding1 finding2 = Pattern_match.submatch finding1.pm finding2.pm

(* Remove matches that are srictly inside another match. *)
let no_submatches findings =
  let tbl = Hashtbl.create 1_024 in
  findings
  |> List.iter (fun finding ->
         (* This is mainly for removing taint-tracking duplicates and
          * there should not be too many matches per file; but if perf
          * is a problem, consider using a specialized data structure. *)
         let k =
           (finding.pm.rule_id, finding.pm.path.internal_path_to_content)
         in
         match Hashtbl.find_opt tbl k with
         | None -> Hashtbl.add tbl k [ finding ]
         | Some ys -> (
             match List.find_opt (fun y -> submatch finding y) ys with
             | Some _ -> ()
             | None ->
                 let ys' = List.filter (fun y -> not (submatch y finding)) ys in
                 Hashtbl.replace tbl k (finding :: ys')));
  tbl |> Hashtbl.to_seq_values |> Seq.flat_map List.to_seq |> List.of_seq
[@@profiling]

let to_proprietary finding = { finding with engine_of_match = `PRO }
