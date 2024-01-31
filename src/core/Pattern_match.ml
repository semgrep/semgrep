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

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Type to represent a pattern match.
 *
 * old: used to be called Match_result.t
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* We use 'eq' below to possibly remove redundant equivalent matches. Indeed,
 * Generic_vs_generic sometimes return multiple times the same match,
 * sometimes because of some bugs we didn't fix, sometimes it's normal
 * because of the way '...' operate. TODO: add an example of such situation.
 *
 * Note that you should not ignore the rule id when comparing 2 matches!
 * One match can come from a pattern-not: in which case
 * even if it returns the same match than a similar match coming
 * from a pattern:, we should not merge them!
 *)

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
  (* rule (or mini rule) responsible for the pattern match found *)
  rule_id : rule_id; [@equal fun a b -> a.id = b.id]
  (* Indicates whether this match was produced during a run
   * of Semgrep PRO. This will be overrided later by the Pro engine, on any
   * matches which are produced from a Pro run.
   * TODO? do we want to consider the same match but with different engine
   * as separate matches? or better make them equal for dedup purpose?
   *)
  engine_kind : Engine_kind.t; [@equal fun _a _b -> true]
  (* location info *)
  file : Fpath.t;
  (* less: redundant with location? *)
  (* note that the two Tok.location can be equal *)
  range_loc : Tok.location * Tok.location;
  (* less: do we need to be lazy? *)
  tokens : Tok.t list Lazy.t; [@equal fun _a _b -> true]
  (* metavars for the pattern match *)
  env : Metavariable.bindings;
      [@equal
        fun a b ->
          List.equal
            (fun (s1, m1) (s2, m2) ->
              (* See the comment in Metavariable.mli for location_aware_equal_mvalue,
                 but basically we would like to consider matches different if they
                 metavariables bound to the same content, but at different locations.
              *)
              s1 = s2 && Metavariable.location_aware_equal_mvalue m1 m2)
            a b]
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
  dependency_match : dependency_match option;
}

and dependency_match = Supply_chain.dependency * Rule.dependency_pattern

(* and dependency_match = {
     dependency_range_loc : Tok.location * Tok.location;
     package_name : string;
     package_version : string;
     ecosystem : Supply_chain.ecosystem;
   } *)

(* This is currently a record, but really only the rule id should matter.
 *
 * We could derive information in the other fields from the id, but that
 * would require to pass around the list of rules to get back the
 * information. Instead, by embedding the information in the pattern match,
 * some functions are simpler (we use the same trick with Parse_info.t
 * where for example we embed the filename in it, not just a position).
 * alt: reuse Mini_rule.t
 *)
and rule_id = {
  (* This id is usually a string like 'check-double-equal'.
   * It can be the id of a rule or mini rule.
   *
   * Note that when we process a full rule, this id can temporarily
   * contain a Rule.pattern_id.
   *)
  id : Rule_ID.t;
  (* other parts of a rule (or mini_rule) used in JSON_report.ml.
   *
   * TODO should we remove these fields and just pass around a Rule.t or
   * mini_rule? *)
  message : string;
  fix : string option;
  fix_regexp : Rule.fix_regexp option;
  (* ?? why we need that? *)
  langs : Lang.t list;
  (* used for debugging (could be removed at some point) *)
  pattern_string : string;
}
[@@deriving show, eq]

(*****************************************************************************)
(* API *)
(*****************************************************************************)

(* Deduplicate matches *)
let uniq (pms : t list) : t list =
  let eq = AST_generic_equals.with_structural_equal equal in
  let tbl = Hashtbl.create 1_024 in
  pms
  |> List.iter (fun match_ ->
         let loc = match_.range_loc in
         let matches_at_loc = Hashtbl_.get_stack tbl loc in
         match
           List.find_opt (fun match2 -> eq match_ match2) matches_at_loc
         with
         | Some _equal_match_at_loc -> ()
         | None -> Hashtbl_.push tbl loc match_);
  Hashtbl.fold (fun _loc stack acc -> List.rev_append !stack acc) tbl []
[@@profiling]

let range pm =
  let start_loc, end_loc = pm.range_loc in
  Range.range_of_token_locations start_loc end_loc

(* Is [pm1] a submatch of [pm2] ? *)
let submatch pm1 pm2 =
  pm1.rule_id = pm2.rule_id && pm1.file = pm2.file
  (* THINK: && "pm1.bindings = pm2.bindings" ? *)
  && Range.( $<$ ) (range pm1) (range pm2)

(* Remove matches that are srictly inside another match. *)
let no_submatches pms =
  let tbl = Hashtbl.create 1_024 in
  pms
  |> List.iter (fun pm ->
         (* This is mainly for removing taint-tracking duplicates and
          * there should not be too many matches per file; but if perf
          * is a problem, consider using a specialized data structure. *)
         let k = (pm.rule_id, pm.file) in
         match Hashtbl.find_opt tbl k with
         | None -> Hashtbl.add tbl k [ pm ]
         | Some ys -> (
             match List.find_opt (fun y -> submatch pm y) ys with
             | Some _ -> ()
             | None ->
                 let ys' = List.filter (fun y -> not (submatch y pm)) ys in
                 Hashtbl.replace tbl k (pm :: ys')));
  tbl |> Hashtbl.to_seq_values |> Seq.flat_map List.to_seq |> List.of_seq
[@@profiling]

let to_proprietary pm = { pm with engine_kind = `PRO }

(* DEAD ?

   (* This special Set is used in the dataflow tainting code,
      which manipulates sets of matches associated to each variables.
      We only care about the metavariable environment carried by the pattern
      matches at the moment.
   *)
   module Set = Set.Make (struct
     type previous_t = t

     (* alt: use type nonrec t = t, but this causes pad's codegraph to blowup *)
     type t = previous_t

     (* If the pattern matches are obviously different (have different ranges),
        this is enough to compare them.
        If their ranges are the same, compare their metavariable environments.
        This is not robust to reordering metavariable environments.
        [("$A",e1);("$B",e2)] is not equal to [("$B",e2);("$A",e1)]. This should
        be ok but is potentially a source of duplicate findings in taint mode,
        where these sets are used.
     *)
     let compare pm1 pm2 =
       match compare pm1.range_loc pm2.range_loc with
       | 0 -> compare pm1.env pm2.env
       | c -> c
   end)
*)
