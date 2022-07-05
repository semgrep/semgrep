(* Yoann Padioleau
 *
 * Copyright (C) 2019-2021 r2c
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
type tainted_tokens = Parse_info.t list [@@deriving show]

(* The tokens associated with a single pattern match involved in a taint trace
 * *)
type pattern_match_tokens = Parse_info.t list [@@deriving show]

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
[@@deriving show]

type taint_trace = {
  source : taint_call_trace;
  tokens : tainted_tokens;
  sink : taint_call_trace;
}
[@@deriving show]

type t = {
  (* rule (or mini rule) responsible for the pattern match found *)
  rule_id : rule_id; [@equal fun a b -> a.id = b.id]
  (* location information *)
  file : Common.filename;
  (* less: redundant with location? *)
  (* note that the two token_location can be equal *)
  range_loc : Parse_info.token_location * Parse_info.token_location;
  (* less: do we need to be lazy? *)
  tokens : Parse_info.t list Lazy.t; [@equal fun _a _b -> true]
  (* metavars for the pattern match *)
  env : Metavariable.bindings;
  (* Lazy since construction involves forcing lazy token lists. *)
  taint_trace : taint_trace Lazy.t option; [@equal fun _a _b -> true]
}

(* This is currently a record, but really only the rule id should matter.
 *
 * We could derive information in the other fields from the id, but that
 * would require to pass around the list of rules to get back the
 * information. Instead by embedding the information in the pattern match,
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
  id : string;
  (* other parts of a rule (or mini_rule) used in JSON_report.ml *)
  message : string;
  (* used for debugging (could be removed at some point) *)
  pattern_string : string;
}
[@@deriving show, eq]

let uniq pms =
  let eq = AST_utils.with_structural_equal equal in
  let tbl = Hashtbl.create 1_024 in
  pms
  |> List.iter (fun pm ->
         let r = pm.range_loc in
         let ys = Hashtbl.find_all tbl r in
         match List.find_opt (fun y -> eq pm y) ys with
         | Some _ -> ()
         | None -> Hashtbl.add tbl r pm);
  tbl |> Hashtbl.to_seq_values |> List.of_seq
  [@@profiling]

let range pm =
  let start_loc, end_loc = pm.range_loc in
  Range.range_of_token_locations start_loc end_loc

(* Is [pm1] a submatch of [pm2] ? *)
let submatch pm1 pm2 =
  pm1.rule_id = pm2.rule_id && pm1.file = pm2.file
  (* THINK: && "pm1.bindings = pm2.bindings" ? *)
  && Range.( $<=$ ) (range pm1) (range pm2)

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

(* This special Set is used in the dataflow tainting code,
   which manipulates sets of matches associated to each variables.
   We only care about the metavariable environment carried by the pattern matches
   at the moment.
*)
module Set = Set.Make (struct
  type previous_t = t

  (* alt: use type nonrec t = t, but this causes pad's codegraph to blowup *)
  type t = previous_t

  (* If the pattern matches are obviously different (have different ranges), this is enough to compare them.
     If their ranges are the same, compare their metavariable environments. This is not robust to reordering
     metavariable environments. [("$A",e1);("$B",e2)] is not equal to [("$B",e2);("$A",e1)]. This should be ok
     but is potentially a source of duplicate findings in taint mode, where these sets are used.
  *)
  let compare pm1 pm2 =
    match compare pm1.range_loc pm2.range_loc with
    | 0 -> compare pm1.env pm2.env
    | c -> c
end)
