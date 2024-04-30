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

(* ! main type ! *)
type t = {
  (* rule (or mini rule) responsible for the pattern match found *)
  rule_id : rule_id; [@equal fun a b -> a.id = b.id]
  (* location info *)
  path : Target.path;
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
}

(* This is currently a record, but really only the rule id should matter.
 *
 * We could derive information in the other fields from the id, but that
 * would require to pass around the list of rules to get back the
 * information. Instead, by embedding the information in the pattern match,
 * some functions are simpler (we use the same trick with Parse_info.t
 * where for example we embed the filename in it, not just a position).
 * alt: reuse Mini_rule.t
 *)
(* !!WARNING!!: If you add a field to this type, if you would like it to be passed
   down to the Pattern_match.t, you need to touch `range_to_pattern_match_adjusted`!
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
  (* so we can calculate core_unique_key later *)
  metadata : JSON.t option;
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
  pm1.rule_id = pm2.rule_id
  && pm1.path.internal_path_to_content = pm2.path.internal_path_to_content
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
         let k = (pm.rule_id, pm.path.internal_path_to_content) in
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
