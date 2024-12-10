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
(* Type to represent a semgrep "match" (a.k.a. finding).
 *
 * Note that the "core" matches are translated at some point in
 * Semgrep_output_v1.core_match, then processed in pysemgrep (or osemgrep)
 * and translated again in Semgrep_output_v1.cli_match, and
 * translated even further by pysemgrep (or osemgrep) ci in
 * Semgrep_output_v1.finding.
 * LATER: it would be good to remove some intermediate types.
 *
 * old: used to be called Pattern_match.t, and before that Match_result.t
 * alt: rename to Finding.ml
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)
(* We use 'eq' below to possibly remove redundant equivalent matches. Indeed,
 * Generic_vs_generic sometimes return multiple times the same match,
 * sometimes because of some bugs we didn't fix, sometimes it's normal
 * because of the way '...' operate. TODO: add an example of such situation.
 *
 * Note that you should not ignore the rule id when comparing two matches!
 * One match can come from a pattern-not: in which case
 * even if it returns the same match than a similar match coming
 * from a pattern:, we should not merge them!
 *)

type t = {
  (* rule (or mini rule) responsible for the pattern match found *)
  rule_id : rule_id; [@equal fun a b -> a.id = b.id]
  (* Indicates whether this match was produced during a run
   * of Semgrep PRO. This will be overrided later by the Pro engine, on any
   * matches which are produced from a Pro run.
   * TODO? do we want to consider the same match but with different engine
   * as separate matches? or better make them equal for dedup purpose?
   *)
  engine_of_match : Engine_kind.engine_of_finding; [@equal fun _a _b -> true]
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
  (* location info *)
  path : Target.path;
  (* less: redundant with location? *)
  (* note that the two Tok.location can be equal *)
  range_loc : Tok.location * Tok.location;
  (* Why is this here?
     When we allow pattern matches to be embedded into metavariables, we want
     to be able to assign a faithful mvalue to the new metavariable.
     This means that we must be able to retrieve the matched AST node from the
     match itself.
     NOTE: This could potentially be a performance issue, because keeping these
     pointers into the AST will persist the lifetime of the AST. However, we
     only need `ast_node` when we are embedding pattern matches into metavariables,
     so we don't need it once the `evaluate_formula` process is done. Thus, we
     set this back to `None` in `Range_with_metavars.range_to_pattern_match_adjusted`.
     NOTE2: Now in `Match_env`, we only set this field if the corresponding
     `has_as_metavariable` is true of the originating rule.
     Otherwise, we leave it at `None`.
  *)
  ast_node : AST_generic.any option;
  (* less: do we need to be lazy? *)
  tokens : Tok.t list Lazy.t; [@equal fun _a _b -> true]
  (* Lazy since construction involves forcing lazy token lists. *)
  (* We used to have `[@equal fun _a _b -> true]` here, but this causes issues with
     multiple findings to the same sink (but different sources) being removed
     in deduplication.
     We now rely on equality of taint traces, which in turn relies on equality of `Parse_info.t`.
  *)
  taint_trace : Taint_trace.t Lazy.t option; (* secrets stuff *)
  (* SCA extra info about a match (e.g., the satisfied version constraint) *)
  sca_match : SCA_match.t option;
  (* Secrets. Indicates whether a postprocessor ran and validated this result. *)
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
  (* A field to be populated based on intra-formula `fix` keys.
     This is _prior_ to AST-based autofix and interpolation, which occurs in
     Autofix.ml.
  *)
  fix_text : string option;
  facts : AST_generic.facts;
}

(* This is currently a record, but really only the rule id should matter.
 *
 * We could derive information in the other fields from the id, but that
 * would require to pass around the list of rules to get back the
 * information. Instead, by embedding the information in the pattern match,
 * some functions are simpler (we use the same trick with Parse_info.t
 * where for example we embed the filename in it, not just a position).
 * alt: reuse Mini_rule.t
 *
 * !!WARNING!!: If you add a field to this type, if you would like it to be
 * passed down to the Core_match.t, you need to touch
 * `range_to_pattern_match_adjusted`!
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
  (* perf: Previously used an initial size of 1024 but profiling with memtrace
   * revealed that this was oversized and contributed to 14% of all major heap
   * usage in an interfile run over juice-shop. Setting it to 8 meant it was
   * undersized in some cases and led to additional allocations as the table
   * gets resized. Change this only with caution. *)
  let tbl = Hashtbl.create (List.length pms) in
  pms
  |> List.iter (fun match_ ->
         let loc = match_.range_loc in
         let matches_at_loc = Hashtbl_.get_stack tbl loc in
         match
           List.find_opt (fun match2 -> equal match_ match2) matches_at_loc
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
  (* Initial hash table size based on memory profiling with memtrace. Increase
   * only with caution. *)
  let tbl = Hashtbl.create (List.length pms) in
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

let to_proprietary pm = { pm with engine_of_match = `PRO }
