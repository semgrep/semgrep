(* Tean Lai
 *
 * Copyright (C) 2025 Semgrep Inc., All rights reserved
 *)

(* EXPERIMENT: Group taint rules *)

module Log = Log_tainting.Log

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type t = { rules : Rule.taint_rule Common2.nonempty; length : int }

(*****************************************************************************)
(* Hooks *)
(*****************************************************************************)

let hook_group_taint_rules = Hook.create false

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(** [equal_taint_sanitizer s1 s2] returns true if [s1] and [s2] are the same
    sanitizer. Placing the function here instead of Rule.ml because a different
    notion of equality might be used in general for sanitizers, but we only care
    about specific properties for the purpose of optimization. *)
let equal_taint_sanitizer
    ({
       sanitizer_id = _;
       sanitizer_formula = s1_sanitizer_formula;
       sanitizer_exact = s1_sanitizer_exact;
       sanitizer_by_side_effect = s1_sanitizer_by_side_effect;
       not_conflicting = s1_not_conflicting;
     } :
      Rule.taint_sanitizer)
    ({
       sanitizer_id = _;
       sanitizer_formula = s2_sanitizer_formula;
       sanitizer_exact = s2_sanitizer_exact;
       sanitizer_by_side_effect = s2_sanitizer_by_side_effect;
       not_conflicting = s2_not_conflicting;
     } :
      Rule.taint_sanitizer) =
  Rule.equal_formula s1_sanitizer_formula s2_sanitizer_formula
  && s1_sanitizer_exact = s2_sanitizer_exact
  && s1_sanitizer_by_side_effect = s2_sanitizer_by_side_effect
  && s1_not_conflicting = s2_not_conflicting

(** [equal_taint_propagator p1 p2] returns true if [p1] and [p2] are the same
    Placement for similar reasons as [equal_taint_sanitizer]. *)
let equal_taint_propagator
    ({
       propagator_id = _;
       propagator_formula = p1_propagator_formula;
       propagator_by_side_effect = p1_propagator_by_side_effect;
       from = p1_from;
       to_ = p1_to_;
       propagator_requires = p1_requires;
       propagator_replace_labels = p1_replace_labels;
       propagator_label = p1_label;
     } :
      Rule.taint_propagator)
    ({
       propagator_id = _;
       propagator_formula = p2_propagator_formula;
       propagator_by_side_effect = p2_propagator_by_side_effect;
       from = p2_from;
       to_ = p2_to_;
       propagator_requires = p2_requires;
       propagator_replace_labels = p2_replace_labels;
       propagator_label = p2_label;
     } :
      Rule.taint_propagator) =
  Rule.equal_formula p1_propagator_formula p2_propagator_formula
  && p1_propagator_by_side_effect = p2_propagator_by_side_effect
  && (Rule.equal_wrap Mvar.equal) p1_from p2_from
  && (Rule.equal_wrap Mvar.equal) p1_to_ p2_to_
  && p1_requires = p2_requires
  && p1_replace_labels = p2_replace_labels
  && p1_label = p2_label

(** [same_rule_group rule1 rule2] returns true if [rule1] and [rule2]
    should belong to the same rule group with respect to sanitizers and
    propagators. This is our main criteria for grouping rules together. *)
let same_rule_group (rule1 : Rule.taint_rule) (rule2 : Rule.taint_rule) =
  let (`Taint rule1_spec) = rule1.mode in
  let (`Taint rule2_spec) = rule2.mode in
  let same_propagators =
    List.equal equal_taint_propagator rule1_spec.propagators
      rule2_spec.propagators
  in
  let same_sanitizers =
    match (rule1_spec.sanitizers, rule2_spec.sanitizers) with
    | Some (_, rule1_san), Some (_, rule2_san) ->
        List.equal equal_taint_sanitizer rule1_san rule2_san
    | None, None -> true
    | _ -> false
  in
  let same_options = rule1.options = rule2.options in
  let same_ssrf_or_concat =
    (* HACK: These rules have sources like like $X + $Y, which can match a lot of
     * stuff. Always safer to make more groups, in the case of rules timing out
     * entire groups. *)
    let has_ssrf_or_concat_in_name (r : Rule.taint_rule) =
      let id_str = Rule_ID.to_string (fst r.id) in
      Str.string_match (Str.regexp ".*-ssrf.*)") id_str 0
      || Str.string_match (Str.regexp ".*-concat.*") id_str 0
    in
    (* For now, setting it so that if this is true for either rule, we are putting
     * them in different groups. Currently, we don't share sources
     * between rules. Concretely, this means even if rule1 and rule2 has the same
     * source "tainted", we would have two taints, one for each rule.
     *
     * TODO: If sources are shared, uncomment code. *)
    not (has_ssrf_or_concat_in_name rule1 || has_ssrf_or_concat_in_name rule2)
    (* has_ssrf_or_concat_in_name rule1 = has_ssrf_or_concat_in_name rule2 *)
  in
  same_options && same_propagators && same_sanitizers && same_ssrf_or_concat

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)
let rules (g : t) : Rule.taint_rule list = g.rules |> Common2.nonempty_to_list

let group_rules (rules : Rule.taint_rule list) : t list =
  (* THINK: Consider placing the rule with the "lowest" rule_id on the top.
   * this would make the first rule always be the same when running group rules
   * at separate times. *)
  let rule_groups = Common2.group same_rule_group rules in
  rule_groups
  |> List_.map (fun rules ->
         (match rules with
         | Common2.Nonempty (r1, []) ->
             Log.info (fun m ->
                 m "group_rules with %d rules (%s)"
                   (Common2.nonempty_length rules)
                   (Rule_ID.to_string (fst r1.Rule.id)))
         | Common2.Nonempty (r1, r2 :: _) ->
             Log.info (fun m ->
                 m "group_rules with %d rules (%s, %s, ...)"
                   (Common2.nonempty_length rules)
                   (Rule_ID.to_string (fst r1.id))
                   (Rule_ID.to_string (fst r2.id))));
         { rules; length = Common2.nonempty_length rules })

let singleton (rule : Rule.taint_rule) : t =
  let rules = Common2.Nonempty (rule, []) in
  { rules; length = 1 }

let first_rule (group : t) : Rule.taint_rule =
  let (Common2.Nonempty (r, _)) = group.rules in
  r

let length (group : t) : int = group.length

let fold_preds (preds : Taint_spec_preds.t list) : Taint_spec_preds.t =
  {
    is_source =
      (fun any ->
        List.concat_map
          (fun (pred : Taint_spec_preds.t) -> pred.is_source any)
          preds);
    is_propagator =
      (fun any ->
        match preds with
        | [] ->
            Log.err (fun m -> m "No propagators found, this should not happen");
            []
        | pred :: _ -> pred.is_propagator any);
    is_sanitizer =
      (fun any ->
        match preds with
        | [] ->
            Log.err (fun m -> m "No sanitizers found, this should not happen");
            []
        | pred :: _ -> pred.is_sanitizer any);
    is_sink =
      (fun any ->
        List.concat_map
          (fun (pred : Taint_spec_preds.t) -> pred.is_sink any)
          preds);
  }
