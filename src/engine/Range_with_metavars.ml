module Log = Log_engine.Log

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type range_kind = Plain | Inside | Anywhere | Regexp [@@deriving show]

(* range with metavars *)
type t = {
  r : Range.t;
  mvars : Metavariable.bindings;
  (* subtle but the pattern:/pattern-inside:/pattern-regex: and
   * pattern-not:/pattern-not-inside:/pattern-not-regex: are actually different,
   * so we need to keep the information around during the evaluation.
   * Note that this useful only for few tests in semgrep-rules/ so we
   * probably want to simplify things later and remove the difference between
   * xxx, xxx-inside, and xxx-regex.
   * TODO: in fact, if we do a proper intersection of ranges, where we
   * properly intersect (not just filter one or the other), and also merge
   * metavariables, this will clean lots of things, and remove the need
   * to keep around the Inside. 'And' would be commutative again!
   *)
  kind : range_kind;
  origin : Core_match.t;
}
[@@deriving show]

(* less: use Set instead of list? *)
type ranges = t list [@@deriving show]

(*****************************************************************************)
(* Convertors *)
(*****************************************************************************)

let match_result_to_range (m : Core_match.t) : t =
  let Core_match.{ range_loc = start_loc, end_loc; env = mvars; _ } = m in
  let r = Range.range_of_token_locations start_loc end_loc in
  { r; mvars; origin = m; kind = Plain }

let range_to_pattern_match_adjusted (r : Rule.t) (range : t) : Core_match.t =
  let m = range.origin in
  let rule_id = m.rule_id in
  let langs = Analyzer.to_langs r.target_analyzer in
  (* adjust the rule id *)
  let rule_id : Core_match.rule_id =
    {
      rule_id with
      id = fst r.id;
      fix = r.fix;
      langs;
      message = r.message (* keep pattern_str which can be useful to debug *);
      metadata = r.metadata;
    }
  in
  (* Need env to be the result of evaluate_formula, which propagates metavariables *)
  (* rather than the original metavariables for the match                          *)
  (* We wipe the `ast_node` fields here, because they are only needed for when are producing
      metavariable bindings using `as`.
      To keep them would persist the pointers to the AST, potentially prolonging its
      lifetime and increasing memory pressure.
  *)
  { m with rule_id; env = range.mvars; ast_node = None }

(*****************************************************************************)
(* Set operations *)
(*****************************************************************************)

let included_in config rv1 rv2 =
  (Range.( $<=$ ) rv1.r rv2.r || rv2.kind = Anywhere)
  && rv1.mvars
     |> List.for_all (fun (mvar, mval1) ->
            match List.assoc_opt mvar rv2.mvars with
            | None -> true
            (* Numeric capture group metavariables (of the form $1, $2, etc) may
               be introduced implicitly via regular expressions that have
               capture groups in them. The unification of these metavariables
               can be dangerous, as it will prevent matches, when users may not
               even know these metavariables exist. To be safe, let's assume
               they always unify.

               note: this does not affect named capture group metavariables
               from <?xxx>, which will still be unified as normal
            *)
            | _ when Mvar.is_metavar_for_capture_group mvar -> true
            | Some mval2 ->
                Matching_generic.equal_ast_bound_code config mval1 mval2)

(* when we know x <= y, are the ranges also in the good Inside direction *)
let inside_compatible x y =
  let x_inside = x.kind = Inside in
  let y_inside = y.kind = Inside in
  (* IF x is pattern-inside THEN y must be too:
   * if we do x=pattern-inside:[1-2] /\ y=pattern:[1-3]
   * we don't want this x to survive.
   * See tests/rules/and_inside.yaml
   *)
  (not x_inside) || y_inside

(* We now not only check whether a range is included in another,
 * we also merge their metavars. The reason is that with some rules like:
 * - pattern-inside:  { dialect: $DIALECT,  ... }
 * - pattern: { minVersion: 'TLSv1' }
 * - metavariable-regex:
 *     metavariable: $DIALECT
 *     regex: "['\"](mariadb|mysql|postgres)['\"]"
 * when intersecting the two patterns, we must propagate the binding
 * for $DIALECT, so we can evaluate the metavariable-regex.
 *
 * alt: we could force the user to first And the metavariable-regex
 * with the pattern-inside, to have the right scope.
 * See https://github.com/semgrep/semgrep/issues/2664
 * alt: we could do the rewriting ourselves, detecting that the
 * metavariable-regex has the wrong scope.
 *
 * TODO: remove ~debug_matches, just use SEMGREP_LOG_SRCS=engine
 *)
let intersect_ranges config ~debug_matches xs ys =
  let left_merge u v =
    if included_in config u v && inside_compatible u v then
      (* [u] extended with [v.mvars], assumes [included_in config u v] *)
      let v_only_mvars =
        v.mvars
        |> List.filter (fun (mvar, _) -> not (List.mem_assoc mvar u.mvars))
      in
      Some { u with mvars = v_only_mvars @ u.mvars }
    else None
  in
  let merge p us vs =
    us |> Common2.map_flatten (fun u -> vs |> List_.filter_map (fun v -> p u v))
  in
  if debug_matches then
    Log.debug (fun m ->
        m "intersect_range:\n\t%s\nvs\n\t%s" (show_ranges xs) (show_ranges ys));
  merge left_merge xs ys
  (* TODO: just call merge once? *)
  @ merge (Fun.flip left_merge) xs ys
[@@profiling]

let difference_ranges config pos neg =
  let surviving_pos =
    pos
    |> List.filter (fun x ->
           not
             (neg
             |> List.exists (fun y ->
                    (* pattern-not vs pattern-not-inside vs pattern-not-regex,
                     * the difference matters!
                     * This fixed 10 mismatches in semgrep-rules and some e2e tests.
                     *)
                    match y.kind with
                    (* pattern-not-inside: x cannot occur inside y *)
                    | Inside -> included_in config x y
                    (* pattern-not-regex: x and y exclude each other *)
                    | Regexp -> included_in config x y || included_in config y x
                    (* pattern-not: we require the ranges to be equal *)
                    | Plain -> included_in config x y && included_in config y x
                    (* not: { anywhere: y } -- y cannot occur *)
                    | Anywhere -> true)))
  in
  surviving_pos
[@@profiling]
