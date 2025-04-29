(* Iago Abal, Yoann Padioleau
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
open Common
open Fpath_.Operators
module Log = Log_tainting.Log
module G = AST_generic
module ME = Matching_explanation
module MV = Metavariable
module R = Rule
module RM = Range_with_metavars
module OutJ = Semgrep_output_v1_t
module D = Dataflow_tainting
module TM = Taint_spec_match
module TP = Taint_spec_preds

(* Taint-tracking via ranges
 * -------------------------
 *
 * First we run a bunch of search queries to obtain the ranges of sources,
 * sanitizers, and sinks. The actual analysis happens in Dataflow_tainting
 * using the IL representation. There we check whether the eorig's and iorig's
 * are within those ranges to decide whether an expression or instruction is a
 * source of taint, a sanitizer, or a sink. Finally we collect the results
 * produced by the dataflow analysis and filter out duplicates.
 *
 * We could think of using an equality test to compare ranges, e.g., an
 * expression would be a source of taint only if its range is exactly one of
 * the ranges matched by `pattern-sources`. In practice, this does not work
 * because `pattern-sources` etc can match anything, and the IL eorig's and
 * iorig's are only expressions. For example, `pattern-sources` can match
 * `foo(x);` but the eorig in the IL will be `foo(x)` whose range does not
 * include the ending `;`.
 *
 * So, we use sub-range checks. And this actually provides some extra power,
 * as it allows us to mark anything as a source/sanitizer/sink. For example,
 * we could use a pattern like `if (E) { ... }` to specify that anything
 * inside such an `if` statement should be considered sanitized. We are not
 * limited to expressions or single statements.
 *
 * However, using sub-range checks leads to duplicates. For example, the PHP
 * expression `sink("$source" . 'here')` will be translated to IL as two
 * instructions `tmp = "$source" . 'here'` and `sink(tmp)`. If `sink(...)`
 * is a `pattern-sinks`, then both instructions' ranges are inside
 * the `pattrn-sinks` ranges. If `$source` is a `pattern-sources`, then both
 * instructions are also tainted, and Dataflow_tainting will report two
 * matches.
 *
 * So, we need to remove duplicate subamtches at the end.
 * TODO: We could perhaps do this in a cleaner way by having an intermediate
 * step where we annotate the Generic AST, marking which statements and
 * expressions are sources, sanitizers, or sinks. If e.g. an expression is a
 * sink, we take care not to mark as sinks any of its subexpressions, in order
 * to prevent duplicates.
 *)

type propagator_match = {
  id : TP.var;
  rwm : RM.t;
  from : Range.t;
  to_ : Range.t;
  spec : R.taint_propagator;
}

type raw_spec_matches = {
  raw_sources : (RM.t * R.taint_source) list;
  raw_propagators : propagator_match list;
  raw_sanitizers : (RM.t * R.taint_sanitizer) list;
  raw_sinks : (RM.t * R.taint_sink) list;
}

type spec_matches = {
  sources : (RM.t * R.taint_source) list;
  propagation_points : TP.propagation_point TM.t list;
  sanitizers : (RM.t * R.taint_sanitizer) list;
  sinks : (RM.t * TP.sink) list;
}

(*****************************************************************************)
(* Hooks *)
(*****************************************************************************)

let hook_mk_taint_spec_match_preds :
    (Rule.rule -> spec_matches -> TP.t) option ref =
  ref None

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let filter_map_warn_if_None ~warn f xs =
  xs
  |> List_.filter_map (fun x ->
         let opt_y = f x in
         if Option.is_none opt_y then Log.warn (fun m -> m "%s" warn);
         opt_y)

(*****************************************************************************)
(* Finding matches for taint specs *)
(*****************************************************************************)

(* Finds all matches of a taint-spec pattern formula. *)
let range_w_metas_of_formula (xconf : Match_env.xconfig) (xtarget : Xtarget.t)
    (rule : R.t) (formula : R.formula) : RM.ranges * ME.t list =
  (* !! Calling Match_search_mode here !! *)
  let report, ranges =
    Match_search_mode.matches_of_formula xconf rule xtarget formula None
  in
  (ranges, report.explanations)

(* =~ List.concat_map with automatic management of matching-explanations *)
let concat_map_with_expls f xs =
  let all_expls = ref [] in
  let res =
    xs
    |> List.concat_map (fun x ->
           let ys, expls = f x in
           Stack_.push expls all_expls;
           ys)
  in
  (res, List_.flatten (List.rev !all_expls))

let%test _ =
  concat_map_with_expls (fun x -> ([ -x; x ], [ 2 * x; 3 * x ])) [ 0; 1; 2 ]
  =*= ([ 0; 0; -1; 1; -2; 2 ], [ 0; 0; 2; 3; 4; 6 ])

let find_range_w_metas formula_cache (xconf : Match_env.xconfig)
    (xtarget : Xtarget.t) (rule : R.t) (specs : (R.formula * 'a) list) :
    (RM.t * 'a) list * ME.t list =
  (* TODO: Make an Or formula and run a single query. *)
  (* if perf is a problem, we could build an interval set here *)
  specs
  |> concat_map_with_expls (fun (pf, x) ->
         let ranges, expls =
           Formula_cache.cached_find_opt formula_cache pf (fun () ->
               range_w_metas_of_formula xconf xtarget rule pf)
         in
         (ranges |> List_.map (fun rwm -> (rwm, x)), expls))

let find_sources_ranges formula_cache xconf xtarget rule (spec : R.taint_spec) =
  find_range_w_metas formula_cache xconf xtarget rule
    (spec.sources |> snd
    |> List_.map (fun (src : R.taint_source) -> (src.source_formula, src)))
[@@trace_trace]

let find_sinks_ranges formula_cache xconf xtarget rule (spec : R.taint_spec) =
  find_range_w_metas formula_cache xconf xtarget rule
    (spec.sinks |> snd
    |> List_.map (fun (sink : R.taint_sink) -> (sink.sink_formula, sink)))
[@@trace_trace]

let find_sanitizers_matches formula_cache (xconf : Match_env.xconfig)
    (xtarget : Xtarget.t) (rule : R.t) (specs : R.taint_sanitizer list) :
    (bool * RM.t * R.taint_sanitizer) list * ME.t list =
  specs
  |> concat_map_with_expls (fun (sanitizer : R.taint_sanitizer) ->
         let ranges, expls =
           Formula_cache.cached_find_opt formula_cache
             sanitizer.sanitizer_formula (fun () ->
               range_w_metas_of_formula xconf xtarget rule
                 sanitizer.sanitizer_formula)
         in
         ( ranges
           |> List_.map (fun x -> (sanitizer.R.not_conflicting, x, sanitizer)),
           expls ))
[@@trace_trace]

(* Finds all matches of `pattern-propagators`. *)
let find_propagators_matches formula_cache (xconf : Match_env.xconfig)
    (xtarget : Xtarget.t) (rule : R.t)
    (propagators_spec : R.taint_propagator list) =
  propagators_spec
  |> List.concat_map (fun (p : R.taint_propagator) ->
         let mvar_pfrom, tok_pfrom = p.from in
         let mvar_pto, tok_pto = p.to_ in
         let ranges_w_metavars, _expsTODO =
           Formula_cache.cached_find_opt formula_cache p.propagator_formula
             (fun () ->
               range_w_metas_of_formula xconf xtarget rule p.propagator_formula)
         in
         (* Now, for each match of the propagator pattern, we try to construct
          * a `propagator_match`. We just need to look up what code is captured
          * by the metavariables `from` and `to`, and check if we can obtain good
          * location info for that code (i.e., we have real tokens rather than
          * fake ones). *)
         ranges_w_metavars
         |> filter_map_warn_if_None
              ~warn:"Skipping propagator match because we lack range info"
              (fun rwm ->
                (* The piece of code captured by the `from` metavariable.  *)
                let* _mvar_from, mval_from =
                  List.find_opt
                    (fun (mvar, _mval) -> MV.equal_mvar mvar_pfrom mvar)
                    rwm.RM.mvars
                in
                (* The piece of code captured by the `to` metavariable.  *)
                let* _mvar_to, mval_to =
                  List.find_opt
                    (fun (mvar, _mval) -> MV.equal_mvar mvar_pto mvar)
                    rwm.RM.mvars
                in
                match (Tok.loc_of_tok tok_pfrom, Tok.loc_of_tok tok_pto) with
                | Error _, _
                | _, Error _ ->
                    None
                | Ok loc_pfrom, Ok loc_pto ->
                    let* mval_from_start_loc, mval_from_end_loc =
                      AST_generic_helpers.range_of_any_opt
                        (MV.mvalue_to_any mval_from)
                    in
                    let* mval_to_start_loc, mval_to_end_loc =
                      AST_generic_helpers.range_of_any_opt
                        (MV.mvalue_to_any mval_to)
                    in
                    let from =
                      Range.range_of_token_locations mval_from_start_loc
                        mval_from_end_loc
                    in
                    let to_ =
                      Range.range_of_token_locations mval_to_start_loc
                        mval_to_end_loc
                    in
                    let id =
                      Common.spf
                        "propagator<%d,%d>(%s@l.%d/%d-%d-->%s@l.%d/%d-%d)"
                        loc_pfrom.pos.bytepos loc_pto.pos.bytepos mvar_pfrom
                        mval_from_start_loc.Loc.pos.line from.Range.start
                        from.Range.end_ mvar_pto mval_to_start_loc.Loc.pos.line
                        to_.Range.start to_.Range.end_
                    in
                    Some { id; rwm; from; to_; spec = p }))
[@@trace_trace]

(*****************************************************************************)
(* Raw spec matches *)
(*****************************************************************************)

let raw_spec_matches_of_taint_rule ~per_file_formula_cache xconf file
    ast_and_errors ({ mode = `Taint spec; _ } as rule : R.taint_rule) =
  let file = Fpath.v file in
  let formula_cache = per_file_formula_cache in
  let xconf = Match_env.adjust_xconfig_with_rule_options xconf rule.options in
  let lazy_ast_and_errors = lazy ast_and_errors in
  (* TODO: should this function just take a target, rather than a file? *)
  let xtarget : Xtarget.t =
    {
      path = { origin = File file; internal_path_to_content = file };
      analyzer = rule.target_analyzer;
      lazy_content = lazy (UFile.read_file file);
      lazy_ast_and_errors;
    }
  in
  let (sources_ranges : (RM.t * R.taint_source) list), expls_sources =
    find_sources_ranges formula_cache xconf xtarget rule spec
  in
  let (propagators_ranges : propagator_match list) =
    (* NOTE "Symbolic-propagation & Taint-propagation"

       Symbolic-propagation and taint-propagation can interact in a rather
       unfortunate way leading to unexpected results.

       E.g. Give the code below and the propagator `$TO.prepare_url($FROM)`
       we would get a finding in line 4!

           1 def test():
           2      tainted_url = taint
           3      r = safe
           4      sink(r) # unexpected finding :-(
           5      r.prepare_url(tainted_url)

       This is due to symbolic-propagation tracking that `r = safe` and
       `tainted_url = taint`, so the propagator `$TO.prepare_url($FROM)`
       produces **four** matches rather than just one, where `$FROM` is
       either `tainted_url` or `taint`, and `$TO` is either `r` or `safe`.
       So, we end up propagating taint from `taint` (line 2) to `safe`
       (line 3), thus producing a false positive.

       Thus we disable symbolic-propagation to match taint propagators.

       [Iago] I am not sure whether symbolic-propagation should produce
       these four findings, but rather just one. I think we should just
       have `$FROM=tainted_url` and `$TO=r`, and e.g. if we later apply
       a `metavariable-regex` to `$TO` then that needs to take the
       sym-prop'ed value of `r` into account. In fact, that is how it
       works for const-prop'ed values already. But while that gets fixed
       then this is an OK solution.
    *)
    Log.warn (fun m ->
        m "Disabling symbolic-propagation to match taint propagators");
    let xconf =
      { xconf with config = { xconf.config with symbolic_propagation = false } }
    in
    find_propagators_matches formula_cache xconf xtarget rule spec.propagators
  in
  let (sinks_ranges : (RM.t * R.taint_sink) list), expls_sinks =
    find_sinks_ranges formula_cache xconf xtarget rule spec
  in
  let sanitizers_ranges, expls_sanitizers =
    match spec.sanitizers with
    | None -> ([], [])
    | Some (_, sanitizers_spec) ->
        find_sanitizers_matches formula_cache xconf xtarget rule sanitizers_spec
  in
  let (sanitizers_ranges : (RM.t * R.taint_sanitizer) list) =
    (* A sanitizer cannot conflict with a sink or a source, otherwise it is
     * filtered out. This allows to e.g. declare `$F(...)` as a sanitizer,
     * to assume that any other function will handle tainted data safely.
     * Without this, `$F(...)` will automatically sanitize any other function
     * call acting as a sink or a source. *)
    sanitizers_ranges
    |> filter_map_warn_if_None
         ~warn:"Skipping (DEPRECATED) not-conflicting sanitizer due to conflict"
         (fun (not_conflicting, range, spec) ->
           (* TODO: Need to remove not-conflicting sanitizers, it has been
                DEPRECATED for a long time already. *)
           if not_conflicting then
             if
               not
                 (List.exists
                    (fun (range', _) -> range'.RM.r =*= range.RM.r)
                    sinks_ranges
                 || List.exists
                      (fun (range', _) -> range'.RM.r =*= range.RM.r)
                      sources_ranges)
             then Some (range, spec)
             else None
           else Some (range, spec))
  in
  let expls =
    if xconf.matching_explanations then
      let ranges_to_pms ranges_and_stuff =
        ranges_and_stuff
        |> List_.map (fun (rwm, _) ->
               RM.range_to_pattern_match_adjusted rule rwm)
      in
      [
        {
          ME.op = OutJ.TaintSource;
          pos = fst spec.sources;
          children = expls_sources;
          matches = ranges_to_pms sources_ranges;
          extra = None;
        };
        {
          ME.op = OutJ.TaintSink;
          pos = fst spec.sinks;
          children = expls_sinks;
          matches = ranges_to_pms sinks_ranges;
          extra = None;
        }
        (* TODO: propagators *);
      ]
      @
      match spec.sanitizers with
      | None -> []
      | Some (tok, _) ->
          [
            {
              ME.op = OutJ.TaintSanitizer;
              pos = tok;
              children = expls_sanitizers;
              (* 'sanitizer_ranges' will be affected by `not-conflicting: true`:
               * if a sanitizer coincides exactly with a source/sink then it will
               * be filtered out. So the sanitizer matches may not be the union of
               * the matches of the individual sanitizers. Anyhow, not-conflicting
               * has been deprecated for quite some time, and we will remove it at
               * some point. *)
              matches = ranges_to_pms sanitizers_ranges;
              extra = None;
            };
          ]
    else []
  in
  ( {
      raw_sources = sources_ranges;
      raw_propagators = propagators_ranges;
      raw_sanitizers = sanitizers_ranges;
      raw_sinks = sinks_ranges;
    },
    expls )

(*****************************************************************************)
(* Spec matches *)
(*****************************************************************************)

(* From a propagator match we get two propagation points: a '`From' and a '`To'
  point. *)
let mk_propagation_points_of_propagator rule (prop : propagator_match) =
  let spec_pm = RM.range_to_pattern_match_adjusted rule prop.rwm in
  let mk_prop_point kind r =
    let spec : TP.propagation_point =
      {
        kind;
        var = prop.id;
        prop_by_side_effect = prop.spec.propagator_by_side_effect;
        prop_requires =
          prop.spec.propagator_requires
          |> Option.map (fun pwr -> pwr.R.precondition);
        prop_label = prop.spec.propagator_label;
        prop_replace_labels = prop.spec.propagator_replace_labels;
        prop_orig = Prop prop.spec;
      }
    in
    {
      TM.spec;
      spec_id = prop.spec.propagator_id;
      spec_pm;
      range = r;
      overlap = 1.0;
    }
  in
  [ mk_prop_point `From prop.from; mk_prop_point `To prop.to_ ]

let propagation_points_of_propagators rule prop_matches =
  prop_matches |> List.concat_map (mk_propagation_points_of_propagator rule)

(* NOTE "Multi-requires as propagators"

  The example below shows a common use of "multi-requires", where we have a
  sink function `foobar` that is too generic, and we want the sink to apply only
  when the receiver object $OBJ has a certain property that we will track with
  a separate label TYPE.

      pattern-sinks:
      - patterns:
          - pattern: $OBJ.foobar($SINK)
          - focus-metavariable: $SINK
        requires:
        - $SINK: TAINT
        - $OBJ: TYPE

  It may feel "trivial" to implement this, after all, we can look at the code
  bound by $OBJ and check what taint it has... Wrong! There is no trivial way
  of knowing what are the taints of that sub-tree of the Generic AST. We would
  need to convert it into the IL first, then re-run taint inference under the
  correct environment...

  So, what we do is to treat this as a propagation problem. We consider $OBJ
  and $SINK in `$OBJ.foobar($SINK)` to be "from" propagation points, with the
  destination/"to" being the corresponding $OBJ and $SINK in the `requires:`.
  This way we reuse the propagation machinery to capture the taints of the
  expressions bound by $OBJ and $SINK, and just query the propagation variables
  when evaluating the `requires:` (see 'Dataflow_tainting.sink_of_match').
 *)
let mk_requires_and_propagation_points_of_sink
    ((rwm : RM.t), (taint_sink : R.taint_sink)) =
  let prop_var_of_requires ((mvar, tok), (pwr : Rule.precondition_with_range)) =
    let* _mvar, mval =
      List.find_opt
        (fun (mvar', _mval) -> MV.equal_mvar mvar mvar')
        rwm.RM.mvars
    in
    let* mvar_rule_loc = Tok.loc_of_tok tok |> Result.to_option in
    let* mval_start_loc, mval_end_loc =
      AST_generic_helpers.range_of_any_opt (MV.mvalue_to_any mval)
    in
    let r_req = Range.range_of_token_locations mval_start_loc mval_end_loc in
    let id =
      Common.spf "sink-requires<%s@l.%d>l.%d/%d-%d" mvar
        mvar_rule_loc.Loc.pos.line mval_start_loc.Loc.pos.line r_req.Range.start
        r_req.Range.end_
    in
    let spec : TP.propagation_point =
      {
        kind = `From;
        var = id;
        prop_by_side_effect = false;
        prop_requires =
          (* Even if the sink has a `requires:`, propagation here is unconditional,
            they are unrelated. *)
          None;
        prop_label = None;
        prop_replace_labels = None;
        prop_orig = SinkMultiReq taint_sink;
      }
    in
    let tsm =
      {
        TM.spec;
        spec_id = taint_sink.sink_id ^ "/" ^ mvar;
        spec_pm = rwm.origin;
        range = r_req;
        overlap = 1.0;
      }
    in
    Some ((id, pwr.precondition), tsm)
  in
  let requires, req_prop_points =
    match taint_sink.sink_requires with
    | None -> (TP.UniReq Rule.(PLabel default_source_label), [])
    | Some (UniReq pwr) -> (TP.UniReq pwr.precondition, [])
    | Some (MultiReq mvars_w_preconds) ->
        let mvars_w_preconds, req_prop_points =
          mvars_w_preconds
          |> filter_map_warn_if_None
               ~warn:"Skipping sink requires because we lack range info"
               prop_var_of_requires
          |> List_.split
        in
        (TP.MultiReq mvars_w_preconds, req_prop_points)
  in
  let sink : TP.sink = { requires; spec = taint_sink } in
  ((rwm, sink), req_prop_points)

let requires_and_propagation_points_of_sinks sinks_ranges =
  let sinks_ranges, sink_req_props =
    sinks_ranges
    |> List_.map mk_requires_and_propagation_points_of_sink
    |> List_.split
  in
  (sinks_ranges, List_.flatten sink_req_props)

let spec_matches_of_raw rule raw =
  let prop_ts_matches =
    propagation_points_of_propagators rule raw.raw_propagators
  in
  let sinks_ranges, sinks_prop_ts_matches =
    requires_and_propagation_points_of_sinks raw.raw_sinks
  in
  let prop_ts_matches = List.rev_append sinks_prop_ts_matches prop_ts_matches in
  {
    sources = raw.raw_sources;
    propagation_points = prop_ts_matches;
    sanitizers = raw.raw_sanitizers;
    sinks = sinks_ranges;
  }

(*****************************************************************************)
(* Testing whether a an AST node matches a taint spec *)
(*****************************************************************************)

let option_bind_list opt f =
  match opt with
  | None -> []
  | Some x -> f x

let range_of_any any =
  (* This is potentially slow. We may need to store range position in
   * the AST at some point. *)
  match AST_generic_helpers.range_of_any_opt any with
  | None ->
      (* IL.any_of_orig will return `G.Anys []` for `NoOrig`, and there is
       * no point in issuing this warning in that case.
       * TODO: Perhaps we should avoid the call to `any_in_ranges` in the
       * first place? *)
      if any <> G.Anys [] then
        Log.debug (fun m ->
            m "Cannot compute range, there are no real tokens in this AST: %s"
              (G.show_any any));
      None
  | Some (tok1, tok2) ->
      let r = Range.range_of_token_locations tok1 tok2 in
      Some r

(* Assuming that `r` is a subrange of `match_range` then this computes a
 * float in [0.0, 1.0]. We expect `r` to be the range of some arbitrary
 * piece of code,  and `match_range` to be the range of a match of some
 * taint spec (e.g. a taint source). Then this float indicates how much
 * overlap there is between the code and the spec. The degree of overlap
 * is used to determine whether the match is "exact" (overlap > 0.99),
 * which e.g. triggets the side-effectful propagation of taint. *)
let overlap_with ~match_range r =
  let r1 = match_range in
  float_of_int (r.Range.end_ - r.Range.start + 1)
  /. float_of_int (r1.Range.end_ - r1.Range.start + 1)

let any_is_in_matches_OSS rule matches ~get_id any =
  let ( let* ) = option_bind_list in
  let* r = range_of_any any in
  matches
  |> List_.filter_map (fun (rwm, spec) ->
         if Range.( $<=$ ) r rwm.RM.r then
           Some
             (let spec_pm = RM.range_to_pattern_match_adjusted rule rwm in
              let overlap = overlap_with ~match_range:rwm.RM.r r in
              TM.{ spec; spec_id = get_id spec; spec_pm; range = r; overlap })
         else None)

let is_exact_match ~match_range r =
  let overlap = overlap_with ~match_range r in
  let r1 = match_range in
  Range.( $<=$ ) r r1 && overlap > 0.99

(* Check whether `any` matches either the `from` or the `to` of any of the
 * `pattern-propagators`. Matches must be exact (overlap > 0.99) to make
 * taint propagation more precise and predictable. *)
let any_is_in_propagators_matches_OSS matches any :
    TP.propagation_point TM.t list =
  match range_of_any any with
  | None -> []
  | Some r ->
      matches
      |> List_.filter_map (fun (tm : _ TM.t) ->
             if is_exact_match ~match_range:tm.range r then Some tm else None)

let mk_taint_spec_match_preds rule matches =
  match !hook_mk_taint_spec_match_preds with
  | None ->
      TP.
        {
          is_source =
            (fun any ->
              any_is_in_matches_OSS rule matches.sources any
                ~get_id:(fun (ts : R.taint_source) -> ts.source_id));
          is_propagator =
            (fun any ->
              any_is_in_propagators_matches_OSS matches.propagation_points any);
          is_sanitizer =
            (fun any ->
              any_is_in_matches_OSS rule matches.sanitizers any
                ~get_id:(fun (ts : R.taint_sanitizer) -> ts.sanitizer_id));
          is_sink =
            (fun any ->
              any_is_in_matches_OSS rule matches.sinks any
                ~get_id:(fun (sink : TP.sink) -> sink.spec.sink_id));
        }
  | Some hook -> hook rule matches

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let taint_config_of_rule ~per_file_formula_cache ~(file : Taint_rule_inst.file)
    xconf ast_and_errors ({ mode = `Taint spec; _ } as rule : R.taint_rule) =
  let raw_spec_matches, expls =
    raw_spec_matches_of_taint_rule ~per_file_formula_cache xconf !!(file.path)
      ast_and_errors rule
  in
  let xconf = Match_env.adjust_xconfig_with_rule_options xconf rule.options in
  let options = xconf.config in
  let spec_matches = spec_matches_of_raw rule raw_spec_matches in
  let preds = mk_taint_spec_match_preds rule spec_matches in
  ( Taint_rule_inst.
      {
        file;
        rule_id = fst rule.R.id;
        options;
        track_control =
          spec.sources |> snd
          |> List.exists (fun (src : R.taint_source) -> src.source_control);
        preds;
      },
    raw_spec_matches,
    expls )
[@@trace_trace]
