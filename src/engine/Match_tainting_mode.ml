(* Iago Abal, Yoann Padioleau
 *
 * Copyright (C) 2019-2022 r2c
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
open File.Operators
module D = Dataflow_tainting
module Var_env = Dataflow_var_env
module G = AST_generic
module H = AST_generic_helpers
module R = Rule
module PM = Pattern_match
module RM = Range_with_metavars
module RP = Core_result
module T = Taint
module Lval_env = Taint_lval_env
module MV = Metavariable
module ME = Matching_explanation
module Out = Semgrep_output_v1_t
module Labels = Set.Make (String)

let logger = Logging.get_logger [ __MODULE__ ]

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Wrapper around the tainting dataflow-based analysis.
 *
 * Here we pass matcher functions that uses semgrep patterns to
 * describe the source/sink/sanitizers.
 *
 * Taint-tracking via ranges
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

type debug_taint = {
  sources : (RM.t * R.taint_source) list;
  sanitizers : RM.ranges;
  sinks : (RM.t * R.taint_sink) list;
}

(*****************************************************************************)
(* Hooks *)
(*****************************************************************************)

let hook_setup_hook_function_taint_signature = ref None

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
module F2 = IL

module DataflowY = Dataflow_core.Make (struct
  type node = F2.node
  type edge = F2.edge
  type flow = (node, edge) CFG.t

  let short_string_of_node n = Display_IL.short_string_of_node_kind n.F2.n
end)

let convert_rule_id (id, _tok) =
  {
    PM.id;
    message = "";
    pattern_string = Rule_ID.to_string id;
    fix = None;
    langs = [];
  }

let option_bind_list opt f =
  match opt with
  | None -> []
  | Some x -> f x

(* Finds all matches of a taint-spec pattern formula. *)
let range_w_metas_of_formula (xconf : Match_env.xconfig) (xtarget : Xtarget.t)
    (rule : R.t) (formula : R.formula) : RM.ranges * ME.t list =
  (* !! Calling Match_search_mode here !! *)
  let report, ranges =
    Match_search_mode.matches_of_formula xconf rule xtarget formula None
  in
  (ranges, report.explanations)

type propagator_match = {
  id : D.var;
      (** An unique identifier for the propagator match. This is used as an
   * auxiliary variable to store the taints flowing from `from` to `to`. *)
  rwm : RM.t;
  from : Range.t;  (** The range matched by the `from` metavariable. *)
  to_ : Range.t;  (** The range matched by the `to` metavariable. *)
  spec : R.taint_propagator;
}
(** Taint will flow from `from` to `to_` through the axiliary variable `id`. *)

(* This Formula_tbl structure is used to create a "formula cache", which will
   permit the sharing of matches resulting from common formulas that occur as
   sources, sinks, sanitizers, or propagators.

   In particular, because of hardcoded propagators, we expect to see lots of
   sharing from Semgrep Pro Engine.
*)
module Formula_tbl = struct
  include Hashtbl.Make (struct
    type t = R.formula

    let equal = AST_generic_equals.with_structural_equal R.equal_formula
    let hash = R.hash_formula
  end)

  let cached_find_opt formula_cache formula compute_matches_fn =
    match find_opt formula_cache formula with
    | None ->
        (* it should not actually be possible for a formula to
           not be in the formula table

           just don't cache it I guess
        *)
        logger#error
          "Tried to compute matches for a taint formula not in the cache \
           (impossible?)";
        compute_matches_fn ()
    | Some (None, count) ->
        let ranges, expls = compute_matches_fn () in
        if count <= 1 then
          (* if there's only 1 more use left, there's no point
             in caching it
          *)
          (ranges, expls)
        else (
          (* otherwise, this is the first time we've seen this
             formula, and we should cache it
          *)
          replace formula_cache formula (Some (ranges, expls), count - 1);
          (ranges, expls))
    | Some (Some (ranges, expls), count) ->
        if count <= 1 then remove formula_cache formula;
        (ranges, expls)
end

type formula_cache = ((RM.t list * ME.t list) option * int) Formula_tbl.t

(* This function is for creating a formula cache which only caches formula that
   it knows will be shared, at least once, among the formula in a bunch of
   taint rules.

   This is because it's obviously not useful to cache a formula's matches if
   that formula never comes up again. This cache stores an option, with keys
   that are only formula that are guaranteed to appear more than once in the
   collection.
*)
let mk_specialized_formula_cache (rules : R.taint_rule list) =
  let count_tbl = Formula_tbl.create 128 in
  let flat_formulas =
    rules
    |> List.concat_map (fun rule ->
           let (`Taint (spec : R.taint_spec)) = rule.R.mode in
           Common.map (fun source -> source.R.source_formula) (snd spec.sources)
           @ Common.map
               (fun sanitizer -> sanitizer.R.sanitizer_formula)
               (match spec.sanitizers with
               | None -> []
               | Some (_, sanitizers) -> sanitizers)
           @ Common.map (fun sink -> sink.R.sink_formula) (snd spec.sinks)
           @ Common.map
               (fun propagator -> propagator.R.propagator_formula)
               spec.propagators)
  in
  flat_formulas
  |> List.iter (fun formula ->
         match Formula_tbl.find_opt count_tbl formula with
         | None -> Formula_tbl.add count_tbl formula (None, 1)
         | Some (_, x) -> Formula_tbl.replace count_tbl formula (None, 1 + x));
  (* We return the table with pairs of (None, count) itself.
     When we try to cache a find, we will first check whether decreasing this
     counter results in 0. Then, there are no more uses, and the result is no
     longer worth caching.
     This way we don't keep around entries when we don't need to.
  *)
  count_tbl

(*****************************************************************************)
(* Finding matches for taint specs *)
(*****************************************************************************)

(* =~ List.concat_map with automatic management of matching-explanations *)
let concat_map_with_expls f xs =
  let all_expls = ref [] in
  let res =
    xs
    |> List.concat_map (fun x ->
           let ys, expls = f x in
           Common.push expls all_expls;
           ys)
  in
  (res, List.flatten !all_expls)

let find_range_w_metas formula_cache (xconf : Match_env.xconfig)
    (xtarget : Xtarget.t) (rule : R.t) (specs : (R.formula * 'a) list) :
    (RM.t * 'a) list * ME.t list =
  (* TODO: Make an Or formula and run a single query. *)
  (* if perf is a problem, we could build an interval set here *)
  specs
  |> concat_map_with_expls (fun (pf, x) ->
         let ranges, expls =
           Formula_tbl.cached_find_opt formula_cache pf (fun () ->
               range_w_metas_of_formula xconf xtarget rule pf)
         in
         (ranges |> Common.map (fun rwm -> (rwm, x)), expls))

let find_sanitizers_matches formula_cache (xconf : Match_env.xconfig)
    (xtarget : Xtarget.t) (rule : R.t) (specs : R.taint_sanitizer list) :
    (bool * RM.t * R.taint_sanitizer) list * ME.t list =
  specs
  |> concat_map_with_expls (fun (sanitizer : R.taint_sanitizer) ->
         let ranges, expls =
           Formula_tbl.cached_find_opt formula_cache sanitizer.sanitizer_formula
             (fun () ->
               range_w_metas_of_formula xconf xtarget rule
                 sanitizer.sanitizer_formula)
         in
         ( ranges
           |> Common.map (fun x -> (sanitizer.R.not_conflicting, x, sanitizer)),
           expls ))

(* Finds all matches of `pattern-propagators`. *)
let find_propagators_matches formula_cache (xconf : Match_env.xconfig)
    (xtarget : Xtarget.t) (rule : R.t)
    (propagators_spec : R.taint_propagator list) =
  propagators_spec
  |> List.concat_map (fun (p : R.taint_propagator) ->
         let mvar_pfrom, tok_pfrom = p.from in
         let mvar_pto, tok_pto = p.to_ in
         let ranges_w_metavars, _expsTODO =
           Formula_tbl.cached_find_opt formula_cache p.propagator_formula
             (fun () ->
               range_w_metas_of_formula xconf xtarget rule p.propagator_formula)
         in
         (* Now, for each match of the propagator pattern, we try to construct
          * a `propagator_match`. We just need to look up what code is captured
          * by the metavariables `from` and `to`, and check if we can obtain good
          * location info for that code (i.e., we have real tokens rather than
          * fake ones). *)
         ranges_w_metavars
         |> Common.map_filter (fun rwm ->
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
                (* TODO: log a warning when we cannot obtain a taint propagator due to
                 * lacking range info. *)
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
                      Common.spf "propagator:%d:%d:%d:%d:%d:%d"
                        loc_pfrom.pos.bytepos loc_pto.pos.bytepos
                        from.Range.start from.Range.end_ to_.Range.start
                        to_.Range.end_
                    in
                    Some { id; rwm; from; to_; spec = p }))

(*****************************************************************************)
(* Testing whether some matches a taint spec *)
(*****************************************************************************)

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
        logger#trace
          "Cannot compute range, there are no real tokens in this AST: %s"
          (G.show_any any);
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

let is_exact_match ~match_range r =
  let r1 = match_range in
  let overlap =
    (* We want to know how well the AST node `any' is matching
       * the taint-annotated code range, this is a ratio in [0.0, 1.0]. *)
    float_of_int (r.Range.end_ - r.Range.start + 1)
    /. float_of_int (r1.Range.end_ - r1.Range.start + 1)
  in
  Range.( $<=$ ) r r1 && overlap > 0.99

let any_is_in_sources_matches rule any matches =
  let ( let* ) = option_bind_list in
  let* r = range_of_any any in
  matches
  |> Common.map_filter (fun (rwm, (ts : R.taint_source)) ->
         if Range.( $<=$ ) r rwm.RM.r then
           Some
             (let spec_pm = RM.range_to_pattern_match_adjusted rule rwm in
              let overlap = overlap_with ~match_range:rwm.RM.r r in
              {
                Taint_smatch.spec = ts;
                spec_id = ts.source_id;
                spec_pm;
                range = r;
                overlap;
              })
         else None)

(* Check whether `any` matches either the `from` or the `to` of any of the
 * `pattern-propagators`. Matches must be exact (overlap > 0.99) to make
 * taint propagation more precise and predictable. *)
let any_is_in_propagators_matches rule any matches :
    D.a_propagator Taint_smatch.t list =
  match range_of_any any with
  | None -> []
  | Some r ->
      matches
      |> List.concat_map (fun prop ->
             let var = prop.id in
             let spec_pm = RM.range_to_pattern_match_adjusted rule prop.rwm in
             let is_from = is_exact_match ~match_range:prop.from r in
             let is_to = is_exact_match ~match_range:prop.to_ r in
             let mk_match kind =
               let spec : D.a_propagator = { kind; prop = prop.spec; var } in
               {
                 Taint_smatch.spec;
                 spec_id = prop.spec.propagator_id;
                 spec_pm;
                 range = r;
                 overlap = 1.0;
               }
             in
             (if is_from then [ mk_match `From ] else [])
             @ (if is_to then [ mk_match `To ] else [])
             @ [])

let any_is_in_sanitizers_matches rule any matches =
  let ( let* ) = option_bind_list in
  let* r = range_of_any any in
  matches
  |> Common.map_filter (fun (rwm, spec) ->
         if Range.( $<=$ ) r rwm.RM.r then
           Some
             (let spec_pm = RM.range_to_pattern_match_adjusted rule rwm in
              let overlap = overlap_with ~match_range:rwm.RM.r r in
              {
                Taint_smatch.spec;
                spec_id = spec.R.sanitizer_id;
                spec_pm;
                range = r;
                overlap;
              })
         else None)

let any_is_in_sinks_matches rule any matches =
  let ( let* ) = option_bind_list in
  let* r = range_of_any any in
  matches
  |> Common.map_filter (fun (rwm, (spec : R.taint_sink)) ->
         if Range.( $<=$ ) r rwm.RM.r then
           Some
             (let spec_pm = RM.range_to_pattern_match_adjusted rule rwm in
              let overlap = overlap_with ~match_range:rwm.RM.r r in
              {
                Taint_smatch.spec;
                spec_id = spec.sink_id;
                spec_pm;
                range = r;
                overlap;
              })
         else None)

let lazy_force x = Lazy.force x [@@profiling]

(*****************************************************************************)
(* Main entry points *)
(*****************************************************************************)

let taint_config_of_rule ~per_file_formula_cache xconf file ast_and_errors
    ({ mode = `Taint spec; _ } as rule : R.taint_rule) handle_findings =
  let file = Fpath.v file in
  let formula_cache = per_file_formula_cache in
  let xconf = Match_env.adjust_xconfig_with_rule_options xconf rule.options in
  let lazy_ast_and_errors = lazy ast_and_errors in
  let xtarget =
    {
      Xtarget.file;
      xlang = rule.target_analyzer;
      lazy_content = lazy (File.read_file file);
      lazy_ast_and_errors;
    }
  in
  let (sources_ranges : (RM.t * R.taint_source) list), expls_sources =
    find_range_w_metas formula_cache xconf xtarget rule
      (spec.sources |> snd
      |> Common.map (fun (src : R.taint_source) -> (src.source_formula, src)))
  and (propagators_ranges : propagator_match list) =
    find_propagators_matches formula_cache xconf xtarget rule spec.propagators
  and (sinks_ranges : (RM.t * R.taint_sink) list), expls_sinks =
    find_range_w_metas formula_cache xconf xtarget rule
      (spec.sinks |> snd
      |> Common.map (fun (sink : R.taint_sink) -> (sink.sink_formula, sink)))
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
    |> Common.map_filter (fun (not_conflicting, range, spec) ->
           (* TODO: Warn user when we filter out a sanitizer? *)
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
        |> Common.map (fun (rwm, _) ->
               RM.range_to_pattern_match_adjusted rule rwm)
      in
      [
        {
          ME.op = Out.TaintSource;
          pos = fst spec.sources;
          children = expls_sources;
          matches = ranges_to_pms sources_ranges;
        };
        {
          ME.op = Out.TaintSink;
          pos = fst spec.sinks;
          children = expls_sinks;
          matches = ranges_to_pms sinks_ranges;
        }
        (* TODO: propagators *);
      ]
      @
      match spec.sanitizers with
      | None -> []
      | Some (tok, _) ->
          [
            {
              ME.op = Out.TaintSanitizer;
              pos = tok;
              children = expls_sanitizers;
              (* 'sanitizer_ranges' will be affected by `not-conflicting: true`:
               * if a sanitizer coincides exactly with a source/sink then it will
               * be filtered out. So the sanitizer matches may not be the union of
               * the matches of the individual sanitizers. Anyhow, not-conflicting
               * has been deprecated for quite some time, and we will remove it at
               * some point. *)
              matches = ranges_to_pms sanitizers_ranges;
            };
          ]
    else []
  in
  let config = xconf.config in
  ( {
      Dataflow_tainting.filepath = !!file;
      rule_id = fst rule.R.id;
      track_control =
        spec.sources |> snd
        |> List.exists (fun (src : R.taint_source) -> src.source_control);
      is_source = (fun x -> any_is_in_sources_matches rule x sources_ranges);
      is_propagator =
        (fun x -> any_is_in_propagators_matches rule x propagators_ranges);
      is_sanitizer =
        (fun x -> any_is_in_sanitizers_matches rule x sanitizers_ranges);
      is_sink = (fun x -> any_is_in_sinks_matches rule x sinks_ranges);
      unify_mvars = config.taint_unify_mvars;
      handle_findings;
    },
    {
      sources = sources_ranges;
      sanitizers = sanitizers_ranges |> Common.map fst;
      sinks = sinks_ranges;
    },
    expls )

let rec convert_taint_call_trace = function
  | Taint.PM (pm, _) ->
      let toks = Lazy.force pm.PM.tokens |> List.filter Tok.is_origintok in
      PM.Toks toks
  | Taint.Call (expr, toks, ct) ->
      PM.Call
        {
          call_toks =
            AST_generic_helpers.ii_of_any (G.E expr)
            |> List.filter Tok.is_origintok;
          intermediate_vars = toks;
          call_trace = convert_taint_call_trace ct;
        }

let pm_of_finding finding =
  match finding with
  | T.ToArg _
  | T.ToReturn _ ->
      None
  | ToSink
      {
        taints_with_precondition = taints, requires;
        sink = { pm = sink_pm; _ };
        merged_env;
      } ->
      if
        not
          (T.taints_satisfy_requires
             (Common.map (fun t -> t.T.taint) taints)
             requires)
      then None
      else
        (* We only report actual sources reaching a sink. If users want Semgrep to
         * report function parameters reaching a sink without sanitization, then
         * they need to specify the parameters as taint sources. *)
        let source_taints =
          taints
          |> Common.map_filter
               (fun { T.taint = { orig; tokens }; sink_trace } ->
                 match orig with
                 | Src src -> Some (src, tokens, sink_trace)
                 (* even if there is any taint "variable", it's irrelevant for the
                  * finding, since the precondition is satisfied. *)
                 | Arg _
                 | Control ->
                     None)
        in
        let rec find_requires = function
          | Taint.PM (_, src) -> src.R.source_requires
          | Taint.Call (_, _, ct) -> find_requires ct
        in
        (* We prioritize taint sources without preconditions,
           selecting their traces first, and then consider sources
           with preconditions as a secondary choice. When we generate
           JSON output for the command-line interface, we arbitrarily
           select the first trace in the list. Consequently, when
           there are multiple sources, and their traces overlap,
           leading to the same sink, the final output doesn't always
           indicate the initial location of the tainted source
           clearly. By following this approach, users are more likely
           to identify the very first taint source that doesn't rely
           on other sources as input. *)
        let with_req, without_req =
          source_taints
          |> Common.partition_either (fun (src, tokens, sink_trace) ->
                 match find_requires src.T.call_trace with
                 | Some _ -> Left (src, tokens, sink_trace)
                 | None -> Right (src, tokens, sink_trace))
        in
        let source_taints =
          if without_req <> [] then without_req
          else (
            logger#warning
              "Taint source without precondition wasn't found. Displaying the \
               taint trace from the source with precondition.";
            with_req)
        in
        (* The old behavior used to be that, for sinks with a `requires`, we would
           generate a finding per every single taint source going in. Later deduplication
           would deal with it.
           We will instead choose to consolidate all sources into a single finding. We can
           do some postprocessing to report only relevant sources later on, but for now we
           will lazily (again) defer that computation to later.
        *)
        let traces =
          source_taints
          |> Common.map (fun (src, tokens, sink_trace) ->
                 {
                   PM.source_trace = convert_taint_call_trace src.T.call_trace;
                   tokens;
                   sink_trace = convert_taint_call_trace sink_trace;
                 })
        in
        (* We always report the finding on the sink that gets tainted, the call trace
            * must be used to explain how exactly the taint gets there. At some point
            * we experimented with reporting the match on the `sink`'s function call that
            * leads to the actual sink. E.g.:
            *
            *     def f(x):
            *       sink(x)
            *
            *     def g():
            *       f(source)
            *
            * Here we tried reporting the match on `f(source)` as "the line to blame"
            * for the injection bug... but most users seem to be confused about this. They
            * already expect Semgrep (and DeepSemgrep) to report the match on `sink(x)`.
        *)
        let taint_trace = Some (lazy traces) in
        Some { sink_pm with env = merged_env; taint_trace }

let check_var_def lang options taint_config env id ii expr =
  let name = AST_to_IL.var_of_id_info id ii in
  let assign =
    G.Assign (G.N (G.Id (id, ii)) |> G.e, Tok.fake_tok (snd id) "=", expr)
    |> G.e |> G.exprstmt
  in
  let xs = AST_to_IL.stmt lang assign in
  let flow = CFG_build.cfg_of_stmts xs in
  let end_mapping =
    let java_props_cache = D.mk_empty_java_props_cache () in
    Dataflow_tainting.fixpoint ~in_env:env lang options taint_config
      java_props_cache flow
  in
  let out_env = end_mapping.(flow.exit).Dataflow_core.out_env in
  let lval : IL.lval = { base = Var name; rev_offset = [] } in
  Lval_env.dumb_find out_env lval

let add_to_env lang options taint_config env id ii opt_expr =
  let var = AST_to_IL.var_of_id_info id ii in
  let var_type = Typing.resolved_type_of_id_info lang var.id_info in
  let id_taints =
    taint_config.D.is_source (G.Tk (snd id))
    |> Common.map (fun (x : _ Taint_smatch.t) -> (x.spec_pm, x.spec))
    (* These sources come from the parameters to a function,
        which are not within the normal control flow of a code.
        We can safely say there's no incoming taints to these sources.
    *)
    |> T.taints_of_pms ~incoming:T.Taint_set.empty
  in
  let expr_taints =
    match opt_expr with
    | Some e -> (
        match check_var_def lang options taint_config env id ii e with
        | `None
        | `Clean ->
            T.Taint_set.empty
        | `Tainted taints -> taints)
    | None -> T.Taint_set.empty
  in
  let taints = id_taints |> T.Taint_set.union expr_taints in
  let taints =
    Dataflow_tainting.drop_taints_if_bool_or_number options taints var_type
  in
  Lval_env.add env (IL_helpers.lval_of_var var) taints

let mk_fun_input_env lang options taint_config ?(glob_env = Lval_env.empty) fdef
    =
  let add_to_env = add_to_env lang options taint_config in
  let in_env =
    (* For each argument, check if it's a source and, if so, add it to the input
     * environment. *)
    List.fold_left
      (fun env par ->
        match par with
        | G.Param { pname = Some id; pinfo; pdefault; _ } ->
            add_to_env env id pinfo pdefault
        (* JS: {arg} : type *)
        | G.ParamPattern
            (G.OtherPat
              ( ("ExprToPattern", _),
                [
                  G.E
                    { e = G.Cast (_, _, { e = G.Record (_, fields, _); _ }); _ };
                ] ))
        (* JS: {arg} *)
        | G.ParamPattern
            (G.OtherPat
              (("ExprToPattern", _), [ G.E { e = G.Record (_, fields, _); _ } ]))
          ->
            List.fold_left
              (fun env field ->
                match field with
                | G.F
                    {
                      s =
                        G.DefStmt
                          ( _,
                            G.FieldDefColon
                              { vinit = Some { e = G.N (G.Id (id, ii)); _ }; _ }
                          );
                      _;
                    } ->
                    add_to_env env id ii None
                | G.F _ -> env)
              env fields
        | G.ParamPattern pat ->
            (* Here, we just get all the identifiers in the pattern, which may
               themselves be sources.
               This is so we can handle patterns such as:
               (x, (y, z, (a, b)))
               and taint all the inner identifiers
            *)
            let ids = Visit_pattern_ids.visit (G.P pat) in
            List.fold_left
              (fun env (id, pinfo) -> add_to_env env id pinfo None)
              env ids
        | G.Param { pname = None; _ }
        | G.ParamRest (_, _)
        | G.ParamHashSplat (_, _)
        | G.ParamEllipsis _
        | G.ParamReceiver _
        | G.OtherParam (_, _) ->
            env)
      glob_env
      (Tok.unbracket fdef.G.fparams)
  in
  in_env

let is_global (id_info : G.id_info) =
  let* kind, _sid = !(id_info.id_resolved) in
  Some (H.name_is_global kind)

let mk_file_env lang options taint_config ast =
  let add_to_env = add_to_env lang options taint_config in
  let env = ref Lval_env.empty in
  let visitor =
    object (_self : 'self)
      inherit [_] G.iter_no_id_info as super

      method! visit_definition env (entity, def_kind) =
        match (entity, def_kind) with
        | { name = EN (Id (id, id_info)); _ }, VarDef { vinit; _ }
          when IdFlags.is_final !(id_info.id_flags)
               && is_global id_info =*= Some true ->
            env := add_to_env !env id id_info vinit
        | __else__ -> super#visit_definition env (entity, def_kind)

      method! visit_Assign env lhs tok expr =
        match lhs with
        | {
         e =
           ( N (Id (id, id_info))
           | DotAccess
               ( { e = IdSpecial ((This | Self), _); _ },
                 _,
                 FN (Id (id, id_info)) ) );
         _;
        }
          when IdFlags.is_final !(id_info.id_flags)
               && is_global id_info =*= Some true ->
            env := add_to_env !env id id_info (Some expr)
        | __else__ -> super#visit_Assign env lhs tok expr
    end
  in
  visitor#visit_program env ast;
  !env

let check_fundef lang options taint_config opt_ent ctx ?glob_env
    java_props_cache fdef =
  let name =
    let* ent = opt_ent in
    let* name = AST_to_IL.name_of_entity ent in
    Some (IL.str_of_name name)
  in
  let _, xs = AST_to_IL.function_definition lang ~ctx fdef in
  let flow = CFG_build.cfg_of_stmts xs in
  let in_env = mk_fun_input_env lang options taint_config ?glob_env fdef in
  let mapping =
    Dataflow_tainting.fixpoint ~in_env ?name lang options taint_config
      java_props_cache flow
  in
  (flow, mapping)

let check_rule per_file_formula_cache (rule : R.taint_rule) match_hook
    (xconf : Match_env.xconfig) (xtarget : Xtarget.t) =
  let matches = ref [] in

  let { Xtarget.file; xlang; lazy_ast_and_errors; _ } = xtarget in
  let lang =
    match xlang with
    | L (lang, _) -> lang
    | LSpacegrep
    | LAliengrep
    | LRegex ->
        failwith "taint-mode and generic/regex matching are incompatible"
  in
  let (ast, skipped_tokens), parse_time =
    Common.with_time (fun () -> lazy_force lazy_ast_and_errors)
  in
  (* TODO: 'debug_taint' should just be part of 'res'
     * (i.e., add a "debugging" field to 'Report.match_result'). *)
  let taint_config, _TODO_debug_taint, expls =
    let handle_findings _ findings _env =
      findings
      |> List.iter (fun finding ->
             pm_of_finding finding
             |> Option.iter (fun pm -> Common.push pm matches))
    in
    taint_config_of_rule ~per_file_formula_cache xconf !!file (ast, []) rule
      handle_findings
  in

  (match !hook_setup_hook_function_taint_signature with
  | None -> ()
  | Some setup_hook_function_taint_signature ->
      setup_hook_function_taint_signature xconf rule taint_config xtarget);

  (* FIXME: This is no longer needed, now we can just check the type 'n'. *)
  let ctx = ref AST_to_IL.empty_ctx in
  Visit_function_defs.visit
    (fun opt_ent _ ->
      match opt_ent with
      | Some { name = EN (Id (n, _)); _ } ->
          ctx := AST_to_IL.add_entity_name !ctx n
      | __else__ -> ())
    ast;

  let java_props_cache = Dataflow_tainting.mk_empty_java_props_cache () in

  let glob_env = mk_file_env lang xconf.config taint_config ast in

  (* Check each function definition. *)
  Visit_function_defs.visit
    (fun opt_ent fdef ->
      check_fundef lang xconf.config taint_config opt_ent !ctx ~glob_env
        java_props_cache fdef
      |> ignore)
    ast;

  (* Check execution of statements during object initialization. *)
  Visit_class_defs.visit
    (fun _ cdef ->
      let fields =
        cdef.G.cbody |> Tok.unbracket
        |> Common.map (function G.F x -> x)
        |> G.stmt1
      in
      let stmts = AST_to_IL.stmt lang fields in
      let flow = CFG_build.cfg_of_stmts stmts in
      Dataflow_tainting.fixpoint lang xconf.config taint_config java_props_cache
        flow
      |> ignore)
    ast;

  (* Check the top-level statements.
   * In scripting languages it is not unusual to write code outside
   * function declarations and we want to check this too. We simply
   * treat the program itself as an anonymous function. *)
  let (), match_time =
    Common.with_time (fun () ->
        let xs = AST_to_IL.stmt lang (G.stmt1 ast) in
        let flow = CFG_build.cfg_of_stmts xs in
        Dataflow_tainting.fixpoint lang xconf.config taint_config
          java_props_cache flow
        |> ignore)
  in
  let matches =
    !matches
    (* same post-processing as for search-mode in Match_rules.ml *)
    |> PM.uniq
    |> PM.no_submatches (* see "Taint-tracking via ranges" *)
    |> Common.before_return (fun v ->
           v
           |> List.iter (fun (m : Pattern_match.t) ->
                  let str =
                    Common.spf "with rule %s" (Rule_ID.to_string m.rule_id.id)
                  in
                  match_hook str m))
    |> Common.map (fun m -> { m with PM.rule_id = convert_rule_id rule.R.id })
  in
  let errors = Parse_target.errors_from_skipped_tokens skipped_tokens in
  let report =
    RP.make_match_result matches errors
      { Core_profiling.rule_id = fst rule.R.id; parse_time; match_time }
  in
  let explanations =
    if xconf.matching_explanations then
      [
        {
          ME.op = Out.Taint;
          children = expls;
          matches = report.matches;
          pos = snd rule.id;
        };
      ]
    else []
  in
  let report = { report with explanations } in
  report

let check_rules ~match_hook
    ~(per_rule_boilerplate_fn :
       R.rule ->
       (unit -> Core_profiling.rule_profiling Core_result.match_result) ->
       Core_profiling.rule_profiling Core_result.match_result)
    (rules : R.taint_rule list) (xconf : Match_env.xconfig)
    (xtarget : Xtarget.t) :
    Core_profiling.rule_profiling Core_result.match_result list =
  (* We create a "formula cache" here, before dealing with individual rules, to
     permit sharing of matches for sources, sanitizers, propagators, and sinks
     between rules.

     In particular, this expects to see big gains due to shared propagators,
     in Semgrep Pro. There may be some benefit in OSS, but it's low-probability.
  *)
  let per_file_formula_cache = mk_specialized_formula_cache rules in

  rules
  |> Common.map (fun rule ->
         let xconf =
           Match_env.adjust_xconfig_with_rule_options xconf rule.R.options
         in
         (* This boilerplate function will take care of things like
            timing out if this rule takes too long, and returning a dummy
            result for the timed-out rule.
         *)
         per_rule_boilerplate_fn
           (rule :> R.rule)
           (fun () ->
             check_rule per_file_formula_cache rule match_hook xconf xtarget))
