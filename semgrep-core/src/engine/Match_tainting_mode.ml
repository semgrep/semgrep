(* Yoann Padioleau, Iago Abal
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

module D = Dataflow_tainting
module G = AST_generic
module H = AST_generic_helpers
module V = Visitor_AST
module R = Rule
module PM = Pattern_match
module RM = Range_with_metavars
module RP = Report
module T = Taint
module PI = Parse_info

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
 * expression `sink("$source" . 'here')` will be transalted to IL as two
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
  sources : RM.ranges;
  sanitizers : RM.ranges;
  sinks : RM.ranges;
}

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

let convert_rule_id (id, _tok) = { PM.id; message = ""; pattern_string = id }

(* Finds all matches of a taint-spec pattern formula. *)
let range_w_metas_of_pformula config equivs xtarget rule pformula =
  let rule_id = fst rule.R.id in
  let formula = Rule.formula_of_pformula ~rule_id pformula in
  (* !! Calling Match_search_mode here !! *)
  Match_search_mode.matches_of_formula (config, equivs) rule xtarget formula
    None
  |> snd

type propagator_match = {
  id : D.var;
      (** An unique identifier for the propagator match. This is used as an
   * auxiliary variable to store the taints flowing from `from` to `to`. *)
  from : Range.t;  (** The range matched by the `from` metavariable. *)
  to_ : Range.t;  (** The range matched by the `to` metavariable. *)
}
(** Taint will flow from `from` to `to_` through the axiliary variable `id`. *)

(* Finds all matches of `pattern-propagators`. *)
let find_propagators_ranges config equivs xtarget rule propagators_spec =
  let ( let* ) = Option.bind in
  let module MV = Metavariable in
  propagators_spec
  |> List.concat_map (fun (p : Rule.taint_propagator) ->
         let mvar_pfrom, tok_pfrom = p.from in
         let mvar_pto, tok_pto = p.to_ in
         let ranges_w_metavars =
           range_w_metas_of_pformula config equivs xtarget rule p.formula
         in
         (* Now, for each match of the propagator pattern, we try to construct
          * a `propagator_match`. We just need to look up what code is captured
          * by the metavariables `from` and `to`, and check if we can obtain good
          * location info for that code (i.e., we have real tokens rather than
          * fake ones). *)
         ranges_w_metavars
         |> List.filter_map (fun rwm ->
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
                match
                  Parse_info.
                    ( token_location_of_info tok_pfrom,
                      token_location_of_info tok_pto )
                with
                | Error _, _
                | _, Error _ ->
                    None
                | Ok loc_pfrom, Ok loc_pto ->
                    let* mval_from_start_loc, mval_from_end_loc =
                      Visitor_AST.range_of_any_opt (MV.mvalue_to_any mval_from)
                    in
                    let* mval_to_start_loc, mval_to_end_loc =
                      Visitor_AST.range_of_any_opt (MV.mvalue_to_any mval_to)
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
                        loc_pfrom.charpos loc_pto.charpos from.Range.start
                        from.Range.end_ to_.Range.start to_.Range.end_
                    in
                    Some { id; from; to_ }))

(* Check whether `any` is in one or more taint-spec `matches`, e.g. whether
 * `any` is a `pattern-source`. *)
let any_is_in_matches any ~p ~f matches =
  (* This is potentially slow. We may need to store range position in
   * the AST at some point. *)
  match Visitor_AST.range_of_any_opt any with
  | None ->
      (* IL.any_of_orig will return `G.Anys []` for `NoOrig`, and there is
       * no point in issuing this warning in that case.
       * TODO: Perhaps we should avoid the call to `any_in_ranges` in the
       * first place? *)
      if any <> G.Anys [] then
        logger#warning
          "Cannot compute range, there are no real tokens in this AST: %s"
          (G.show_any any);
      []
  | Some (tok1, tok2) ->
      let r = Range.range_of_token_locations tok1 tok2 in
      List.filter (p r) matches |> Common.map (f r)

(* Check whether `any` is in one or more taint-spec `matches` given by
 * a list of `Range_with_metavars.t`. For each match, it returns the
 * corresponding `Pattern_match.t`. *)
let any_is_in_ranges_w_metavars rule any matches =
  let p r rwm = Range.( $<=$ ) r rwm.RM.r in
  let f _r rwm = RM.range_to_pattern_match_adjusted rule rwm in
  any_is_in_matches any ~p ~f matches

(* Same as `any_is_in_ranges_w_metavars` but it also returns the
 * overlap (a float in [0.0, 1.0] that indicates how much overlap
 * there is between `any` and the taint-spec). The overlap is
 * used for side-effectful propagation of taint, which requires
 * a perfect match (overlap > 0.99). *)
let any_is_in_ranges_w_metavars_overlap rule any xs =
  let p r rwm = Range.( $<=$ ) r rwm.RM.r in
  let f r rwm =
    let r1 = rwm.RM.r in
    let overlap =
      (* We want to know how well the AST node `any' is matching
         * the taint-annotated code range, this is a ratio in [0.0, 1.0]. *)
      float_of_int (r.Range.end_ - r.Range.start + 1)
      /. float_of_int (r1.Range.end_ - r1.Range.start + 1)
    in
    let pm = RM.range_to_pattern_match_adjusted rule rwm in
    (pm, overlap)
  in
  any_is_in_matches any ~p ~f xs

(* Check whether `any` matches either the `from` or the `to` of any of the
 * `pattern-propagators`. Matches must be exact (overlap > 0.99) to make
 * taint propagation more precise and predictable. *)
let any_is_in_propagators_matches any matches :
    D.propagator_from list * D.propagator_to list =
  let mk_p ~get_range r x =
    let r1 = get_range x in
    let overlap =
      (* We want to know how well the AST node `any' is matching
       * the taint-annotated code range, this is a ratio in [0.0, 1.0]. *)
      float_of_int (r.Range.end_ - r.Range.start + 1)
      /. float_of_int (r1.Range.end_ - r1.Range.start + 1)
    in
    Range.( $<=$ ) r r1 && overlap > 0.99
  in
  let matches_pfrom =
    let get_range x = x.from in
    let p = mk_p ~get_range in
    let f _r x = x.id in
    any_is_in_matches any ~p ~f matches
  in
  let matches_pto =
    let get_range x = x.to_ in
    let p = mk_p ~get_range in
    let f _r x = x.id in
    any_is_in_matches any ~p ~f matches
  in
  (matches_pfrom, matches_pto)

let lazy_force x = Lazy.force x [@@profiling]

(*****************************************************************************)
(* Main entry points *)
(*****************************************************************************)

let taint_config_of_rule default_config equivs file ast_and_errors
    ({ mode = `Taint spec; _ } as rule : R.taint_rule) handle_findings =
  let config = Common.( ||| ) rule.options default_config in
  let lazy_ast_and_errors = lazy ast_and_errors in
  let xtarget =
    {
      Xtarget.file;
      xlang = rule.languages;
      lazy_content = lazy (Common.read_file file);
      lazy_ast_and_errors;
    }
  in
  let find_range_w_metas pfs =
    (* TODO: Make an Or formula and run a single query. *)
    (* if perf is a problem, we could build an interval set here *)
    pfs
    |> List.concat_map (range_w_metas_of_pformula config equivs xtarget rule)
  in
  let find_range_w_metas_santizers specs =
    specs
    |> List.concat_map (fun spec ->
           Common.map
             (fun pf -> (spec.Rule.not_conflicting, pf))
             (range_w_metas_of_pformula config equivs xtarget rule spec.pformula))
  in
  let sources_ranges = find_range_w_metas spec.sources
  and propagators_ranges =
    find_propagators_ranges config equivs xtarget rule spec.propagators
  and sinks_ranges = find_range_w_metas spec.sinks in
  let sanitizers_ranges =
    find_range_w_metas_santizers spec.sanitizers
    (* A sanitizer cannot conflict with a sink or a source, otherwise it is
     * filtered out. This allows to e.g. declare `$F(...)` as a sanitizer,
     * to assume that any other function will handle tainted data safely.
     * Without this, `$F(...)` will automatically sanitize any other function
     * call acting as a sink or a source. *)
    |> List.filter_map (fun (not_conflicting, range) ->
           (* TODO: Warn user when we filter out a sanitizer? *)
           if not_conflicting then
             if
               not
                 (List.exists
                    (fun range' -> range'.RM.r = range.RM.r)
                    sinks_ranges
                 || List.exists
                      (fun range' -> range'.RM.r = range.RM.r)
                      sources_ranges)
             then Some range
             else None
           else Some range)
  in
  ( {
      Dataflow_tainting.filepath = file;
      rule_id = fst rule.R.id;
      is_source =
        (fun x -> any_is_in_ranges_w_metavars_overlap rule x sources_ranges);
      is_propagator =
        (fun x -> any_is_in_propagators_matches x propagators_ranges);
      is_sanitizer =
        (fun x -> any_is_in_ranges_w_metavars_overlap rule x sanitizers_ranges);
      is_sink = (fun x -> any_is_in_ranges_w_metavars rule x sinks_ranges);
      unify_mvars = config.taint_unify_mvars;
      handle_findings;
    },
    {
      sources = sources_ranges;
      sanitizers = sanitizers_ranges;
      sinks = sinks_ranges;
    } )

let rec convert_taint_call_trace = function
  | Taint.PM pm ->
      let toks = Lazy.force pm.PM.tokens |> List.filter PI.is_origintok in
      PM.Toks toks
  | Taint.Call (expr, toks, ct) ->
      PM.Call
        {
          call_toks = V.ii_of_any (G.E expr) |> List.filter PI.is_origintok;
          intermediate_vars = toks;
          call_trace = convert_taint_call_trace ct;
        }

let taint_trace_of_src_to_sink source tokens sink =
  {
    Pattern_match.source = convert_taint_call_trace source;
    tokens;
    sink = convert_taint_call_trace sink;
  }

let pm_of_finding finding =
  match finding with
  | T.SrcToSink { source; tokens; sink; merged_env } ->
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
      let taint_trace =
        Some (lazy (taint_trace_of_src_to_sink source tokens sink))
      in
      let sink_pm = T.pm_of_trace sink in
      Some { sink_pm with env = merged_env; taint_trace }
  | T.SrcToReturn _
  (* TODO: We might want to report functions that let input taint
   * go into a sink (?) *)
  | T.ArgToSink _
  | T.ArgToReturn _ ->
      None

let check_rule rule match_hook (default_config, equivs) xtarget =
  let matches = ref [] in

  let { Xtarget.file; xlang; lazy_ast_and_errors; _ } = xtarget in
  let lang =
    match xlang with
    | L (lang, _) -> lang
    | LGeneric
    | LRegex ->
        failwith "taint-mode and generic/regex matching are incompatible"
  in
  let (ast, skipped_tokens), parse_time =
    Common.with_time (fun () -> lazy_force lazy_ast_and_errors)
  in
  let taint_config, debug_taint =
    let handle_findings _ findings _env =
      findings
      |> List.iter (fun finding ->
             pm_of_finding finding
             |> Option.iter (fun pm -> Common.push pm matches))
    in
    taint_config_of_rule default_config equivs file (ast, []) rule
      handle_findings
  in

  let fun_env = Hashtbl.create 8 in

  (* Check the top-level statements.
   * In scripting languages it is not unusual to write code outside
   * function declarations and we want to check this too. We simply
   * treat the program itself as an anonymous function. *)
  let (), match_time =
    Common.with_time (fun () ->
        let xs = AST_to_IL.stmt lang (G.stmt1 ast) in
        let flow = CFG_build.cfg_of_stmts lang xs in
        Dataflow_tainting.fixpoint ~fun_env taint_config flow |> ignore)
  in

  let matches =
    !matches
    (* same post-processing as for search-mode in Match_rules.ml *)
    |> PM.uniq
    |> PM.no_submatches (* see "Taint-tracking via ranges" *)
    |> Common.before_return (fun v ->
           v
           |> List.iter (fun (m : Pattern_match.t) ->
                  let str = Common.spf "with rule %s" m.rule_id.id in
                  match_hook str m))
    |> Common.map (fun m ->
           { m with PM.rule_id = convert_rule_id rule.Rule.id })
  in
  let errors = Parse_target.errors_from_skipped_tokens skipped_tokens in
  ( RP.make_match_result matches errors
      { RP.rule_id = fst rule.Rule.id; parse_time; match_time },
    debug_taint )
