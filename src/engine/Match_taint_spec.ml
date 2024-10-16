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
open Fpath_.Operators (* e.g. !! *)
module Log = Log_tainting.Log
module G = AST_generic
module ME = Matching_explanation
module MV = Metavariable
module R = Rule
module RM = Range_with_metavars
module OutJ = Semgrep_output_v1_t
module D = Dataflow_tainting

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
  id : D.var;
  rwm : RM.t;
  from : Range.t;
  to_ : Range.t;
  spec : R.taint_propagator;
}

type spec_matches = {
  sources : (RM.t * R.taint_source) list;
  propagators : propagator_match list;
  sanitizers : (RM.t * R.taint_sanitizer) list;
  sinks : (RM.t * R.taint_sink) list;
}

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
         |> List_.filter_map (fun rwm ->
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
(* Spec matches *)
(*****************************************************************************)

let spec_matches_of_taint_rule ~per_file_formula_cache xconf file ast_and_errors
    ({ mode = `Taint spec; _ } as rule : R.taint_rule) =
  let file = Fpath.v file in
  let formula_cache = per_file_formula_cache in
  let xconf = Match_env.adjust_xconfig_with_rule_options xconf rule.options in
  let lazy_ast_and_errors = lazy ast_and_errors in
  (* TODO: should this function just take a target, rather than a file? *)
  let xtarget : Xtarget.t =
    {
      path = { origin = File file; internal_path_to_content = file };
      xlang = rule.target_analyzer;
      lazy_content = lazy (UFile.read_file file);
      lazy_ast_and_errors;
    }
  in
  let (sources_ranges : (RM.t * R.taint_source) list), expls_sources =
    find_range_w_metas formula_cache xconf xtarget rule
      (spec.sources |> snd
      |> List_.map (fun (src : R.taint_source) -> (src.source_formula, src)))
  and (propagators_ranges : propagator_match list) =
    find_propagators_matches formula_cache xconf xtarget rule spec.propagators
  and (sinks_ranges : (RM.t * R.taint_sink) list), expls_sinks =
    find_range_w_metas formula_cache xconf xtarget rule
      (spec.sinks |> snd
      |> List_.map (fun (sink : R.taint_sink) -> (sink.sink_formula, sink)))
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
    |> List_.filter_map (fun (not_conflicting, range, spec) ->
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
      sources = sources_ranges;
      propagators = propagators_ranges;
      sanitizers = sanitizers_ranges;
      sinks = sinks_ranges;
    },
    expls )

(*****************************************************************************)
(* Testing whether a an AST node matches a taint spec *)
(*****************************************************************************)

let option_bind_list opt f =
  match opt with
  | None -> []
  | Some x -> f x

module Range_table = struct
  module T = Hashtbl.Make (struct
    type t = Range.t

    let equal = Range.equal
    let hash = Hashtbl.hash
  end)

  let create () = T.create 100

  let push tbl k v =
    match T.find_opt tbl k with
    | None -> T.add tbl k [ v ]
    | Some vs -> T.replace tbl k (v :: vs)

  let get tbl k =
    match T.find_opt tbl k with
    | None -> []
    | Some vs -> vs
end

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

let any_is_in_sources_matches rule any matches =
  let ( let* ) = option_bind_list in
  let* r = range_of_any any in
  matches
  |> List_.filter_map (fun (rwm, (ts : R.taint_source)) ->
         if Range.( $<=$ ) r rwm.RM.r then
           Some
             (let spec_pm = RM.range_to_pattern_match_adjusted rule rwm in
              let overlap = overlap_with ~match_range:rwm.RM.r r in
              {
                Taint_spec_match.spec = ts;
                spec_id = ts.source_id;
                spec_pm;
                range = r;
                overlap;
              })
         else None)

(* Builds a table to quickly look up for propagator matches, taking advantage of
 * the requirement that propagators must be exact matches.
 *
 * OBS: Previously we allowed these matches if they had an overlap of >0.99, but
 *   to be honest that was arbitrary and fragile (the overlap largely depends on
 *   the amount of text being matched). Typically propagators from/to match
 *   l-values and those typically we can count as being 100% perfect matches.
 *   If this causes problems we can always roll back, but all tests still work.
 *)
let propagators_table_of_matches rule matches =
  let mk_match (prop : propagator_match) var kind r =
    let spec_pm = RM.range_to_pattern_match_adjusted rule prop.rwm in
    let spec : D.a_propagator = { kind; prop = prop.spec; var } in
    {
      Taint_spec_match.spec;
      spec_id = prop.spec.propagator_id;
      spec_pm;
      range = r;
      overlap = 1.0;
    }
  in
  let tbl = Range_table.create () in
  matches
  |> List.iter (fun (prop : propagator_match) ->
         let var = prop.id in
         Range_table.push tbl prop.to_ (mk_match prop var `To prop.to_);
         Range_table.push tbl prop.from (mk_match prop var `From prop.from));
  tbl

(* Check whether `any` matches either the `from` or the `to` of any of the
 * `pattern-propagators`. Matches must be exact to make taint propagation
 * more precise and predictable.
 *
 * THINK: Now that we have "Best_matches" we could perhaps use that for
 *   propagators too?
 *)
let any_is_in_propagators_matches any tbl :
    D.a_propagator Taint_spec_match.t list =
  match range_of_any any with
  | None -> []
  | Some r -> Range_table.get tbl r

let any_is_in_sanitizers_matches rule any matches =
  let ( let* ) = option_bind_list in
  let* r = range_of_any any in
  matches
  |> List_.filter_map (fun (rwm, spec) ->
         if Range.( $<=$ ) r rwm.RM.r then
           Some
             (let spec_pm = RM.range_to_pattern_match_adjusted rule rwm in
              let overlap = overlap_with ~match_range:rwm.RM.r r in
              {
                Taint_spec_match.spec;
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
  |> List_.filter_map (fun (rwm, (spec : R.taint_sink)) ->
         if Range.( $<=$ ) r rwm.RM.r then
           Some
             (let spec_pm = RM.range_to_pattern_match_adjusted rule rwm in
              let overlap = overlap_with ~match_range:rwm.RM.r r in
              {
                Taint_spec_match.spec;
                spec_id = spec.sink_id;
                spec_pm;
                range = r;
                overlap;
              })
         else None)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let default_effect_handler _fun_name new_effects = new_effects

let taint_config_of_rule ~per_file_formula_cache
    ?(handle_effects = default_effect_handler) xconf file ast_and_errors
    ({ mode = `Taint spec; _ } as rule : R.taint_rule) =
  let spec_matches, expls =
    spec_matches_of_taint_rule ~per_file_formula_cache xconf file ast_and_errors
      rule
  in
  let file = Fpath.v file in
  let xconf = Match_env.adjust_xconfig_with_rule_options xconf rule.options in
  let propagators_tbl =
    propagators_table_of_matches rule spec_matches.propagators
  in
  let config = xconf.config in
  ( {
      Dataflow_tainting.filepath = !!file;
      rule_id = fst rule.R.id;
      track_control =
        spec.sources |> snd
        |> List.exists (fun (src : R.taint_source) -> src.source_control);
      is_source =
        (fun x -> any_is_in_sources_matches rule x spec_matches.sources);
      is_propagator = (fun x -> any_is_in_propagators_matches x propagators_tbl);
      is_sanitizer =
        (fun x -> any_is_in_sanitizers_matches rule x spec_matches.sanitizers);
      is_sink = (fun x -> any_is_in_sinks_matches rule x spec_matches.sinks);
      unify_mvars = config.taint_unify_mvars;
      handle_effects;
    },
    spec_matches,
    expls )
[@@trace_trace]
