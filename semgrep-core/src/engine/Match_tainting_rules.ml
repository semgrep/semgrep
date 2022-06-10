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

let any_in_ranges rule any rwms =
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
      List.filter (fun rwm -> Range.( $<=$ ) r rwm.RM.r) rwms
      |> Common.map (fun rwm ->
             let r1 = rwm.RM.r in
             let overlap =
               (* We want to know how well the AST node `any' is matching
                * the taint-annotated code range, this is a ratio in [0.0, 1.0]. *)
               float_of_int (r.Range.end_ - r.Range.start + 1)
               /. float_of_int (r1.Range.end_ - r1.Range.start + 1)
             in
             let pm = RM.range_to_pattern_match_adjusted rule rwm in
             (pm, overlap))

let any_in_ranges_no_overlap rule any rwms =
  any_in_ranges rule any rwms |> Common.map fst

let range_w_metas_of_pformula config equivs file_and_more rule pformula =
  let rule_id = fst rule.R.id in
  let formula = Rule.formula_of_pformula ~rule_id pformula in
  Match_search_rules.matches_of_formula (config, equivs) rule file_and_more
    formula None
  |> snd

let lazy_force x = Lazy.force x [@@profiling]
let ( let* ) = Option.bind

(*****************************************************************************)
(* Main entry points *)
(*****************************************************************************)

let taint_config_of_rule default_config equivs file ast_and_errors
    (rule : R.rule) (spec : R.taint_spec) handle_findings =
  let config = Common.( ||| ) rule.options default_config in
  let lazy_ast_and_errors = lazy ast_and_errors in
  let file_and_more =
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
    |> List.concat_map
         (range_w_metas_of_pformula config equivs file_and_more rule)
  in
  let find_range_w_metas_santizers specs =
    specs
    |> List.concat_map (fun spec ->
           Common.map
             (fun pf -> (spec.Rule.not_conflicting, pf))
             (range_w_metas_of_pformula config equivs file_and_more rule
                spec.pformula))
  in
  let sources_ranges = find_range_w_metas spec.sources
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
      is_source = (fun x -> any_in_ranges rule x sources_ranges);
      is_sanitizer = (fun x -> any_in_ranges rule x sanitizers_ranges);
      is_sink = (fun x -> any_in_ranges_no_overlap rule x sinks_ranges);
      unify_mvars = config.taint_unify_mvars;
      handle_findings;
    },
    {
      sources = sources_ranges;
      sanitizers = sanitizers_ranges;
      sinks = sinks_ranges;
    } )

let pm_of_finding finding =
  match finding with
  | T.SrcToSink { source = _; tokens = _; sink; merged_env } ->
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
      let sink_pm = T.pm_of_trace sink in
      Some { sink_pm with env = merged_env }
  | T.SrcToReturn _
  (* TODO: We might want to report functions that let input taint
   * go into a sink (?) *)
  | T.ArgToSink _
  | T.ArgToReturn _ ->
      None

let check_stmt ?in_env ?name lang fun_env taint_config def_body =
  let xs = AST_to_IL.stmt lang def_body in
  let flow = CFG_build.cfg_of_stmts xs in
  let mapping =
    Dataflow_tainting.fixpoint ?in_env ?name ~fun_env taint_config flow
  in
  ignore mapping

let check_fundef lang fun_env taint_config opt_ent fdef =
  let name =
    let* ent = opt_ent in
    let* name = AST_to_IL.name_of_entity ent in
    Some (D.str_of_name name)
  in
  let add_to_env env id ii =
    let var = D.str_of_name (AST_to_IL.var_of_id_info id ii) in
    let taint =
      taint_config.D.is_source (G.Tk (snd id))
      |> Common.map fst |> T.taints_of_pms
    in
    Dataflow_core.VarMap.add var taint env
  in
  let in_env =
    (* For each argument, check if it's a source and, if so, add it to the input
     * environment. *)
    List.fold_left
      (fun env par ->
        match par with
        | G.Param { pname = Some id; pinfo; _ } -> add_to_env env id pinfo
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
                    add_to_env env id ii
                | _ -> env)
              env fields
        | _ -> env)
      Dataflow_core.VarMap.empty fdef.G.fparams
  in
  check_stmt ?name ~in_env lang fun_env taint_config
    (H.funcbody_to_stmt fdef.G.fbody)

(* TODO: Pass a hashtable to cache the CFG of each def, otherwise we are
 * recomputing the CFG for each taint rule. *)
module PMtbl = Hashtbl.Make (struct
  type t = PM.t

  let hash (pm : PM.t) = Hashtbl.hash (pm.rule_id, pm.file, pm.range_loc, pm.env)

  (* TODO: Shouldn't be the PM.equal that does the right thing? Instead of
   * deriving `equal` for `Metavariable.bindings` via ppx_deriving, perhaps
   * we need to have a custom definition that relies on AST_utils there. *)
  let equal = AST_utils.with_structural_equal PM.equal
end)

let check_rule rule match_hook (default_config, equivs) taint_spec xtarget =
  let matches = ref [] in
  let pm2finding = PMtbl.create 10 in

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
             |> Option.iter (fun pm ->
                    Common.push pm matches;
                    PMtbl.add pm2finding pm finding))
    in
    taint_config_of_rule default_config equivs file (ast, []) rule taint_spec
      handle_findings
  in

  let fun_env = Hashtbl.create 8 in

  let v =
    V.mk_visitor
      {
        V.default_visitor with
        V.kdef =
          (fun (k, _v) ((ent, def_kind) as def) ->
            match def_kind with
            | G.FuncDef fdef ->
                check_fundef lang fun_env taint_config (Some ent) fdef;
                (* go into nested functions *)
                k def
            | __else__ -> k def);
        V.kfunction_definition =
          (fun (k, _v) def ->
            check_fundef lang fun_env taint_config None def;
            (* go into nested functions *)
            k def);
      }
  in
  (* Check each function definition. *)
  v (G.Pr ast);
  (* Check the top-level statements.
   * In scripting languages it is not unusual to write code outside
   * function declarations and we want to check this too. We simply
   * treat the program itself as an anonymous function. *)
  let (), match_time =
    Common.with_time (fun () ->
        check_stmt lang fun_env taint_config (G.stmt1 ast))
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
                  let opt_finding = PMtbl.find_opt pm2finding m in
                  match_hook str m.env m.tokens opt_finding))
    |> Common.map (fun m ->
           { m with PM.rule_id = convert_rule_id rule.Rule.id })
  in
  let errors = Parse_target.errors_from_skipped_tokens skipped_tokens in
  ( {
      RP.matches;
      errors;
      skipped_targets = [];
      profiling = { RP.rule_id = fst rule.Rule.id; parse_time; match_time };
    },
    debug_taint )
