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
module D = Dataflow_tainting
module Var_env = Dataflow_var_env
module G = AST_generic
module H = AST_generic_helpers
module R = Rule
module PM = Core_match
module RP = Core_result
module T = Taint
module Lval_env = Taint_lval_env
module MV = Metavariable
module ME = Matching_explanation
module OutJ = Semgrep_output_v1_t
module Labels = Set.Make (String)
module Log = Log_tainting.Log
module Effect = Shape_and_sig.Effect
module Effects = Shape_and_sig.Effects
module Signature = Shape_and_sig.Signature

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Wrapper around the tainting dataflow-based analysis. *)

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

let get_source_requires src =
  let _pm, src_spec = T.pm_of_trace src.T.call_trace in
  src_spec.R.source_requires

(*****************************************************************************)
(* Testing whether some matches a taint spec *)
(*****************************************************************************)

let lazy_force x = Lazy.force x [@@profiling]

(*****************************************************************************)
(* Pattern match from finding *)
(*****************************************************************************)

(* If the 'requires' has the shape 'A and ...' then we assume that 'A' is the
 * preferred label for reporting the taint trace. *)
let preferred_label_of_sink ({ rule_sink; _ } : Effect.sink) =
  match rule_sink.sink_requires with
  | Some { precondition = PAnd (PLabel label :: _); _ } -> Some label
  | Some _
  | None ->
      None

let rec convert_taint_call_trace = function
  | Taint.PM (pm, _) ->
      let toks = Lazy.force pm.tokens |> List.filter Tok.is_origintok in
      Taint_trace.Toks toks
  | Taint.Call (expr, toks, ct) ->
      Taint_trace.Call
        {
          call_toks =
            AST_generic_helpers.ii_of_any (G.E expr)
            |> List.filter Tok.is_origintok;
          intermediate_vars = toks;
          call_trace = convert_taint_call_trace ct;
        }

(* For now CLI does not support multiple taint traces for a finding, and it
 * simply picks the _first_ trace from this list. So here we apply a number
 * of heuristics to make sure the first trace in this list is the most
 * relevant one. This is particularly important when using (experimental)
 * taint labels, because not all labels are equally relevant for the finding. *)
let sources_of_taints ?preferred_label taints =
  (* We only report actual sources reaching a sink. If users want Semgrep to
   * report function parameters reaching a sink without sanitization, then
   * they need to specify the parameters as taint sources. *)
  let taint_sources =
    taints
    |> List_.filter_map
         (fun { Effect.taint = { orig; rev_tokens }; sink_trace } ->
           match orig with
           | Src src -> Some (src, List.rev rev_tokens, sink_trace)
           (* even if there is any taint "variable", it's irrelevant for the
            * finding, since the precondition is satisfied. *)
           | Var _
           | Shape_var _
           | Control ->
               None)
  in
  let taint_sources =
    (* If there is a "preferred label", then sort sources to make sure this
       label is picked before others. See 'preferred_label_of_sink'. *)
    match preferred_label with
    | None -> taint_sources
    | Some label ->
        taint_sources
        |> List.stable_sort (fun (src1, _, _) (src2, _, _) ->
               match (src1.T.label = label, src2.T.label = label) with
               | true, false -> -1
               | false, true -> 1
               | false, false
               | true, true ->
                   0)
  in
  (* We prioritize taint sources without preconditions,
     selecting their traces first, and then consider sources
     with preconditions as a secondary choice. *)
  let with_req, without_req =
    taint_sources
    |> Either_.partition (fun (src, tokens, sink_trace) ->
           match get_source_requires src with
           | Some _ -> Left (src, tokens, sink_trace)
           | None -> Right (src, tokens, sink_trace))
  in
  if without_req <> [] then without_req
  else (
    Log.warn (fun m ->
        m
          "Taint source without precondition wasn't found. Displaying the \
           taint trace from the source with precondition.");
    with_req)

let trace_of_source source =
  let src, tokens, sink_trace = source in
  {
    Taint_trace.source_trace = convert_taint_call_trace src.T.call_trace;
    tokens;
    sink_trace = convert_taint_call_trace sink_trace;
  }

let pms_of_effect ~match_on (effect : Effect.t) =
  match effect with
  | ToLval _
  | ToReturn _
  | ToSinkInCall _ ->
      []
  | ToSink
      {
        taints_with_precondition = taints, requires;
        sink = { pm = sink_pm; _ } as sink;
        merged_env;
      } -> (
      if
        not
          (T.taints_satisfy_requires
             (List_.map (fun t -> t.Effect.taint) taints)
             requires)
      then []
      else
        let preferred_label = preferred_label_of_sink sink in
        let taint_sources = sources_of_taints ?preferred_label taints in
        match match_on with
        | `Sink ->
            (* The old behavior used to be that, for sinks with a `requires`, we would
               generate a finding per every single taint source going in. Later deduplication
               would deal with it.
               We will instead choose to consolidate all sources into a single finding. We can
               do some postprocessing to report only relevant sources later on, but for now we
               will lazily (again) defer that computation to later.
            *)
            let traces = List_.map trace_of_source taint_sources in
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
            [ { sink_pm with env = merged_env; taint_trace } ]
        | `Source ->
            taint_sources
            |> List_.map (fun source ->
                   let src, tokens, sink_trace = source in
                   let src_pm, _ = T.pm_of_trace src.T.call_trace in
                   let trace =
                     {
                       Taint_trace.source_trace =
                         convert_taint_call_trace src.T.call_trace;
                       tokens;
                       sink_trace = convert_taint_call_trace sink_trace;
                     }
                   in
                   {
                     src_pm with
                     env = merged_env;
                     taint_trace = Some (lazy [ trace ]);
                   }))

(*****************************************************************************)
(* Main entry points *)
(*****************************************************************************)

let check_fundef (taint_inst : Taint_rule_inst.t) name ctx ?glob_env fdef =
  let fdef = AST_to_IL.function_definition taint_inst.lang ~ctx fdef in
  let fcfg = CFG_build.cfg_of_fdef fdef in
  let in_env, env_effects =
    Taint_input_env.mk_fun_input_env taint_inst ?glob_env fdef.fparams
  in
  let effects, mapping =
    Dataflow_tainting.fixpoint taint_inst ~in_env ?name fcfg
  in
  let effects = Effects.union env_effects effects in
  (fcfg, effects, mapping)

let check_rule per_file_formula_cache (rule : R.taint_rule) match_hook
    (xconf : Match_env.xconfig) (xtarget : Xtarget.t) =
  Log.info (fun m ->
      m
        "Match_tainting_mode:\n\
         ====================\n\
         Running rule %s\n\
         ===================="
        (Rule_ID.to_string (fst rule.R.id)));
  let matches = ref [] in
  let match_on =
    (* TEMPORARY HACK to support both taint_match_on (DEPRECATED) and
       * taint_focus_on (preferred name by SR). *)
    match (xconf.config.taint_focus_on, xconf.config.taint_match_on) with
    | `Source, _
    | _, `Source ->
        `Source
    | `Sink, `Sink -> `Sink
  in
  let record_matches new_effects =
    new_effects
    |> Effects.iter (fun effect ->
           let effect_pms = pms_of_effect ~match_on effect in
           matches := List.rev_append effect_pms !matches)
  in
  let {
    path = { internal_path_to_content = file; _ };
    analyzer;
    lazy_ast_and_errors;
    _;
  } : Xtarget.t =
    xtarget
  in
  let lang =
    match analyzer with
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
  let taint_inst, _TODO_debug_taint, expls =
    Match_taint_spec.taint_config_of_rule ~per_file_formula_cache xconf lang
      file (ast, []) rule
  in

  (match !hook_setup_hook_function_taint_signature with
  | None -> ()
  | Some setup_hook_function_taint_signature ->
      setup_hook_function_taint_signature rule taint_inst xtarget);

  (* FIXME: This is no longer needed, now we can just check the type 'n'. *)
  let ctx = ref AST_to_IL.empty_ctx in
  Visit_function_defs.visit
    (fun opt_ent _fdef ->
      match opt_ent with
      | Some { name = EN (Id (n, _)); _ } ->
          ctx := AST_to_IL.add_entity_name !ctx n
      | __else__ -> ())
    ast;

  let glob_env, glob_effects = Taint_input_env.mk_file_env taint_inst ast in
  record_matches glob_effects;

  (* Check each function definition. *)
  Visit_function_defs.visit
    (fun opt_ent fdef ->
      match fst fdef.fkind with
      | LambdaKind
      | Arrow ->
          (* We do not need to analyze lambdas here, they will be analyzed
             together with their enclosing function. This would just duplicate
             work. *)
          ()
      | Function
      | Method
      | BlockCases ->
          let opt_name =
            let* ent = opt_ent in
            AST_to_IL.name_of_entity ent
          in
          Log.info (fun m ->
              m
                "Match_tainting_mode:\n\
                 --------------------\n\
                 Checking func def: %s\n\
                 --------------------"
                (Option.map IL.str_of_name opt_name ||| "???"));
          let _flow, fdef_effects, _mapping =
            check_fundef taint_inst opt_name !ctx ~glob_env fdef
          in
          record_matches fdef_effects)
    ast;

  (* Check execution of statements during object initialization. *)
  Visit_class_defs.visit
    (fun opt_ent cdef ->
      let opt_name =
        let* ent = opt_ent in
        AST_to_IL.name_of_entity ent
      in
      let fields =
        cdef.G.cbody |> Tok.unbracket
        |> List_.map (function G.F x -> x)
        |> G.stmt1
      in
      let stmts = AST_to_IL.stmt taint_inst.lang fields in
      let cfg, lambdas = CFG_build.cfg_of_stmts stmts in
      Log.info (fun m ->
          m
            "Match_tainting_mode:\n\
             --------------------\n\
             Checking object initialization: %s\n\
             --------------------"
            (Option.map IL.str_of_name opt_name ||| "???"));
      let init_effects, _mapping =
        Dataflow_tainting.fixpoint taint_inst ?name:opt_name
          IL.{ params = []; cfg; lambdas }
      in
      record_matches init_effects)
    ast;

  (* Check the top-level statements.
   * In scripting languages it is not unusual to write code outside
   * function declarations and we want to check this too. We simply
   * treat the program itself as an anonymous function. *)
  let (), match_time =
    Common.with_time (fun () ->
        let xs = AST_to_IL.stmt taint_inst.lang (G.stmt1 ast) in
        let cfg, lambdas = CFG_build.cfg_of_stmts xs in
        Log.info (fun m ->
            m
              "Match_tainting_mode:\n\
               --------------------\n\
               Checking top-level program\n\
               --------------------");
        let top_effects, _mapping =
          Dataflow_tainting.fixpoint taint_inst IL.{ params = []; cfg; lambdas }
        in
        record_matches top_effects)
  in
  let matches =
    !matches
    (* same post-processing as for search-mode in Match_rules.ml *)
    |> PM.uniq
    |> PM.no_submatches (* see "Taint-tracking via ranges" *) |> match_hook
  in
  let errors = Parse_target.errors_from_skipped_tokens skipped_tokens in
  let report =
    RP.mk_match_result matches errors
      {
        Core_profiling.rule_id = fst rule.R.id;
        rule_parse_time = parse_time;
        rule_match_time = match_time;
      }
  in
  let explanations =
    if xconf.matching_explanations then
      [
        {
          ME.op = OutJ.Taint;
          children = expls;
          matches = report.matches;
          pos = snd rule.id;
          extra = None;
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
  let per_file_formula_cache =
    Formula_cache.mk_specialized_formula_cache rules
  in

  rules
  |> List_.map (fun rule ->
         let%trace_debug sp = "Match_tainting_mode.check_rules.rule" in
         Tracing.add_data_to_span sp
           [
             ("rule_id", `String (fst rule.R.id |> Rule_ID.to_string));
             ("taint", `Bool true);
           ];

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
             Logs_.with_debug_trace ~__FUNCTION__
               ~pp_input:(fun _ ->
                 "target: "
                 ^ !!(xtarget.path.internal_path_to_content)
                 ^ "\nruleid: "
                 ^ (rule.id |> fst |> Rule_ID.to_string))
               (fun () ->
                 check_rule per_file_formula_cache rule match_hook xconf xtarget)))
