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
module PM = Pattern_match
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
    |> List_.filter_map (fun { Effect.taint = { orig; tokens }; sink_trace } ->
           match orig with
           | Src src -> Some (src, tokens, sink_trace)
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
    PM.source_trace = convert_taint_call_trace src.T.call_trace;
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
                       PM.source_trace =
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

let check_var_def lang options taint_config env id ii expr =
  let name = AST_to_IL.var_of_id_info id ii in
  let assign =
    G.Assign (G.N (G.Id (id, ii)) |> G.e, Tok.fake_tok (snd id) "=", expr)
    |> G.e |> G.exprstmt
  in
  let xs = AST_to_IL.stmt lang assign in
  let flow = CFG_build.cfg_of_stmts xs in
  let effects, end_mapping =
    (* There could be taint effects indeed, e.g. if 'expr' is `sink(taint)`. *)
    let java_props_cache = D.mk_empty_java_props_cache () in
    Dataflow_tainting.fixpoint ~in_env:env lang options taint_config
      java_props_cache flow
  in
  let out_env = end_mapping.(flow.exit).Dataflow_core.out_env in
  let lval : IL.lval = { base = Var name; rev_offset = [] } in
  let xtaint = Lval_env.find_lval_xtaint out_env lval in
  (xtaint, effects)

let add_to_env_aux lang options taint_config env id ii opt_expr =
  let var = AST_to_IL.var_of_id_info id ii in
  let var_type = Typing.resolved_type_of_id_info lang var.id_info in
  let id_taints =
    taint_config.D.is_source (G.Tk (snd id))
    |> List_.map (fun (x : _ Taint_spec_match.t) -> (x.spec_pm, x.spec))
    (* These sources come from the parameters to a function,
        which are not within the normal control flow of a code.
        We can safely say there's no incoming taints to these sources.
    *)
    |> T.taints_of_pms ~incoming:T.Taint_set.empty
  in
  let expr_taints, expr_effects =
    match opt_expr with
    | Some e ->
        let xtaint, effects =
          check_var_def lang options taint_config env id ii e
        in
        (Xtaint.to_taints xtaint, effects)
    | None -> (T.Taint_set.empty, Effects.empty)
  in
  let taints = id_taints |> T.Taint_set.union expr_taints in
  let taints =
    Dataflow_tainting.drop_taints_if_bool_or_number options taints var_type
  in
  let env = env |> Lval_env.add (IL_helpers.lval_of_var var) taints in
  (env, expr_effects)

let add_to_env lang options taint_config (env, effects) id id_info opt_expr =
  let env, new_effects =
    add_to_env_aux lang options taint_config env id id_info opt_expr
  in
  (env, Effects.union new_effects effects)

let mk_fun_input_env lang options taint_config ?(glob_env = Lval_env.empty)
    (fdef : IL.function_definition) =
  let add_to_env = add_to_env lang options taint_config in
  let in_env =
    (* For each argument, check if it's a source and, if so, add it to the input
     * environment. *)
    List.fold_left
      (fun env_and_effects par ->
        match par with
        | IL.Param { pname = name; pdefault } ->
            add_to_env env_and_effects name.ident name.id_info pdefault
        (* JS: {arg} : type *)
        | IL.PatternParam
            (G.OtherPat
              ( ("ExprToPattern", _),
                [
                  G.E
                    { e = G.Cast (_, _, { e = G.Record (_, fields, _); _ }); _ };
                ] ))
        (* JS: {arg} *)
        | IL.PatternParam
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
              env_and_effects fields
        | IL.PatternParam pat ->
            (* Here, we just get all the identifiers in the pattern, which may
               themselves be sources.
               This is so we can handle patterns such as:
               (x, (y, z, (a, b)))
               and taint all the inner identifiers
            *)
            let ids = Visit_pattern_ids.visit (G.P pat) in
            List.fold_left
              (fun env (id, pinfo) -> add_to_env env id pinfo None)
              env_and_effects ids
        | IL.FixmeParam -> env_and_effects)
      (glob_env, Effects.empty) fdef.fparams
  in
  in_env

let is_global (id_info : G.id_info) =
  let* kind, _sid = !(id_info.id_resolved) in
  Some (H.name_is_global kind)

let mk_file_env lang options taint_config ast =
  let add_to_env = add_to_env lang options taint_config in
  let env = ref (Lval_env.empty, Effects.empty) in
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
  let fdef = AST_to_IL.function_definition lang ~ctx fdef in
  let IL.{ fcfg = flow; _ } = CFG_build.cfg_of_fdef fdef in
  let in_env, env_effects =
    mk_fun_input_env lang options taint_config ?glob_env fdef
  in
  let effects, mapping =
    Dataflow_tainting.fixpoint ~in_env ?name lang options taint_config
      java_props_cache flow
  in
  let effects = Effects.union env_effects effects in
  (flow, effects, mapping)

let check_rule per_file_formula_cache (rule : R.taint_rule) match_hook
    (xconf : Match_env.xconfig) (xtarget : Xtarget.t) =
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

  let { path = { internal_path_to_content; _ }; xlang; lazy_ast_and_errors; _ }
      : Xtarget.t =
    xtarget
  in
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
    Match_taint_spec.taint_config_of_rule ~per_file_formula_cache xconf
      !!internal_path_to_content (ast, []) rule
  in

  (match !hook_setup_hook_function_taint_signature with
  | None -> ()
  | Some setup_hook_function_taint_signature ->
      setup_hook_function_taint_signature xconf rule taint_config xtarget);

  (* FIXME: This is no longer needed, now we can just check the type 'n'. *)
  let ctx = ref AST_to_IL.empty_ctx in
  Visit_function_defs.visit
    (fun opt_ent _fdef ->
      match opt_ent with
      | Some { name = EN (Id (n, _)); _ } ->
          ctx := AST_to_IL.add_entity_name !ctx n
      | __else__ -> ())
    ast;

  let java_props_cache = Dataflow_tainting.mk_empty_java_props_cache () in

  let glob_env, glob_effects = mk_file_env lang xconf.config taint_config ast in
  record_matches glob_effects;

  (* Check each function definition. *)
  Visit_function_defs.visit
    (fun opt_ent fdef ->
      let _flow, fdef_effects, _mapping =
        check_fundef lang xconf.config taint_config opt_ent !ctx ~glob_env
          java_props_cache fdef
      in
      record_matches fdef_effects)
    ast;

  (* Check execution of statements during object initialization. *)
  Visit_class_defs.visit
    (fun _ cdef ->
      let fields =
        cdef.G.cbody |> Tok.unbracket
        |> List_.map (function G.F x -> x)
        |> G.stmt1
      in
      let stmts = AST_to_IL.stmt lang fields in
      let flow = CFG_build.cfg_of_stmts stmts in
      let init_effects, _mapping =
        Dataflow_tainting.fixpoint lang xconf.config taint_config
          java_props_cache flow
      in
      record_matches init_effects)
    ast;

  (* Check the top-level statements.
   * In scripting languages it is not unusual to write code outside
   * function declarations and we want to check this too. We simply
   * treat the program itself as an anonymous function. *)
  let (), match_time =
    Common.with_time (fun () ->
        let xs = AST_to_IL.stmt lang (G.stmt1 ast) in
        let flow = CFG_build.cfg_of_stmts xs in
        let top_effects, _mapping =
          Dataflow_tainting.fixpoint lang xconf.config taint_config
            java_props_cache flow
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
