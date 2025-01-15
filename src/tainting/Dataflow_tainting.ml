(* Yoann Padioleau, Iago Abal
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
open IL
module Log = Log_tainting.Log
module G = AST_generic
module F = IL
module D = Dataflow_core
module Var_env = Dataflow_var_env
module VarMap = Var_env.VarMap
module PM = Core_match
module R = Rule
module LV = IL_helpers
module T = Taint
module Lval_env = Taint_lval_env
module Taints = T.Taint_set
module TM = Taint_spec_match
module TRI = Taint_rule_inst
module S = Shape_and_sig.Shape
module Shape = Taint_shape
module Effect = Shape_and_sig.Effect
module Effects = Shape_and_sig.Effects
module Signature = Shape_and_sig.Signature

(* TODO: Rename things to make clear that there are "sub-matches" and there are
 * "best matches". *)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Tainting dataflow analysis.
 *
 * - This is a rudimentary taint analysis in some ways, but rather complex in
 *   other ways... We don't do alias analysis, and inter-procedural support
 *   (for DeepSemgrep) still doesn't cover some common cases. On the other hand,
 *   almost _anything_ can be a source/sanitizer/sink, we have taint propagators,
 *   etc.
 * - It is a MAY analysis, it finds *potential* bugs (the tainted path could not
 *   be feasible in practice).
 * - Field sensitivity is limited to l-values of the form x.a.b.c, see module
 *   Taint_lval_env and check_tainted_lval for more details. Very coarse grained
 *   otherwise, e.g. `x[i] = tainted` will taint the whole array,
 *
 * old: This was originally in src/analyze, but it now depends on
 *      Pattern_match, so it was moved to src/engine.
 *)

module DataflowX = Dataflow_core.Make (struct
  type node = F.node
  type edge = F.edge
  type flow = (node, edge) CFG.t

  let short_string_of_node n = Display_IL.short_string_of_node_kind n.F.n
end)

module SMap = Map.Make (String)

let sigs_tag = Log_tainting.sigs_tag
let transfer_tag = Log_tainting.transfer_tag

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type mapping = Lval_env.t D.mapping
type java_props_cache = (string * G.SId.t, IL.name) Hashtbl.t

let mk_empty_java_props_cache () = Hashtbl.create 30

type func = {
  fname : IL.name option;
  best_matches : TM.Best_matches.t;
      (** Best matches for the taint sources/etc, see 'Taint_spec_match'. *)
  used_lambdas : IL.NameSet.t;
      (** Set of lambda names that are *used* within the function. If a lambda
        is used, we analyze it at use-site, otherwise we analyze it at def site. *)
}
(** Data about the top-level function definition under analysis, this does not
 * vary when analyzing lambdas. *)

(* REFACTOR: Rename 'Taint_lval_env' as 'Taint_var_env' and create a new module
    for this 'env' type called 'Taint_env' or 'Taint_state' or sth, then we could
    e.g. move all lambda stuff to 'Taint_lambda'. *)
(* THINK: Separate read-only enviroment into a new a "cfg" type? *)
type env = {
  taint_inst : Taint_rule_inst.t;
  func : func;
  in_lambda : IL.name option;
  lambdas : IL.lambdas_cfgs;
  needed_vars : IL.NameSet.t;
      (** Vars that we need to track in the current function/lambda under analysis,
    other vars can be filtered out, see 'fixpoint_lambda' as well as
    'Taint_lambda.find_vars_to_track_across_lambdas'. *)
  lval_env : Lval_env.t;
  effects_acc : Effects.t ref;
}

(*****************************************************************************)
(* Hooks *)
(*****************************************************************************)

let hook_function_taint_signature = ref None
let hook_find_attribute_in_class = ref None
let hook_check_tainted_at_exit_sinks = ref None
let hook_infer_sig_for_lambda = ref None

(*****************************************************************************)
(* Options *)
(*****************************************************************************)

let propagate_through_functions env =
  (not env.taint_inst.options.taint_assume_safe_functions)
  && not env.taint_inst.options.taint_only_propagate_through_assignments

let propagate_through_indexes env =
  (not env.taint_inst.options.taint_assume_safe_indexes)
  && not env.taint_inst.options.taint_only_propagate_through_assignments

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let log_timeout_warning (taint_inst : Taint_rule_inst.t) opt_name timeout =
  match timeout with
  | `Ok -> ()
  | `Timeout ->
      (* nosemgrep: no-logs-in-library *)
      Logs.warn (fun m ->
          m
            "Fixpoint timeout while performing taint analysis [rule: %s file: \
             %s func: %s]"
            (Rule_ID.to_string taint_inst.rule_id)
            !!(taint_inst.file)
            (Option.map IL.str_of_name opt_name ||| "???"))

let map_check_expr env check_expr xs =
  let rev_taints_and_shapes, lval_env =
    xs
    |> List.fold_left
         (fun (rev_taints_and_shapes, lval_env) x ->
           let taints, shape, lval_env = check_expr { env with lval_env } x in
           ((taints, shape) :: rev_taints_and_shapes, lval_env))
         ([], env.lval_env)
  in
  (List.rev rev_taints_and_shapes, lval_env)

let union_map_taints_and_vars env check xs =
  let taints, lval_env =
    xs
    |> List.fold_left
         (fun (taints_acc, lval_env) x ->
           let taints, shape, lval_env = check { env with lval_env } x in
           let taints_acc =
             taints_acc |> Taints.union taints
             |> Taints.union (Shape.gather_all_taints_in_shape shape)
           in
           (taints_acc, lval_env))
         (Taints.empty, env.lval_env)
  in
  let taints =
    if env.taint_inst.options.taint_only_propagate_through_assignments then
      Taints.empty
    else taints
  in
  (taints, lval_env)

let gather_all_taints_in_args_taints args_taints =
  args_taints
  |> List.fold_left
       (fun acc arg ->
         match arg with
         | Named (_, (_, shape))
         | Unnamed (_, shape) ->
             Shape.gather_all_taints_in_shape shape |> Taints.union acc)
       Taints.empty

let any_is_best_sanitizer env any =
  env.taint_inst.preds.is_sanitizer any
  |> List.filter (fun (m : R.taint_sanitizer TM.t) ->
         (not m.spec.sanitizer_exact)
         || TM.is_best_match env.func.best_matches m)

(* TODO: We could return source matches already split by `by-side-effect` here ? *)
let any_is_best_source ?(is_lval = false) env any =
  env.taint_inst.preds.is_source any
  |> List.filter (fun (m : R.taint_source TM.t) ->
         (* Remove sources that should match exactly but do not here. *)
         match m.spec.source_by_side_effect with
         | Only -> is_lval && TM.is_exact m
         (* 'Yes' should probably require an exact match like 'Only' but for
          *  backwards compatibility we keep it this way. *)
         | Yes
         | No ->
             (not m.spec.source_exact)
             || TM.is_best_match env.func.best_matches m)

let any_is_best_sink env any =
  env.taint_inst.preds.is_sink any
  |> List.filter (fun (tm : R.taint_sink TM.t) ->
         (* at-exit sinks are handled in 'check_tainted_at_exit_sinks' *)
         (not tm.spec.sink_at_exit) && TM.is_best_match env.func.best_matches tm)

let orig_is_source (taint_inst : Taint_rule_inst.t) orig =
  taint_inst.preds.is_source (any_of_orig orig)

let orig_is_best_source env orig : R.taint_source TM.t list =
  any_is_best_source env (any_of_orig orig)
[@@profiling]

let orig_is_sanitizer (taint_inst : Taint_rule_inst.t) orig =
  taint_inst.preds.is_sanitizer (any_of_orig orig)

let orig_is_best_sanitizer env orig =
  any_is_best_sanitizer env (any_of_orig orig)
[@@profiling]

let orig_is_sink (taint_inst : Taint_rule_inst.t) orig =
  taint_inst.preds.is_sink (any_of_orig orig)

let orig_is_best_sink env orig = any_is_best_sink env (any_of_orig orig)
[@@profiling]

let any_of_lval lval =
  match lval with
  | { rev_offset = { oorig; _ } :: _; _ } -> any_of_orig oorig
  | { base = Var var; rev_offset = [] } ->
      let _, tok = var.ident in
      G.Tk tok
  | { base = VarSpecial (_, tok); rev_offset = [] } -> G.Tk tok
  | { base = Mem e; rev_offset = [] } -> any_of_orig e.eorig

let lval_is_source env lval =
  any_is_best_source ~is_lval:true env (any_of_lval lval)

let lval_is_best_sanitizer env lval =
  any_is_best_sanitizer env (any_of_lval lval)

let lval_is_sink env lval =
  (* TODO: This should be = any_is_best_sink env (any_of_lval lval)
   *    but see tests/rules/TODO_taint_messy_sink. *)
  env.taint_inst.preds.is_sink (any_of_lval lval)
  |> List.filter (fun (tm : R.taint_sink TM.t) ->
         (* at-exit sinks are handled in 'check_tainted_at_exit_sinks' *)
         not tm.spec.sink_at_exit)
[@@profiling]

let taints_of_matches env ~incoming sources =
  let control_sources, data_sources =
    sources
    |> List.partition (fun (m : R.taint_source TM.t) -> m.spec.source_control)
  in
  (* THINK: It could make sense to merge `incoming` with `control_incoming`, so
   * a control source could influence a data source and vice-versa. *)
  let data_taints =
    data_sources
    |> List_.map (fun x -> (x.TM.spec_pm, x.spec))
    |> T.taints_of_pms ~incoming
  in
  let control_incoming = Lval_env.get_control_taints env.lval_env in
  let control_taints =
    control_sources
    |> List_.map (fun x -> (x.TM.spec_pm, x.spec))
    |> T.taints_of_pms ~incoming:control_incoming
  in
  let lval_env = Lval_env.add_control_taints env.lval_env control_taints in
  (data_taints, lval_env)

let record_effects env new_effects =
  if not (List_.null new_effects) then
    let new_effects =
      env.taint_inst.handle_effects env.func.fname new_effects
    in
    env.effects_acc := Effects.add_list new_effects !(env.effects_acc)

let unify_mvars_sets env mvars1 mvars2 =
  let xs =
    List.fold_left
      (fun xs_opt (mvar, mval) ->
        let* xs = xs_opt in
        match List.assoc_opt mvar mvars2 with
        | None -> Some ((mvar, mval) :: xs)
        | Some mval' ->
            if
              Matching_generic.equal_ast_bound_code env.taint_inst.options mval
                mval'
            then Some ((mvar, mval) :: xs)
            else None)
      (Some []) mvars1
  in
  let ys =
    List.filter (fun (mvar, _) -> not @@ List.mem_assoc mvar mvars1) mvars2
  in
  Option.map (fun xs -> xs @ ys) xs

let sink_biased_union_mvars source_mvars sink_mvars =
  let source_mvars' =
    List.filter
      (fun (mvar, _) -> not @@ List.mem_assoc mvar sink_mvars)
      source_mvars
  in
  Some (source_mvars' @ sink_mvars)

(* Takes the bindings of multiple taint sources and filters the bindings ($MVAR, MVAL)
 * such that either $MVAR is bound by a single source, or all MVALs bounds to $MVAR
 * can be unified. *)
let merge_source_mvars env bindings =
  let flat_bindings = List_.flatten bindings in
  let bindings_tbl =
    flat_bindings
    |> List_.map (fun (mvar, _) -> (mvar, None))
    |> List.to_seq |> Hashtbl.of_seq
  in
  flat_bindings
  |> List.iter (fun (mvar, mval) ->
         match Hashtbl.find_opt bindings_tbl mvar with
         | None ->
             (* This should only happen if we've previously found that
                there is a conflict between bound values at `mvar` in
                the sources.
             *)
             ()
         | Some None ->
             (* This is our first time seeing this value, let's just
                add it in.
             *)
             Hashtbl.replace bindings_tbl mvar (Some mval)
         | Some (Some mval') ->
             if
               not
                 (Matching_generic.equal_ast_bound_code env.taint_inst.options
                    mval mval')
             then Hashtbl.remove bindings_tbl mvar);
  (* After this, the only surviving bindings should be those where
     there was no conflict between bindings in different sources.
  *)
  bindings_tbl |> Hashtbl.to_seq |> List.of_seq
  |> List_.filter_map (fun (mvar, mval_opt) ->
         match mval_opt with
         | None ->
             (* This actually shouldn't really be possible, every
                binding should either not exist, or contain a value
                if there's no conflict. But whatever. *)
             None
         | Some mval -> Some (mvar, mval))

(* Merge source's and sink's bound metavariables. *)
let merge_source_sink_mvars env source_mvars sink_mvars =
  if env.taint_inst.options.taint_unify_mvars then
    (* This used to be the default, but it turned out to be confusing even for
     * r2c's security team! Typically you think of `pattern-sources` and
     * `pattern-sinks` as independent. We keep this option mainly for
     * backwards compatibility, it may be removed later on if no real use
     * is found. *)
    unify_mvars_sets env source_mvars sink_mvars
  else
    (* The union of both sets, but taking the sink mvars in case of collision. *)
    sink_biased_union_mvars source_mvars sink_mvars

let partition_sources_by_side_effect sources_matches =
  sources_matches
  |> Either_.partition_either3 (fun (m : R.taint_source TM.t) ->
         match m.spec.source_by_side_effect with
         | R.Only -> Left3 m
         (* A 'Yes' should be a 'Yes' regardless of whether the match is exact...
          * Whether the match is exact or not is/should be taken into consideration
          * later on. Same as for 'Only'. But for backwards-compatibility we keep
          * it this way for now. *)
         | R.Yes when TM.is_exact m -> Middle3 m
         | R.Yes
         | R.No ->
             Right3 m)
  |> fun (only, yes, no) -> (`Only only, `Yes yes, `No no)

(* We need to filter out `Control` variables since those do not propagate trough return
 * (there is just no point in doing so). *)
let get_control_taints_to_return env =
  Lval_env.get_control_taints env.lval_env
  |> Taints.filter (fun ({ orig; _ } : T.taint) ->
         match orig with
         | T.Src _ -> true
         | Var _
         | Shape_var _
         | Control ->
             false)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

let type_of_lval env lval =
  match lval with
  | { base = Var x; rev_offset = [] } ->
      Typing.resolved_type_of_id_info env.taint_inst.lang x.id_info
  | { base = _; rev_offset = { o = Dot fld; _ } :: _ } ->
      Typing.resolved_type_of_id_info env.taint_inst.lang fld.id_info
  | __else__ -> Type.NoType

let type_of_expr env e =
  match e.eorig with
  | SameAs eorig -> Typing.type_of_expr env.taint_inst.lang eorig |> fst
  | __else__ -> Type.NoType

(* We only check this at a few key places to avoid calling `type_of_expr` too
 * many times which could be bad for perf (but haven't properly benchmarked):
 * - assignments
 * - return's
 * - function calls and their actual arguments
 * TODO: Ideally we add an `e_type` field and have a type-inference pass to
 *  fill it in, so that every expression has its known type available without
 *  extra cost.
 *)
let drop_taints_if_bool_or_number (options : Rule_options.t) taints ty =
  match ty with
  | Type.(Builtin Bool) when options.taint_assume_safe_booleans -> Taints.empty
  | Type.(Builtin (Int | Float | Number)) when options.taint_assume_safe_numbers
    ->
      Taints.empty
  | __else__ -> taints

(* Calls to 'type_of_expr' seem not to be cheap and even though we tried to limit the
 * number of these calls being made, doing them unconditionally caused a slowdown of
 * ~25% in a ~dozen repos in our stress-test-monorepo. We should just not call
 * 'type_of_expr' unless at least one of the taint_assume_safe_{booleans,numbers} has
 * been set, so rules that do not use these options remain unaffected. Long term we
 * should make type_of_expr less costly.
 *)
let check_type_and_drop_taints_if_bool_or_number env taints type_of_x x =
  if
    (env.taint_inst.options.taint_assume_safe_booleans
   || env.taint_inst.options.taint_assume_safe_numbers)
    && not (Taints.is_empty taints)
  then
    match type_of_x env x with
    | Type.Function (_, return_ty) ->
        drop_taints_if_bool_or_number env.taint_inst.options taints return_ty
    | ty -> drop_taints_if_bool_or_number env.taint_inst.options taints ty
  else taints

(*****************************************************************************)
(* Labels *)
(*****************************************************************************)

(* This function is used to convert some taint thing we're holding
   to one which has been propagated to a new label.
   See [handle_taint_propagators] for more.
*)
let propagate_taint_to_label replace_labels label (taint : T.taint) =
  let new_orig =
    match (taint.orig, replace_labels) with
    (* if there are no replaced labels specified, we will replace
       indiscriminately
    *)
    | Src src, None -> T.Src { src with label }
    | Src src, Some replace_labels when List.mem src.T.label replace_labels ->
        T.Src { src with label }
    | ((Src _ | Var _ | Shape_var _ | Control) as orig), _ -> orig
  in
  { taint with orig = new_orig }

(*****************************************************************************)
(* Effects and signatures *)
(*****************************************************************************)

(* Potentially produces an effect from incoming taints + call traces to a sink.
   Note that, while this sink has a `requires` and incoming labels,
   we decline to solve this now!
   We will figure out how many actual Semgrep findings are generated
   when this information is used, later.
*)
let effects_of_tainted_sink env taints_with_traces (sink : Effect.sink) :
    Effect.t list =
  match taints_with_traces with
  | [] -> []
  | _ :: _ -> (
      (* We cannot check whether we satisfy the `requires` here.
         This is because this sink may be inside of a function, meaning that
         argument taint can reach it, which can only be instantiated at the
         point where we call the function.
         So we record the `requires` within the taint finding, and evaluate
         the formula later, when we extract the PMs
      *)
      let { Effect.pm = sink_pm; rule_sink = ts } = sink in
      let taints_and_bindings =
        taints_with_traces
        |> List_.map (fun ({ Effect.taint; _ } as item) ->
               let bindings =
                 match taint.T.orig with
                 | T.Src source ->
                     let src_pm, _ = T.pm_of_trace source.call_trace in
                     src_pm.env
                 | Var _
                 | Shape_var _
                 | Control ->
                     []
               in
               (item, bindings))
      in
      (* If `unify_mvars` is set, then we will just do the previous behavior,
         and emit a finding for every single source coming into the sink.
         This will mean we don't regress on `taint_unify_mvars: true` rules.

         This is problematic because there may be many sources, all of which do not
         unify with each other, but which unify with the sink.
         If we did as below and unified them all with each other, we would sometimes
         produce no findings when we should.
      *)
      (* The same will happen if our sink does not have an explicit `requires`.

         This is because our behavior in the second case will remove metavariables
         from the finding, if they conflict in the sources.

         This can lead to a loss of metavariable interpolation in the finding message,
         even for "vanilla" taint mode rules that don't use labels, for instance if
         we had two instances of the source

         foo($X)

         reaching a sink, where in both instances, `$X` is not the same. The current
         behavior is that one of the `$X` bindings is chosen arbitrarily. We will
         try to keep this behavior here.
      *)
      if
        env.taint_inst.options.taint_unify_mvars
        || Option.is_none sink.rule_sink.sink_requires
      then
        taints_and_bindings
        |> List_.filter_map (fun (t, bindings) ->
               let* merged_env =
                 merge_source_sink_mvars env sink_pm.PM.env bindings
               in
               Some
                 (Effect.ToSink
                    {
                      taints_with_precondition = ([ t ], R.get_sink_requires ts);
                      sink;
                      merged_env;
                    }))
      else
        match
          taints_and_bindings |> List_.map snd |> merge_source_mvars env
          |> merge_source_sink_mvars env sink_pm.PM.env
        with
        | None -> []
        | Some merged_env ->
            [
              Effect.ToSink
                {
                  taints_with_precondition =
                    (List_.map fst taints_and_bindings, R.get_sink_requires ts);
                  sink;
                  merged_env;
                };
            ])

(* Produces a finding for every unifiable source-sink pair. *)
let effects_of_tainted_sinks env taints sinks : Effect.t list =
  let taints =
    let control_taints = Lval_env.get_control_taints env.lval_env in
    taints |> Taints.union control_taints
  in
  if Taints.is_empty taints then []
  else
    sinks
    |> List.concat_map (fun sink ->
           (* This is where all taint effects start. If it's interproc,
              the call trace will be later augmented into the Call variant,
              but it starts out here as just a PM variant.
           *)
           let taints_with_traces =
             taints |> Taints.elements
             |> List_.map (fun t ->
                    { Effect.taint = t; sink_trace = T.PM (sink.Effect.pm, ()) })
           in
           effects_of_tainted_sink env taints_with_traces sink)

let effects_of_tainted_return env taints shape return_tok : Effect.t list =
  let control_taints = get_control_taints_to_return env in
  if
    Shape.taints_and_shape_are_relevant taints shape
    || not (Taints.is_empty control_taints)
  then
    [
      Effect.ToReturn
        { data_taints = taints; data_shape = shape; control_taints; return_tok };
    ]
  else []

(* If a 'fun_exp' has no known taint signature, then it should have a polymorphic
 * shape and we record its effects with an "effect variable" (that's kind of what
 * 'ToSinkInCall' does). *)
let effects_of_call_func_arg fun_exp fun_shape args_taints =
  match fun_shape with
  | S.Arg fun_arg ->
      [ Effect.ToSinkInCall { callee = fun_exp; arg = fun_arg; args_taints } ]
  | __else__ ->
      Log.debug (fun m ->
          m "Function (?) %s has shape %s"
            (Display_IL.string_of_exp fun_exp)
            (S.show_shape fun_shape));
      []

let lookup_signature env fun_exp =
  match (!hook_function_taint_signature, fun_exp) with
  | Some hook, { e = Fetch _f; eorig = SameAs eorig } ->
      hook env.taint_inst eorig
  | __else__ -> None

let lookup_fun_signature env fun_exp =
  match lookup_signature env fun_exp with
  | Some (taint_sig, `Fun) -> Some taint_sig
  | Some (_, `Var)
  | None ->
      None

(*****************************************************************************)
(* Lambdas *)
(*****************************************************************************)

let lambdas_used_in_node lambdas node =
  LV.rlvals_of_node node.IL.n |> List_.filter_map (LV.lval_is_lambda lambdas)

let lambdas_used_in_cfg (fun_cfg : fun_cfg) =
  fun_cfg |> LV.reachable_nodes
  |> Seq.fold_left
       (fun used_lambdas_acc node ->
         let lambdas_in_node =
           node
           |> lambdas_used_in_node fun_cfg.lambdas
           |> List.to_seq
           |> Seq.map (fun (lname, _) -> lname)
           |> IL.NameSet.of_seq
         in
         IL.NameSet.union lambdas_in_node used_lambdas_acc)
       IL.NameSet.empty

let lambdas_to_analyze_in_node env lambdas node =
  let unused_lambda_def =
    let* instr =
      match node.F.n with
      | NInstr i -> Some i
      | __else__ -> None
    in
    let* lval = LV.lval_of_instr_opt instr in
    let* ((lname, _) as lambda) = LV.lval_is_lambda lambdas lval in
    if IL.NameSet.mem lname env.func.used_lambdas then None else Some lambda
  in
  Option.to_list unused_lambda_def @ lambdas_used_in_node lambdas node

(*****************************************************************************)
(* Miscellaneous *)
(*****************************************************************************)

let check_orig_if_sink env ?filter_sinks orig taints shape =
  (* NOTE(gather-all-taints):
   * A sink is something opaque to us, e.g. consider sink(["ok", "tainted"]),
   * `sink` could potentially access "tainted". So we must take into account
   * all taints reachable through its shape.
   *)
  let taints =
    taints |> Taints.union (Shape.gather_all_taints_in_shape shape)
  in
  let sinks = orig_is_best_sink env orig in
  let sinks =
    match filter_sinks with
    | None -> sinks
    | Some sink_pred -> sinks |> List.filter sink_pred
  in
  let sinks = sinks |> List_.map TM.sink_of_match in
  let effects = effects_of_tainted_sinks env taints sinks in
  record_effects env effects

let fix_poly_taint_with_field lval xtaint =
  match xtaint with
  | `Sanitized
  | `Clean
  | `None ->
      xtaint
  | `Tainted taints -> (
      match lval.rev_offset with
      | o :: _ ->
          let o = T.offset_of_IL o in
          let taints = Shape.fix_poly_taint_with_offset [ o ] taints in
          `Tainted taints
      | [] -> xtaint)

(*****************************************************************************)
(* Tainted *)
(*****************************************************************************)

let sanitize_lval_by_side_effect lval_env sanitizer_pms lval =
  let lval_is_now_safe =
    (* If the l-value is an exact match (overlap > 0.99) for a sanitizer
     * annotation, then we infer that the l-value itself has been updated
     * (presumably by side-effect) and is no longer tainted. We will update
     * the environment (i.e., `lval_env') accordingly. *)
    List.exists
      (fun (m : R.taint_sanitizer TM.t) ->
        m.spec.sanitizer_by_side_effect && TM.is_exact m)
      sanitizer_pms
  in
  if lval_is_now_safe then Lval_env.clean lval_env lval else lval_env

(* Check if an expression is sanitized, if so returns `Some' and otherise `None'.
   If the expression is of the form `x.a.b.c` then we try to sanitize it by
   side-effect, in which case this function will return a new lval_env. *)
let exp_is_sanitized env exp =
  match orig_is_best_sanitizer env exp.eorig with
  (* See NOTE [is_sanitizer] *)
  | [] -> None
  | sanitizer_pms -> (
      match exp.e with
      | Fetch lval ->
          Some (sanitize_lval_by_side_effect env.lval_env sanitizer_pms lval)
      | __else__ -> Some env.lval_env)

(* Checks if `thing' is a propagator `from' and if so propagates `taints' through it.
   Checks if `thing` is a propagator `'to' and if so fetches any taints that had been
   previously propagated. Returns *only* the newly propagated taint. *)
let handle_taint_propagators env thing taints shape =
  (* We propagate taints via an auxiliary variable (the propagator id). This is
   * simple but it has limitations. It works well to propagate "forward" and,
   * within an instruction node, to propagate in the order in which we visit the
   * subexpressions. E.g. in `x.f(y,z)` we can easily propagate taint from `y` or
   * `z` to `x`, or from `y` to `z`.
   *
   * So, how to propagate taint from `x` to `y` or `z`, or from `z` to `y` ?
   * In Pro, we do it by recording them as "pending" (see
   * 'Taint_lval_env.pending_propagation_dests'). The problem with that kind of
   * "delayed" propagation is that it **only** works by side-effect, but not at
   * the very location of the destination. So we can propagate taint by side-effect
   * from `z` to `y` in `x.f(y,z)`, but the `y` occurrence that is the actual
   * destination (i.e. the `$TO`) will not have the taints coming from `z`, only
   * the subsequent occurrences of `y` will.
   * TODO: To support that, we may need to introduce taint variables that we can
   *       later substitute, like we do for labels.
   * *)
  let taints =
    taints |> Taints.union (Shape.gather_all_taints_in_shape shape)
  in
  let lval_env = env.lval_env in
  let propagators =
    let any =
      match thing with
      | `Lval lval -> any_of_lval lval
      | `Exp exp -> any_of_orig exp.eorig
      | `Ins ins -> any_of_orig ins.iorig
    in
    env.taint_inst.preds.is_propagator any
  in
  let propagate_froms, propagate_tos =
    List.partition (fun p -> p.TM.spec.TRI.kind =*= `From) propagators
  in
  let lval_env =
    (* `thing` is the source (the "from") of propagation, we add its taints to
     * the environment. *)
    List.fold_left
      (fun lval_env prop ->
        (* Only propagate if the current set of taint labels can satisfy the
           propagator's requires precondition.
        *)
        (* TODO(brandon): Interprocedural propagator labels
           This is trickier than I thought. You have to augment the Arg taints
           with preconditions as well, and allow conjunction, because when you
           replace an Arg taint with a precondition, all the produced taints
           inherit the precondition. There's not an easy way to express this
           in the type right now.

           More concretely, the existence of labeled propagators means that
           preconditions can be attached to arbitrary taint. This is because
           if we have a taint that is being propagated with a `requires`, then
           that taint now has a precondition on that `requires` being true. This
           taint might also be an `Arg` taint, meaning that `Arg` taints can
           have preconditions.

           This is more than just a simple type-level change because when `Arg`s
           have preconditions, what happens for substitution? Say I want to
           replace an `Arg x` taint with [t], that is, a single taint. Well,
           that taint `t` might itself have a precondition. That means that we
           now have a taint which is `t`, substituted for `Arg x`, but also
           inheriting `Arg x`'s precondition. Our type for preconditions doesn't
           allow arbitrary conjunction of preconditions like that, so this is
           more pervasive of a change.

           I'll come back to this later.
        *)
        match
          T.solve_precondition ~ignore_poly_taint:false ~taints
            (R.get_propagator_precondition prop.TM.spec.TRI.prop)
        with
        | Some true ->
            (* If we have an output label, change the incoming taints to be
               of the new label.
               Otherwise, keep them the same.
            *)
            let new_taints =
              match prop.TM.spec.prop.propagator_label with
              | None -> taints
              | Some label ->
                  Taints.map
                    (propagate_taint_to_label
                       prop.spec.prop.propagator_replace_labels label)
                    taints
            in
            Lval_env.propagate_to prop.spec.var new_taints lval_env
        | Some false
        | None ->
            lval_env)
      lval_env propagate_froms
  in
  let taints_propagated, lval_env =
    (* `thing` is the destination (the "to") of propagation. we collect all the
     * incoming taints by looking for the propagator ids in the environment. *)
    List.fold_left
      (fun (taints_in_acc, lval_env) prop ->
        let opt_propagated, lval_env =
          Lval_env.propagate_from prop.TM.spec.TRI.var lval_env
        in
        let taints_from_prop =
          match opt_propagated with
          | None -> Taints.empty
          | Some taints -> taints
        in
        let lval_env =
          if prop.spec.TRI.prop.propagator_by_side_effect then
            match thing with
            (* If `thing` is an l-value of the form `x.a.b.c`, then taint can be
             *  propagated by side-effect. A pattern-propagator may use this to
             * e.g. propagate taint from `x` to `y` in `f(x,y)`, so that
             * subsequent uses of `y` are tainted if `x` was previously tainted. *)
            | `Lval lval ->
                if Option.is_some opt_propagated then
                  lval_env |> Lval_env.add_lval lval taints_from_prop
                else
                  (* If we did not find any taint to be propagated, it could
                   * be because we have not encountered the 'from' yet, so we
                   * add the 'lval' to a "pending" queue. *)
                  lval_env |> Lval_env.pending_propagation prop.TM.spec.var lval
            | `Exp _
            | `Ins _ ->
                lval_env
          else lval_env
        in
        (Taints.union taints_in_acc taints_from_prop, lval_env))
      (Taints.empty, lval_env) propagate_tos
  in
  (taints_propagated, lval_env)

let find_lval_taint_sources env incoming_taints lval =
  let taints_of_pms env = taints_of_matches env ~incoming:incoming_taints in
  let source_pms = lval_is_source env lval in
  (* Partition sources according to the value of `by-side-effect:`,
   * either `only`, `yes`, or `no`. *)
  let ( `Only by_side_effect_only_pms,
        `Yes by_side_effect_yes_pms,
        `No by_side_effect_no_pms ) =
    partition_sources_by_side_effect source_pms
  in
  let by_side_effect_only_taints, lval_env =
    by_side_effect_only_pms
    (* We require an exact match for `by-side-effect` to take effect. *)
    |> List.filter TM.is_exact
    |> taints_of_pms env
  in
  let by_side_effect_yes_taints, lval_env =
    by_side_effect_yes_pms
    (* We require an exact match for `by-side-effect` to take effect. *)
    |> List.filter TM.is_exact
    |> taints_of_pms { env with lval_env }
  in
  let by_side_effect_no_taints, lval_env =
    by_side_effect_no_pms |> taints_of_pms { env with lval_env }
  in
  let taints_to_add_to_env =
    by_side_effect_only_taints |> Taints.union by_side_effect_yes_taints
  in
  let lval_env = lval_env |> Lval_env.add_lval lval taints_to_add_to_env in
  let taints_to_return =
    Taints.union by_side_effect_no_taints by_side_effect_yes_taints
  in
  (taints_to_return, lval_env)

let rec check_tainted_lval env (lval : IL.lval) :
    Taints.t * S.shape * [ `Sub of Taints.t * S.shape ] * Lval_env.t =
  let new_taints, lval_in_env, lval_shape, sub, lval_env =
    check_tainted_lval_aux env lval
  in
  let taints_from_env = Xtaint.to_taints lval_in_env in
  let taints = Taints.union new_taints taints_from_env in
  let taints =
    check_type_and_drop_taints_if_bool_or_number env taints type_of_lval lval
  in
  let sinks =
    lval_is_sink env lval
    |> List.filter (TM.is_best_match env.func.best_matches)
    |> List_.map TM.sink_of_match
  in
  let effects = effects_of_tainted_sinks { env with lval_env } taints sinks in
  record_effects { env with lval_env } effects;
  (taints, lval_shape, sub, lval_env)

(* Java: Whenever we find a getter/setter without definition we end up here,
 * this happens if the getter/setters are being autogenerated at build time,
 * as when you use Lombok. This function will "resolve" the getter/setter to
 * the corresponding property, and propagate taint to/from that property.
 * So that `o.getX()` returns whatever taints `o.x` has, and so `o.setX(E)`
 * propagates any taints in `E` to `o.x`. *)
and propagate_taint_via_java_getters_and_setters_without_definition env e args
    all_args_taints =
  match e with
  | {
   e =
     Fetch
       ({
          base = Var obj;
          rev_offset =
            [ { o = Dot { IL.ident = method_str, method_tok; sid; _ }; _ } ];
        } as lval);
   _;
  }
  (* We check for the "get"/"set" prefix below. *)
    when env.taint_inst.lang =*= Lang.Java && String.length method_str > 3 -> (
      let mk_prop_lval () =
        (* e.g. getFooBar/setFooBar -> fooBar *)
        let prop_str =
          String.uncapitalize_ascii (Str.string_after method_str 3)
        in
        let prop_name =
          match
            Hashtbl.find_opt env.taint_inst.java_props_cache (prop_str, sid)
          with
          | Some prop_name -> prop_name
          | None -> (
              let mk_default_prop_name () =
                let prop_name =
                  {
                    ident = (prop_str, method_tok);
                    sid = G.SId.unsafe_default;
                    id_info = G.empty_id_info ();
                  }
                in
                Hashtbl.add env.taint_inst.java_props_cache (prop_str, sid)
                  prop_name;
                prop_name
              in
              match (!(obj.id_info.id_type), !hook_find_attribute_in_class) with
              | Some { t = TyN class_name; _ }, Some hook -> (
                  match hook class_name prop_str with
                  | None -> mk_default_prop_name ()
                  | Some prop_name ->
                      let prop_name = AST_to_IL.var_of_name prop_name in
                      Hashtbl.add env.taint_inst.java_props_cache
                        (prop_str, sid) prop_name;
                      prop_name)
              | __else__ -> mk_default_prop_name ())
        in
        { lval with rev_offset = [ { o = Dot prop_name; oorig = NoOrig } ] }
      in
      match args with
      | [] when String.(starts_with ~prefix:"get" method_str) ->
          let taints, shape, _sub, lval_env =
            check_tainted_lval env (mk_prop_lval ())
          in
          Some (taints, shape, lval_env)
      | [ _ ] when String.starts_with ~prefix:"set" method_str ->
          if not (Taints.is_empty all_args_taints) then
            Some
              ( Taints.empty,
                Bot,
                env.lval_env
                |> Lval_env.add_lval (mk_prop_lval ()) all_args_taints )
          else Some (Taints.empty, Bot, env.lval_env)
      | __else__ -> None)
  | __else__ -> None

and check_tainted_lval_aux env (lval : IL.lval) :
    Taints.t
    * Xtaint.t_or_sanitized
    * S.shape
    * [ `Sub of Taints.t * S.shape ]
    * Lval_env.t =
  (* Recursively checks an l-value bottom-up.
   *
   *  This check needs to combine matches from pattern-{sources,sanitizers,sinks}
   *  with the info we have stored in `env.lval_env`. This can be subtle, see
   *  comments below.
   *)
  match lval_is_best_sanitizer env lval with
  (* See NOTE [is_sanitizer] *)
  (* TODO: We should check that taint and sanitizer(s) are unifiable. *)
  | _ :: _ as sanitizer_pms ->
      (* NOTE [lval/sanitized]:
       *  If lval is sanitized, then we will "bubble up" the `Sanitized status, so
       *  any taint recorded in lval_env for any extension of lval will be discarded.
       *
       *  So, if we are checking `x.a.b.c` and `x.a` is sanitized then any extension
       *  of `x.a` is considered sanitized as well, and we do look for taint info in
       *  the environment.
       *
       *  *IF* sanitization is side-effectful then any taint info will be removed
       *  from lval_env by sanitize_lval, but that is not guaranteed.
       *)
      let lval_env =
        sanitize_lval_by_side_effect env.lval_env sanitizer_pms lval
      in
      (Taints.empty, `Sanitized, Bot, `Sub (Taints.empty, Bot), lval_env)
  | [] ->
      (* Recursive call, check sub-lvalues first.
       *
       * It needs to be done bottom-up because any sub-lvalue can be a source and a
       * sink by itself, even if an extension of lval is not. For example, given
       * `x.a.b`, this lvalue may be considered sanitized, but at the same time `x.a`
       * could be tainted and considered a sink in some context. We cannot just check
       * `x.a.b` and forget about the sub-lvalues.
       *)
      let sub_new_taints, sub_in_env, sub_shape, lval_env =
        match lval with
        | { base; rev_offset = [] } ->
            (* Base case, no offset. *)
            check_tainted_lval_base env base
        | { base = _; rev_offset = _ :: rev_offset' } ->
            (* Recursive case, given `x.a.b` we must first check `x.a`. *)
            let sub_new_taints, sub_in_env, sub_shape, _sub_sub, lval_env =
              check_tainted_lval_aux env { lval with rev_offset = rev_offset' }
            in
            (sub_new_taints, sub_in_env, sub_shape, lval_env)
      in
      let sub_new_taints, sub_in_env =
        if env.taint_inst.options.taint_only_propagate_through_assignments then
          match sub_in_env with
          | `Sanitized -> (Taints.empty, `Sanitized)
          | `Clean
          | `None
          | `Tainted _ ->
              (Taints.empty, `None)
        else (sub_new_taints, sub_in_env)
      in
      (* Check the status of lval in the environemnt. *)
      let lval_in_env, lval_shape =
        match sub_in_env with
        | `Sanitized ->
            (* See NOTE [lval/sanitized] *)
            (`Sanitized, S.Bot)
        | (`Clean | `None | `Tainted _) as sub_xtaint ->
            let xtaint', shape =
              (* THINK: Should we just use 'Sig.find_in_shape' directly here ?
                       We have the 'sub_shape' available. *)
              match Lval_env.find_lval lval_env lval with
              | None -> (`None, S.Bot)
              | Some (Cell (xtaint', shape)) -> (xtaint', shape)
            in
            let xtaint' =
              match xtaint' with
              | (`Clean | `Tainted _) as xtaint' -> xtaint'
              | `None ->
                  (* HACK(field-sensitivity): If we encounter `obj.x` and `obj` has
                     * polymorphic taint, and we know nothing specific about `obj.x`, then
                     * we add the same offset `.x` to the polymorphic taint coming from `obj`.
                     * (See also 'propagate_taint_via_unresolved_java_getters_and_setters'.)
                     *
                     * For example, given `function foo(o) { sink(o.x); }`, and being '0 the
                     * polymorphic taint of `o`, this allows us to record that what goes into
                     * the sink is '0.x (and not just '0). So if later we encounter `foo(obj)`
                     * where `obj.y` is tainted but `obj.x` is not tainted, we will not
                     * produce a finding.
                  *)
                  fix_poly_taint_with_field lval sub_xtaint
            in
            (xtaint', shape)
      in
      let taints_from_env = Xtaint.to_taints lval_in_env in
      (* Find taint sources matching lval. *)
      let current_taints = Taints.union sub_new_taints taints_from_env in
      let taints_from_sources, lval_env =
        find_lval_taint_sources { env with lval_env } current_taints lval
      in
      (* Check sub-expressions in the offset. *)
      let taints_from_offset, lval_env =
        match lval.rev_offset with
        | [] -> (Taints.empty, lval_env)
        | offset :: _ -> check_tainted_lval_offset { env with lval_env } offset
      in
      (* Check taint propagators. *)
      let taints_incoming (* TODO: find a better name *) =
        if env.taint_inst.options.taint_only_propagate_through_assignments then
          taints_from_sources
        else
          sub_new_taints
          |> Taints.union taints_from_sources
          |> Taints.union taints_from_offset
      in
      let taints_propagated, lval_env =
        handle_taint_propagators { env with lval_env } (`Lval lval)
          (taints_incoming |> Taints.union taints_from_env)
          lval_shape
      in
      let new_taints = taints_incoming |> Taints.union taints_propagated in
      let sinks =
        lval_is_sink env lval
        (* For sub-lvals we require sinks to be exact matches. Why? Let's say
           * we have `sink(x.a)` and `x' is tainted but `x.a` is clean...
           * with the normal subset semantics for sinks we would consider `x'
           * itself to be a sink, and we would report a finding!
        *)
        |> List.filter TM.is_exact
        |> List_.map TM.sink_of_match
      in
      let all_taints = Taints.union taints_from_env new_taints in
      let effects =
        effects_of_tainted_sinks { env with lval_env } all_taints sinks
      in
      record_effects { env with lval_env } effects;
      ( new_taints,
        lval_in_env,
        lval_shape,
        `Sub (Xtaint.to_taints sub_in_env, sub_shape),
        lval_env )

and check_tainted_lval_base env base =
  match base with
  | Var _
  | VarSpecial _ ->
      (Taints.empty, `None, Bot, env.lval_env)
  | Mem { e = Fetch lval; _ } ->
      (* i.e. `*ptr` *)
      let taints, lval_in_env, shape, _sub, lval_env =
        check_tainted_lval_aux env lval
      in
      (taints, lval_in_env, shape, lval_env)
  | Mem e ->
      let taints, shape, lval_env = check_tainted_expr env e in
      (taints, `None, shape, lval_env)

and check_tainted_lval_offset env offset =
  match offset.o with
  | Dot _n ->
      (* THINK: Allow fields to be taint sources, sanitizers, or sinks ??? *)
      (Taints.empty, env.lval_env)
  | Index e ->
      let taints, _shape, lval_env = check_tainted_expr env e in
      let taints =
        if propagate_through_indexes env then taints
        else (* Taints from the index should be ignored. *)
          Taints.empty
      in
      (taints, lval_env)

and check_tainted_lval_with_sig env lval eorig =
  let exp = { e = Fetch lval; eorig } in
  let taints, shape, _sub, lval_env = check_tainted_lval env lval in
  match (shape, lookup_signature env exp) with
  | __any__, None -> (taints, shape, lval_env)
  | S.Bot, Some (fun_sig, `Fun) ->
      (* This 'lval' is a function, with a known taint signature, so we give it a
         `Fun` shape. *)
      (taints, S.Fun fun_sig, lval_env)
  | _non_Bot_shape, Some (fun_sig, `Fun) ->
      (* A top-level function/method is expected to have shape 'Bot'. *)
      Log.warn (fun m ->
          m "'%s' has a taint signature (%s) but has also shape '%s'"
            (Display_IL.string_of_exp exp)
            (Signature.show fun_sig) (S.show_shape shape));
      (taints, shape, lval_env)
  | __any__, Some (var_sig, `Var) -> (
      (* We instantiate 'var_sig' as if 'lval' were a 0-arity function. *)
      match
        instantiate_function_signature { env with lval_env } exp var_sig None []
      with
      | Some (call_taints, call_shape, lval_env) ->
          ( taints |> Taints.union call_taints,
            shape |> Shape.unify_shape call_shape,
            lval_env )
      | None -> (taints, shape, lval_env))

(* Test whether an expression is tainted, and if it is also a sink,
 * report the finding too (by side effect). *)
and check_tainted_expr env exp : Taints.t * S.shape * Lval_env.t =
  let check env = check_tainted_expr env in
  let check_subexpr exp =
    match exp.e with
    | Fetch _
    (* TODO: 'Fetch' is handled specially, this case should not never be taken.  *)
    | Literal _
    | FixmeExp (_, _, None) ->
        (Taints.empty, S.Bot, env.lval_env)
    | FixmeExp (_, _, Some e) ->
        let taints, shape, lval_env = check env e in
        let taints =
          taints |> Taints.union (Shape.gather_all_taints_in_shape shape)
        in
        (taints, S.Bot, lval_env)
    | Composite ((CTuple | CArray | CList), (_, es, _)) ->
        let taints_and_shapes, lval_env = map_check_expr env check es in
        let tuple_shape = Shape.tuple_like_obj taints_and_shapes in
        (Taints.empty, tuple_shape, lval_env)
    | Composite ((CSet | Constructor _ | Regexp), (_, es, _)) ->
        let taints, lval_env = union_map_taints_and_vars env check es in
        (taints, S.Bot, lval_env)
    | Operator ((op, _), es) ->
        let args_taints, all_args_taints, lval_env =
          check_function_call_arguments env es
        in
        let all_args_taints =
          all_args_taints
          |> Taints.union (gather_all_taints_in_args_taints args_taints)
        in
        let all_args_taints =
          if env.taint_inst.options.taint_only_propagate_through_assignments
          then Taints.empty
          else all_args_taints
        in
        let op_taints =
          match op with
          | G.Eq
          | G.NotEq
          | G.PhysEq
          | G.NotPhysEq
          | G.Lt
          | G.LtE
          | G.Gt
          | G.GtE
          | G.Cmp
          | G.RegexpMatch
          | G.NotMatch
          | G.In
          | G.NotIn
          | G.Is
          | G.NotIs ->
              if env.taint_inst.options.taint_assume_safe_comparisons then
                Taints.empty
              else all_args_taints
          | G.And
          | G.Or
          | G.Xor
          | G.Not
          | G.LSL
          | G.LSR
          | G.ASR
          | G.BitOr
          | G.BitXor
          | G.BitAnd
          | G.BitNot
          | G.BitClear
          | G.Plus
          | G.Minus
          | G.Mult
          | G.Div
          | G.Mod
          | G.Pow
          | G.FloorDiv
          | G.MatMult
          | G.Concat
          | G.Append
          | G.Range
          | G.RangeInclusive
          | G.NotNullPostfix
          | G.Length
          | G.Elvis
          | G.Nullish
          | G.Background
          | G.Pipe
          | G.LDA
          | G.RDA
          | G.LSA
          | G.RSA ->
              all_args_taints
        in
        (op_taints, S.Bot, lval_env)
    | RecordOrDict fields ->
        (* TODO: Construct a proper record/dict shape here. *)
        let (lval_env, taints), taints_and_shapes =
          fields
          |> List.fold_left_map
               (fun (lval_env, taints_acc) field ->
                 match field with
                 | Field (id, e) ->
                     (* TODO: Check 'id' for taint? *)
                     let e_taints, e_shape, lval_env =
                       check { env with lval_env } e
                     in
                     ((lval_env, taints_acc), `Field (id, e_taints, e_shape))
                 | Spread e ->
                     let e_taints, e_shape, lval_env =
                       check { env with lval_env } e
                     in
                     ((lval_env, e_taints), `Spread e_shape)
                 | Entry (ke, ve) ->
                     let ke_taints, ke_shape, lval_env =
                       check { env with lval_env } ke
                     in
                     let taints_acc =
                       taints_acc |> Taints.union ke_taints
                       |> Taints.union
                            (Shape.gather_all_taints_in_shape ke_shape)
                     in
                     let ve_taints, ve_shape, lval_env =
                       check { env with lval_env } ve
                     in
                     ((lval_env, taints_acc), `Entry (ke, ve_taints, ve_shape)))
               (env.lval_env, Taints.empty)
        in
        let record_shape = Shape.record_or_dict_like_obj taints_and_shapes in
        (taints, record_shape, lval_env)
    | Cast (_, e) -> check env e
  in
  match exp_is_sanitized env exp with
  (* THINK: Can we just skip checking the subexprs in 'exp'? There could be a
   * sanitizer by-side-effect that will not trigger, see CODE-6548. E.g.
   * if `x` in `foo(x)` is supposed to be sanitized by-side-effect, but `foo(x)`
   * itself is sanitized, the by-side-effect sanitization of `x` will not happen.
   * Problem is, we do not want sources or propagators by-side-effect to trigger
   * on `x` if `foo(x)` is sanitized, so we would need to check the subexprs while
   * disabling taint sources.
   *)
  | Some lval_env ->
      (* TODO: We should check that taint and sanitizer(s) are unifiable. *)
      (Taints.empty, Bot, lval_env)
  | None ->
      let taints, shape, lval_env =
        match exp.e with
        | Fetch lval -> check_tainted_lval_with_sig env lval exp.eorig
        | __else__ ->
            let taints_exp, shape, lval_env = check_subexpr exp in
            let taints_sources, lval_env =
              orig_is_best_source env exp.eorig
              |> taints_of_matches { env with lval_env } ~incoming:taints_exp
            in
            let taints = taints_exp |> Taints.union taints_sources in
            let taints_propagated, lval_env =
              handle_taint_propagators { env with lval_env } (`Exp exp) taints
                shape
            in
            let taints = Taints.union taints taints_propagated in
            (taints, shape, lval_env)
      in
      check_orig_if_sink env exp.eorig taints shape;
      (taints, shape, lval_env)

(* Check the actual arguments of a function call. This also handles left-to-right
 * taint propagation by chaining the 'lval_env's returned when checking the arguments.
 * For example, given `foo(x.a)` we'll check whether `x.a` is tainted or whether the
 * argument is a sink. *)
and check_function_call_arguments env args =
  let (rev_taints, lval_env), args_taints =
    args
    |> List.fold_left_map
         (fun (rev_taints, lval_env) arg ->
           let e = IL_helpers.exp_of_arg arg in
           let taints, shape, lval_env =
             check_tainted_expr { env with lval_env } e
           in
           let taints =
             check_type_and_drop_taints_if_bool_or_number env taints
               type_of_expr e
           in
           let new_acc = (taints :: rev_taints, lval_env) in
           match arg with
           | Unnamed _ -> (new_acc, Unnamed (taints, shape))
           | Named (id, _) -> (new_acc, Named (id, (taints, shape))))
         ([], env.lval_env)
  in
  let all_args_taints = List.fold_left Taints.union Taints.empty rev_taints in
  (args_taints, all_args_taints, lval_env)

and check_tainted_var env (var : IL.name) : Taints.t * S.shape * Lval_env.t =
  let taints, shape, _sub, lval_env =
    check_tainted_lval env (LV.lval_of_var var)
  in
  (taints, shape, lval_env)

and instantiate_function_signature env fun_exp fun_sig args args_taints =
  let* call_effects =
    Sig_inst.instantiate_function_signature env.lval_env fun_sig ~callee:fun_exp
      ~args args_taints
  in
  Some
    (call_effects
    |> List.fold_left
         (fun (taints_acc, shape_acc, lval_env)
              (call_effect : Sig_inst.call_effect) ->
           match call_effect with
           | ToSink
               {
                 taints_with_precondition = incoming_taints, _requires;
                 sink;
                 _;
               } ->
               effects_of_tainted_sink env incoming_taints sink
               |> record_effects env;
               (taints_acc, shape_acc, lval_env)
           | ToReturn
               {
                 data_taints = taints;
                 data_shape = shape;
                 control_taints;
                 return_tok = _;
               } ->
               ( Taints.union taints taints_acc,
                 Shape.unify_shape shape shape_acc,
                 Lval_env.add_control_taints lval_env control_taints )
           | ToLval (taints, var, offset) ->
               ( taints_acc,
                 shape_acc,
                 lval_env |> Lval_env.add var offset taints ))
         (Taints.empty, Bot, env.lval_env))

(* This function is consuming the taint signature of a function to determine
   a few things:
   1) What is the status of taint in the current environment, after the function
      call occurs?
   2) Are there any effects that occur within the function due to taints being
      input into the function body, from the calling context?
*)
and check_function_call env fun_exp (args : exp argument list)
    (args_taints : (Taints.t * S.shape) argument list) :
    (Taints.t * S.shape * Lval_env.t) option =
  match lookup_fun_signature env fun_exp with
  | Some fun_sig ->
      Log.debug (fun m ->
          m ~tags:sigs_tag "Call to %s : %s"
            (Display_IL.string_of_exp fun_exp)
            (Signature.show fun_sig));
      instantiate_function_signature env fun_exp fun_sig (Some args) args_taints
  | None ->
      Log.info (fun m ->
          m "No taint signature found for `%s'"
            (Display_IL.string_of_exp fun_exp));
      None

let check_function_call_callee env e =
  match e.e with
  | Fetch ({ base = _; rev_offset = _ :: _ } as lval) ->
      (* Method call <object ...>.<method>, the 'sub_taints' and 'sub_shape'
       * correspond to <object ...>. *)
      let taints, shape, `Sub (sub_taints, sub_shape), lval_env =
        check_tainted_lval env lval
      in
      let obj_taints =
        sub_taints |> Taints.union (Shape.gather_all_taints_in_shape sub_shape)
      in
      (`Obj obj_taints, taints, shape, lval_env)
  | __else__ ->
      let taints, shape, lval_env = check_tainted_expr env e in
      (`Fun, taints, shape, lval_env)

let mk_lambda_in_env env lcfg =
  (* We do some processing of the lambda parameters but it's mainly
   * to enable taint propagation, e.g.
   *
   *     obj.do_something(lambda x: sink(x))
   *
   * so we can propagate taint from `obj` to `x`.
   *)
  lcfg.params
  |> Fold_IL_params.fold
       (fun lval_env id id_info _pdefault ->
         let var = AST_to_IL.var_of_id_info id id_info in
         (* This is a *new* variable, so we clean any taint that we may have
            attached to it previously. This can happen when a lambda is called
            inside a loop. *)
         let lval_env = Lval_env.clean lval_env (LV.lval_of_var var) in
         (* Now check if the parameter is itself a taint source. *)
         let taints, shape, lval_env =
           check_tainted_var { env with lval_env } var
         in
         lval_env |> Lval_env.add_lval_shape (LV.lval_of_var var) taints shape)
       env.lval_env

let check_lambda env lval fdef =
  if env.taint_inst.options.taint_interproc_lambdas then
    let opt_lshape =
      let* lname, lcfg = LV.lval_is_lambda env.lambdas lval in
      let in_env = mk_lambda_in_env env lcfg in
      let* hook = !hook_infer_sig_for_lambda in
      let lsig = hook env.taint_inst env.func ~in_env lname fdef lcfg in
      Some (S.Fun lsig)
    in
    (Taints.empty, opt_lshape ||| S.Bot, env.lval_env)
  else (Taints.empty, S.Bot, env.lval_env)

(* Test whether an instruction is tainted, and if it is also a sink,
 * report the effect too (by side effect). *)
let check_tainted_instr env instr : Taints.t * S.shape * Lval_env.t =
  let check_expr env = check_tainted_expr env in
  let check_instr = function
    | Assign (_, e) ->
        let taints, shape, lval_env = check_expr env e in
        let taints =
          check_type_and_drop_taints_if_bool_or_number env taints type_of_expr e
        in
        (taints, shape, lval_env)
    | AssignAnon (lval, Lambda fdef) -> check_lambda env lval fdef
    | AssignAnon _ -> (Taints.empty, Bot, env.lval_env)
    | Call (_, e, args) ->
        let args_taints, all_args_taints, lval_env =
          check_function_call_arguments env args
        in
        let all_args_taints =
          all_args_taints
          |> Taints.union (gather_all_taints_in_args_taints args_taints)
        in
        let e_obj, e_taints, e_shape, lval_env =
          check_function_call_callee { env with lval_env } e
        in
        (* NOTE(sink_has_focus):
         * After we made sink specs "exact" by default, we need this trick to
         * be backwards compatible wrt to specifications like `sink(...)`. Even
         * if the sink is "exact", if it has NO focus, then we consider that all
         * of the parameters of the function are sinks. So, even if
         * `taint_assume_safe_functions: true`, if the spec is `sink(...)`, we
         * still report `sink(tainted)`.
         *)
        check_orig_if_sink { env with lval_env } instr.iorig all_args_taints Bot
          ~filter_sinks:(fun m ->
            not (m.spec.sink_exact && m.spec.sink_has_focus));
        let call_taints, shape, lval_env =
          match
            check_function_call { env with lval_env } e args args_taints
          with
          | Some (call_taints, shape, lval_env) ->
              (* THINK: For debugging, we could print a diff of the previous and new lval_env *)
              Log.debug (fun m ->
                  m ~tags:sigs_tag "- Instantiating %s: returns %s & %s"
                    (Display_IL.string_of_exp e)
                    (T.show_taints call_taints)
                    (S.show_shape shape));
              (call_taints, shape, lval_env)
          | None -> (
              let call_taints =
                if not (propagate_through_functions env) then Taints.empty
                else
                  (* Otherwise assume that the function will propagate
                     * the taint of its arguments. *)
                  all_args_taints
              in
              match
                propagate_taint_via_java_getters_and_setters_without_definition
                  { env with lval_env } e args all_args_taints
              with
              | Some (getter_taints, _TODOshape, lval_env) ->
                  (* HACK: Java: If we encounter `obj.setX(arg)` we interpret it as
                   * `obj.x = arg`, if we encounter `obj.getX()` we interpret it as
                   * `obj.x`. *)
                  let call_taints = Taints.union call_taints getter_taints in
                  (call_taints, Bot, lval_env)
              | None ->
                  (* We have no taint signature and it's neither a get/set method. *)
                  if not (propagate_through_functions env) then
                    (Taints.empty, Bot, lval_env)
                  else (
                    effects_of_call_func_arg e e_shape args_taints
                    |> record_effects { env with lval_env };
                    (* If this is a method call, `o.method(...)`, then we fetch the
                       * taint of the callee object `o`. This is a conservative worst-case
                       * asumption that any taint in `o` can be tainting the call's effect. *)
                    let call_taints =
                      match e_obj with
                      | `Fun -> call_taints
                      | `Obj obj_taints ->
                          call_taints |> Taints.union obj_taints
                    in
                    (call_taints, Bot, lval_env)))
        in
        (* We add the taint of the function itselt (i.e., 'e_taints') too. *)
        let all_call_taints =
          if env.taint_inst.options.taint_only_propagate_through_assignments
          then call_taints
          else Taints.union e_taints call_taints
        in
        let all_call_taints =
          check_type_and_drop_taints_if_bool_or_number env all_call_taints
            type_of_expr e
        in
        (all_call_taints, shape, lval_env)
    | New (_lval, _ty, Some constructor, args) -> (
        (* 'New' with reference to constructor, although it doesn't mean it has been resolved. *)
        let args_taints, all_args_taints, lval_env =
          check_function_call_arguments env args
        in
        match
          check_function_call { env with lval_env } constructor args args_taints
        with
        | Some (call_taints, shape, lval_env) -> (call_taints, shape, lval_env)
        | None ->
            let all_args_taints =
              all_args_taints
              |> Taints.union (gather_all_taints_in_args_taints args_taints)
            in
            let all_args_taints =
              if env.taint_inst.options.taint_only_propagate_through_assignments
              then Taints.empty
              else all_args_taints
            in
            (all_args_taints, Bot, lval_env))
    | New (_lval, _ty, None, args) ->
        (* 'New' without reference to constructor *)
        let args_taints, all_args_taints, lval_env =
          check_function_call_arguments env args
        in
        let all_args_taints =
          all_args_taints
          |> Taints.union (gather_all_taints_in_args_taints args_taints)
        in
        let all_args_taints =
          if env.taint_inst.options.taint_only_propagate_through_assignments
          then Taints.empty
          else all_args_taints
        in
        (all_args_taints, Bot, lval_env)
    | CallSpecial (_, _, args) ->
        let args_taints, all_args_taints, lval_env =
          check_function_call_arguments env args
        in
        let all_args_taints =
          all_args_taints
          |> Taints.union (gather_all_taints_in_args_taints args_taints)
        in
        let all_args_taints =
          if env.taint_inst.options.taint_only_propagate_through_assignments
          then Taints.empty
          else all_args_taints
        in
        (all_args_taints, Bot, lval_env)
    | FixmeInstr _ -> (Taints.empty, Bot, env.lval_env)
  in
  let sanitizer_pms = orig_is_best_sanitizer env instr.iorig in
  match sanitizer_pms with
  (* See NOTE [is_sanitizer] *)
  | _ :: _ ->
      (* TODO: We should check that taint and sanitizer(s) are unifiable. *)
      (Taints.empty, Bot, env.lval_env)
  | [] ->
      let taints_instr, rhs_shape, lval_env = check_instr instr.i in
      let taint_sources, lval_env =
        orig_is_best_source env instr.iorig
        |> taints_of_matches { env with lval_env } ~incoming:taints_instr
      in
      let taints = Taints.union taints_instr taint_sources in
      let taints_propagated, lval_env =
        handle_taint_propagators { env with lval_env } (`Ins instr) taints
          rhs_shape
      in
      let taints = Taints.union taints taints_propagated in
      check_orig_if_sink env instr.iorig taints rhs_shape;
      let taints =
        match LV.lval_of_instr_opt instr with
        | None -> taints
        | Some lval ->
            check_type_and_drop_taints_if_bool_or_number env taints type_of_lval
              lval
      in
      (taints, rhs_shape, lval_env)
[@@profiling]

(* Test whether a `return' is tainted, and if it is also a sink,
 * report the effect too (by side effect). *)
let check_tainted_return env tok e : Taints.t * S.shape * Lval_env.t =
  let sinks =
    any_is_best_sink env (G.Tk tok) @ orig_is_best_sink env e.eorig
    |> List.filter (TM.is_best_match env.func.best_matches)
    |> List_.map TM.sink_of_match
  in
  let taints, shape, var_env' = check_tainted_expr env e in
  let taints =
    (* TODO: Clean shape as well based on type ? *)
    check_type_and_drop_taints_if_bool_or_number env taints type_of_expr e
  in
  let effects = effects_of_tainted_sinks env taints sinks in
  record_effects env effects;
  (taints, shape, var_env')

let effects_from_arg_updates_at_exit ~in_lambda enter_env exit_env :
    Effect.t list =
  (* TODO: We need to get a map of `lval` to `Taint.arg`, and if an extension
   * of `lval` has new taints, then we can compute its correspoding `Taint.arg`
   * extension and generate a `ToLval` effect too. *)
  exit_env |> Lval_env.seq_of_tainted
  |> Seq.map (fun (var, exit_var_ref) ->
         match Lval_env.find_var enter_env var with
         | None when in_lambda && Tok.is_origintok (snd var.ident) ->
             (* The 'var' is in the 'exit_env', and although it's not in the
                'enter_env', we are analyzing a lambda, so this 'var' is of
                 interest to the enclosing function, and we want to generate
                 a ToLval here. *)
             Shape.enum_in_cell exit_var_ref
             |> Seq.filter_map (fun (offset, new_taints) ->
                    let lval = T.{ base = BVar var; offset } in
                    (* TODO: Also report if taints are _cleaned_. *)
                    if not (Taints.is_empty new_taints) then
                      Some (Effect.ToLval (new_taints, lval))
                    else None)
         | None ->
             (* For top-level functions we already set up an env that contains all
                the globals... so we expect any relevant variable to be found in
                'enter_env' and do nothing here... but maybe we should re-evaluate. *)
             Seq.empty
         | Some (Cell ((`Clean | `None), _)) -> Seq.empty
         | Some (Cell (`Tainted enter_taints, _)) -> (
             (* For each lval in the enter_env, we get its `T.lval`, and check
              * if it got new taints at the exit_env. If so, we generate a 'ToLval'. *)
             match
               enter_taints |> Taints.elements
               |> List_.filter_map (fun taint ->
                      match taint.T.orig with
                      | T.Var lval -> Some lval
                      | _ -> None)
             with
             | []
             | _ :: _ :: _ ->
                 Seq.empty
             | [ lval ] ->
                 Shape.enum_in_cell exit_var_ref
                 |> Seq.filter_map (fun (offset, exit_taints) ->
                        let lval =
                          { lval with offset = lval.offset @ offset }
                        in
                        let new_taints = Taints.diff exit_taints enter_taints in
                        (* TODO: Also report if taints are _cleaned_. *)
                        if not (Taints.is_empty new_taints) then
                          Some (Effect.ToLval (new_taints, lval))
                        else None)))
  |> Seq.concat |> List.of_seq

let check_tainted_control_at_exit node env =
  match node.F.n with
  (* This is only for implicit returns, we could handle 'NReturn' here too
   * but we would be generating duplicate effects. *)
  | NReturn _ -> ()
  | __else__ ->
      if node.IL.at_exit then
        let return_tok =
          (* Getting a token from an arbitrary node could be expensive
           * (see 'AST_generic_helpers.range_of_tokens'). We just use a
           * fake one but use the function's name if available to make
           * it unique. If it were not unique, the effects cache in
           * 'Deep_tainting' would consider all `ToReturn`s with the
           * same control taint as being the same, given that
           * `Taint.compare_source` does not compare the length of the
           * call trace. And that could cause some calls to be missing
           * in the call trace of a finding. *)
          match env.func.fname with
          | None -> G.fake "return"
          | Some name -> G.fake (IL.str_of_name name ^ "/return")
        in
        let effects =
          effects_of_tainted_return env Taints.empty Bot return_tok
        in
        record_effects env effects

let check_tainted_at_exit_sinks node env =
  match !hook_check_tainted_at_exit_sinks with
  | None -> ()
  | Some hook -> (
      match hook env.taint_inst env.lval_env node with
      | None -> ()
      | Some (taints_at_exit, sink_matches_at_exit) ->
          effects_of_tainted_sinks env taints_at_exit sink_matches_at_exit
          |> record_effects env)

(*****************************************************************************)
(* Transfer *)
(*****************************************************************************)

let input_env ~enter_env ~(flow : F.cfg) mapping ni =
  let node = flow.graph#nodes#assoc ni in
  match node.F.n with
  | Enter -> enter_env
  | _else -> (
      let pred_envs =
        CFG.predecessors flow ni
        |> List_.map (fun (pi, _) -> mapping.(pi).D.out_env)
      in
      match pred_envs with
      | [] -> Lval_env.empty
      | [ penv ] -> penv
      | penv1 :: penvs -> List.fold_left Lval_env.union penv1 penvs)

let rec transfer : env -> fun_cfg:F.fun_cfg -> Lval_env.t D.transfn =
 fun enter_env ~fun_cfg
     (* the transfer function to update the mapping at node index ni *)
       mapping ni ->
  let flow = fun_cfg.cfg in
  (* DataflowX.display_mapping flow mapping show_tainted; *)
  let in' : Lval_env.t =
    input_env ~enter_env:enter_env.lval_env ~flow mapping ni
  in
  let node = flow.graph#nodes#assoc ni in
  let env = { enter_env with lval_env = in' } in
  let out' : Lval_env.t =
    match node.F.n with
    | NInstr x ->
        let taints, shape, lval_env' = check_tainted_instr env x in
        let opt_lval = LV.lval_of_instr_opt x in
        let lval_env' =
          match opt_lval with
          | Some lval ->
              (* We call `check_tainted_lval` here because the assigned `lval`
               * itself could be annotated as a source of taint. *)
              let taints, lval_shape, _sub, lval_env' =
                check_tainted_lval { env with lval_env = lval_env' } lval
              in
              (* We check if the instruction is a sink, and if so the taints
               * from the `lval` could make a finding. *)
              check_orig_if_sink env x.iorig taints lval_shape;
              lval_env'
          | None -> lval_env'
        in
        let lval_env' =
          match opt_lval with
          | Some lval ->
              if Shape.taints_and_shape_are_relevant taints shape then
                (* Instruction returns tainted data, add taints to lval.
                 * See [Taint_lval_env] for details. *)
                lval_env' |> Lval_env.add_lval_shape lval taints shape
              else
                (* The RHS returns no taint, but taint could propagate by
                 * side-effect too. So, we check whether the taint assigned
                 * to 'lval' has changed to determine whether we need to
                 * clean 'lval' or not. *)
                let lval_taints_changed =
                  not (Lval_env.equal_by_lval in' lval_env' lval)
                in
                if lval_taints_changed then
                  (* The taint of 'lval' has changed, so there was a source or
                   * sanitizer acting by side-effect on this instruction. Thus we do NOT
                   * do anything more here. *)
                  lval_env'
                else
                  (* No side-effects on 'lval', and the instruction returns safe data,
                   * so we assume that the assigment acts as a sanitizer and therefore
                   * remove taints from lval. See [Taint_lval_env] for details. *)
                  Lval_env.clean lval_env' lval
          | None ->
              (* Instruction returns 'void' or its return value is ignored. *)
              lval_env'
        in
        lval_env'
    | NCond (_tok, e)
    | NThrow (_tok, e) ->
        let _taints, _shape, lval_env' = check_tainted_expr env e in
        lval_env'
    | NReturn (tok, e) ->
        (* TODO: Move most of this to check_tainted_return. *)
        let taints, shape, lval_env' = check_tainted_return env tok e in
        let effects = effects_of_tainted_return env taints shape tok in
        record_effects env effects;
        lval_env'
    | NGoto _
    | Enter
    | Exit
    | TrueNode _
    | FalseNode _
    | Join
    | NOther _
    | NTodo _ ->
        in'
  in
  let effects_lambdas, out' =
    do_lambdas { env with lval_env = out' } fun_cfg.lambdas node
  in
  env.effects_acc := Effects.union effects_lambdas !(env.effects_acc);
  let env_at_exit = { env with lval_env = out' } in
  check_tainted_control_at_exit node env_at_exit;
  check_tainted_at_exit_sinks node env_at_exit;
  Log.debug (fun m ->
      m ~tags:transfer_tag "Taint transfer %s%s\n  %s:\n  IN:  %s\n  OUT: %s"
        (Option.map IL.str_of_name env.func.fname ||| "<FUN>")
        (Option.map
           (fun lname -> spf "(in lambda %s)" (IL.str_of_name lname))
           env.in_lambda
        ||| "")
        (Display_IL.short_string_of_node_kind node.F.n)
        (Lval_env.to_string in') (Lval_env.to_string out'));
  { D.in_env = in'; out_env = out' }

(* In OSS, lambdas are mostly treated like statement blocks, that is, we
 * check the body of the lambda at the place where it is called, but we
 * do not "connect" actual arguments with formals, nor we track if the
 * lambda returns any taint.
 *
 * TODO: In Pro we should do inter-procedural analysis here. *)
and do_lambdas env (lambdas : IL.lambdas_cfgs) node =
  let node_is_call =
    (* See 'out_env' below. *)
    match node.F.n with
    | NInstr i -> (
        match i.i with
        | Call _
        | CallSpecial _
        | New _ ->
            true
        | Assign _
        | AssignAnon _
        | FixmeInstr _ ->
            false)
    | __else__ -> false
  in
  (* We visit lambdas at their "use" site (where they are fetched), so we can e.g.
   * propagate taint from an object receiving a method call, to a lambda being
   * passed to that method. *)
  let lambdas_to_analyze = lambdas_to_analyze_in_node env lambdas node in
  let num_lambdas = List.length lambdas_to_analyze in
  if num_lambdas > 0 then
    Log.debug (fun m ->
        m "There are %d lambda(s) occurring in: %s" num_lambdas
          (Display_IL.short_string_of_node_kind node.F.n));
  let effects_lambdas, out_envs_lambdas =
    lambdas_to_analyze
    |> List_.map (fun (lambda_name, lambda_cfg) ->
           let lambda_in_env = mk_lambda_in_env env lambda_cfg in
           match Lval_env.find_var env.lval_env lambda_name with
           | Some (S.Cell (_, S.Fun tsig)) ->
               if env.taint_inst.options.taint_interproc_lambdas then
                 let args_taints =
                   (* Enables propagation from the enclosing function to the
                      parameters of the lambda. *)
                   lambda_cfg.params
                   |> List_.map (function
                        | Param { pname; _ } -> (
                            match Lval_env.find_var lambda_in_env pname with
                            | None -> Named (pname.ident, (Taints.empty, S.Bot))
                            | Some (S.Cell (xtaints, shape)) ->
                                let taints = Xtaint.to_taints xtaints in
                                Named (pname.ident, (taints, shape)))
                        | PatternParam _ (* TODO *)
                        | FixmeParam ->
                            Unnamed (Taints.empty, S.Bot))
                 in
                 match
                   instantiate_function_signature env
                     { e = Fetch (LV.lval_of_var lambda_name); eorig = NoOrig }
                     tsig None args_taints
                 with
                 | None -> (Effects.empty, env.lval_env)
                 | Some (_taints, _shape, lval_env) ->
                     (* We are just interested in the side-effects of the lambda. *)
                     (Effects.empty, lval_env)
               else
                 (* This allows for propagators like:

                        $FROM.foobar($X => {
                          ...
                          $TO.$ANY(...)
                          ...
                        })

                    that do not work if we use inter-procedural analysis to check lambdas.

                    TODO: Make propagation work inter-proc, may be related to extra-requires?
                 *)
                 fixpoint_lambda env.taint_inst env.func env.needed_vars
                   lambda_name lambda_cfg lambda_in_env
           | Some _
           | None ->
               (* We are in OSS or we could not infer a taint signature for the lambda. *)
               fixpoint_lambda env.taint_inst env.func env.needed_vars
                 lambda_name lambda_cfg lambda_in_env)
    |> List_.split
  in
  let effects = Effects.union_list effects_lambdas in
  let out_env =
    if node_is_call then
      (* We only take the side-effects of the lambda into consideration if the
       * node is a call, so the lambda is either the callee or one of its arguments.
       * E.g.
       *
       *     do_something([]() { taint(p) });
       *     sink(p) // finding wanted
       *
       * We assume that these lambdas are being evaluated and that their side-effects
       * should affect the subsequent statements.
       *)
      Lval_env.union_list ~default:env.lval_env out_envs_lambdas
    else
      (* If lambdas are not part of a call, we don't make their side-effects visible.
       * E.g.
       *
       *     void test(int *p) {
       *       auto f1 = [&p]() {
       *         source(p);
       *       };
       *       auto f2 = [&p]() {
       *         sink(p); // NO finding wanted
       *       };
       *     }
       *)
      env.lval_env
  in
  (effects, out_env)

and fixpoint_lambda taint_inst func needed_vars lambda_name lambda_cfg in_env :
    Effects.t * Lval_env.t =
  Log.debug (fun m ->
      m "Analyzing lambda %s (%s)"
        (IL.str_of_name lambda_name)
        (Lval_env.to_string in_env));
  let effects, mapping =
    fixpoint_aux taint_inst func ~needed_vars ~enter_lval_env:in_env
      ~in_lambda:(Some lambda_name) lambda_cfg
  in
  let effects =
    effects
    |> Effects.filter (function
         | ToSink _
         | ToLval _
         | ToSinkInCall _ ->
             true
         | ToReturn _ -> false)
  in
  let out_env = mapping.(lambda_cfg.cfg.exit).Dataflow_core.out_env in
  let out_env' =
    out_env
    |> Lval_env.filter_tainted (fun var -> IL.NameSet.mem var needed_vars)
  in
  Log.debug (fun m ->
      m ~tags:transfer_tag "Lambda out_env %s --FILTER(%s)--> %s"
        (Lval_env.to_string out_env)
        (IL.NameSet.show needed_vars)
        (Lval_env.to_string out_env'));
  (effects, out_env')

and fixpoint_aux taint_inst func ?(needed_vars = IL.NameSet.empty)
    ~enter_lval_env ~in_lambda fun_cfg =
  let flow = fun_cfg.cfg in
  let init_mapping = DataflowX.new_node_array flow Lval_env.empty_inout in
  let needed_vars =
    needed_vars
    |> IL.NameSet.union
         (Taint_lambdas.find_vars_to_track_across_lambdas fun_cfg)
  in
  let env =
    {
      taint_inst;
      func;
      in_lambda;
      lambdas = fun_cfg.lambdas;
      lval_env = enter_lval_env;
      needed_vars;
      effects_acc = ref Effects.empty;
    }
  in
  (* THINK: Why I cannot just update mapping here ? if I do, the mapping gets overwritten later on! *)
  (* DataflowX.display_mapping flow init_mapping show_tainted; *)
  let end_mapping, timeout =
    DataflowX.fixpoint ~timeout:Limits_semgrep.taint_FIXPOINT_TIMEOUT
      ~eq_env:Lval_env.equal ~init:init_mapping ~trans:(transfer env ~fun_cfg)
      ~forward:true ~flow
  in
  log_timeout_warning taint_inst env.func.fname timeout;
  let exit_lval_env = end_mapping.(flow.exit).D.out_env in
  effects_from_arg_updates_at_exit ~in_lambda:(Option.is_some in_lambda)
    enter_lval_env exit_lval_env
  |> record_effects env;
  (!(env.effects_acc), end_mapping)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

and (fixpoint :
      Taint_rule_inst.t ->
      ?in_env:Lval_env.t ->
      ?name:IL.name ->
      F.fun_cfg ->
      Effects.t * mapping) =
 fun taint_inst ?(in_env = Lval_env.empty) ?name:opt_name fun_cfg ->
  let best_matches =
    (* Here we compute the "canonical" or "best" source/sanitizer/sink matches,
     * for each source/sanitizer/sink we check whether there is a "best match"
     * among all the potential matches in the CFG.
     * See NOTE "Best matches" *)
    fun_cfg
    |> TM.best_matches_in_nodes ~sub_matches_of_orig:(fun orig ->
           let sources =
             orig_is_source taint_inst orig
             |> List.to_seq
             |> Seq.filter (fun (m : R.taint_source TM.t) ->
                    m.spec.source_exact)
             |> Seq.map (fun m -> TM.Any m)
           in
           let sanitizers =
             orig_is_sanitizer taint_inst orig
             |> List.to_seq
             |> Seq.filter (fun (m : R.taint_sanitizer TM.t) ->
                    m.spec.sanitizer_exact)
             |> Seq.map (fun m -> TM.Any m)
           in
           let sinks =
             orig_is_sink taint_inst orig
             |> List.to_seq
             |> Seq.filter (fun (m : R.taint_sink TM.t) -> m.spec.sink_exact)
             |> Seq.map (fun m -> TM.Any m)
           in
           sources |> Seq.append sanitizers |> Seq.append sinks)
  in
  let used_lambdas = lambdas_used_in_cfg fun_cfg in
  let func = { fname = opt_name; best_matches; used_lambdas } in
  fixpoint_aux taint_inst func ~enter_lval_env:in_env ~in_lambda:None fun_cfg
[@@profiling]

let fixpoint taint_inst ?in_env ?name fun_cfg =
  fixpoint taint_inst ?in_env ?name fun_cfg
[@@profiling]
