(* Yoann Padioleau
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
open Common
open IL
module G = AST_generic
module F = IL
module D = Dataflow_core
module VarMap = Dataflow_core.VarMap
module PM = Pattern_match
module R = Rule
module LV = IL_lvalue_helpers
module T = Taint
module Taints = T.Taint_set

let logger = Logging.get_logger [ __MODULE__ ]

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Tainting dataflow analysis.
 *
 * This is a very rudimentary tainting analysis.
 * It is a MAY analysis, it finds *potential* bugs (the tainted path could not
 * be feasible in practice).
 * Very coarse grained (taint whole array/object).
 * This is step1 for taint tracking support in semgrep.
 * This was originally in semgrep-core/src/analyze, but it now depends on Pattern_match,
 * so it was moved to semgrep-core/src/engine
 *)

let ( let* ) = Option.bind

module DataflowX = Dataflow_core.Make (struct
  type node = F.node
  type edge = F.edge
  type flow = (node, edge) CFG.t

  let short_string_of_node n = Display_IL.short_string_of_node_kind n.F.n
end)

module LabelSet = Set.Make (String)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type var = Dataflow_core.var
type overlap = float
type 'spec tmatch = { spec : 'spec; pm : PM.t; overlap : overlap }

type a_propagator = {
  kind : [ `From | `To ];
  prop : R.taint_propagator;
  var : var;
}

type config = {
  filepath : Common.filename;
  rule_id : string;
  is_source : G.any -> Rule.taint_source tmatch list;
  is_propagator : AST_generic.any -> a_propagator tmatch list;
  is_sink : G.any -> R.taint_sink tmatch list;
  is_sanitizer : G.any -> R.taint_sanitizer tmatch list;
  unify_mvars : bool;
  handle_findings :
    var option -> T.finding list -> Taints.t Dataflow_core.env -> unit;
}

type mapping = Taints.t Dataflow_core.mapping

(* HACK: Tracks tainted functions intrafile. *)
type fun_env = (var, Taints.t) Hashtbl.t
type var_env = Taints.t VarMap.t

type env = {
  config : config;
  fun_name : var option;
  fun_env : fun_env;
  var_env : var_env;
}

(*****************************************************************************)
(* Hooks *)
(*****************************************************************************)

let hook_function_taint_signature = ref None

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let union_vars = Dataflow_core.varmap_union Taints.union

let union_map_taints_and_vars env f xs =
  xs
  |> List.fold_left
       (fun (taints1, var_env) x ->
         let taints2, var_env = f { env with var_env } x in
         (Taints.union taints1 taints2, var_env))
       (Taints.empty, env.var_env)

(* Debug *)
let _show_env =
  let (env_to_str : ('a -> string) -> 'a VarMap.t -> string) =
   fun val2str env ->
    VarMap.fold (fun dn v s -> s ^ dn ^ ":" ^ val2str v ^ " ") env ""
  in
  env_to_str T.show_taints

let str_of_name name = spf "%s:%d" (fst name.ident) name.sid
let orig_is_source config orig = config.is_source (any_of_orig orig)
let orig_is_sanitized config orig = config.is_sanitizer (any_of_orig orig)
let orig_is_sink config orig = config.is_sink (any_of_orig orig)
let trace_of_match x = T.trace_of_pm (x.pm, x.spec)

let taints_of_matches xs =
  xs |> Common.map (fun x -> (x.pm, x.spec)) |> T.taints_of_pms

let report_findings env findings =
  if findings <> [] then
    env.config.handle_findings env.fun_name findings env.var_env

let unify_mvars_sets mvars1 mvars2 =
  let xs =
    List.fold_left
      (fun xs (mvar, mval) ->
        xs >>= fun xs ->
        match List.assoc_opt mvar mvars2 with
        | None -> Some ((mvar, mval) :: xs)
        | Some mval' ->
            if Metavariable.equal_mvalue mval mval' then
              Some ((mvar, mval) :: xs)
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

(* Merge source's and sink's bound metavariables. *)
let merge_source_sink_mvars env source_mvars sink_mvars =
  if env.config.unify_mvars then
    (* This used to be the default, but it turned out to be confusing even for
     * r2c's security team! Typically you think of `pattern-sources` and
     * `pattern-sinks` as independent. We keep this option mainly for
     * backwards compatibility, it may be removed later on if no real use
     * is found. *)
    unify_mvars_sets source_mvars sink_mvars
  else
    (* The union of both sets, but taking the sink mvars in case of collision. *)
    sink_biased_union_mvars source_mvars sink_mvars

let labels_in_taint taints : LabelSet.t =
  taints |> Taints.to_seq
  |> Seq.filter_map (fun (t : T.taint) ->
         match t.orig with
         | Src src ->
             let _, ts = T.pm_of_trace src in
             Some ts.Rule.label
         | Arg _ -> None)
  |> LabelSet.of_seq

let rec eval_label_requires ~labels e =
  match e.G.e with
  | G.L (G.Bool (v, _)) -> v
  | G.N (G.Id (id, _)) ->
      let str, _ = id in
      LabelSet.mem str labels
  | G.Call ({ e = G.IdSpecial (G.Op G.Not, _); _ }, (_, [ Arg e1 ], _)) ->
      not (eval_label_requires ~labels e1)
  | G.Call ({ e = G.IdSpecial (G.Op op, _); _ }, (_, args, _)) -> (
      match (op, eval_label_args ~labels args) with
      | G.And, xs -> List.fold_left ( && ) true xs
      | G.Or, xs -> List.fold_left ( || ) false xs
      | _ ->
          logger#error "Unexpected Boolean operator";
          false)
  | G.ParenExpr (_, e, _) -> eval_label_requires ~labels e
  | ___else__ ->
      logger#error "Unexpected `requires' expression";
      false

and eval_label_args ~labels args =
  match args with
  | [] -> []
  | G.Arg e :: args' ->
      eval_label_requires ~labels e :: eval_label_args ~labels args'
  | _ :: args' ->
      logger#error "Unexpected argument kind";
      false :: eval_label_args ~labels args'

(* Produces a finding for every taint source that is unifiable with the sink. *)
let findings_of_tainted_sink env taints (sink : T.sink) : T.finding list =
  let ( let* ) = Option.bind in
  let labels = labels_in_taint taints in
  let sink_pm, ts = T.pm_of_trace sink in
  let req = eval_label_requires ~labels ts.requires in
  if req then
    (* TODO: With taint labels it's less clear what is "the source",
     * in fact, there could be many sources, each one providing a
     * different label. Here these would be reported as different
     * findings... We would need to compute subsets of taints satisfying
     * the `requires`, and then have multiple sources in `SrcToSink`! *)
    taints |> Taints.elements
    |> List.filter_map (fun (taint : T.taint) ->
           let tokens = List.rev taint.tokens in
           match taint.orig with
           | Arg i ->
               (* We need to check unifiability at the call site. *)
               Some (T.ArgToSink (i, tokens, sink))
           | Src source ->
               let src_pm, _ = T.pm_of_trace source in
               let* merged_env =
                 merge_source_sink_mvars env sink_pm.PM.env src_pm.PM.env
               in
               Some (T.SrcToSink { source; tokens; sink; merged_env }))
  else []

(* Produces a finding for every unifiable source-sink pair. *)
let findings_of_tainted_sinks env taints sinks : T.finding list =
  sinks |> List.concat_map (findings_of_tainted_sink env taints)

let findings_of_tainted_return taints return_tok : T.finding list =
  taints |> Taints.elements
  |> Common.map (fun (taint : T.taint) ->
         let tokens = List.rev taint.tokens in
         match taint.orig with
         | T.Arg i -> T.ArgToReturn (i, tokens, return_tok)
         | T.Src src -> T.SrcToReturn (src, tokens, return_tok))

let union_taints_filtering_labels ~new_ curr =
  let labels = labels_in_taint curr in
  Taints.fold
    (fun new_taint taints ->
      match new_taint.orig with
      | Arg _ -> Taints.add new_taint taints
      | Src src ->
          let _, ts = T.pm_of_trace src in
          let req = eval_label_requires ~labels ts.requires in
          if req then Taints.add new_taint taints else taints)
    new_ curr

(*****************************************************************************)
(* Tainted *)
(*****************************************************************************)

let sanitize_var var_env sanitizer_pms var =
  let var_is_now_safe =
    (* If the variable is an exact match (overlap > 0.99) for a sanitizer
       * annotation, then we infer that the variable itself has been updated
       * (presumably by side-effect) and is no longer tainted. We will update
       * the environment (i.e., `var_env') accordingly. *)
    List.exists (fun x -> x.overlap > 0.99) sanitizer_pms
  in
  if var_is_now_safe then VarMap.remove (str_of_name var) var_env else var_env

(* Check if an expression is sanitized, if so, return a new variable environment. *)
let exp_is_sanitized env exp =
  match orig_is_sanitized env.config exp.eorig with
  | [] -> None
  | sanitizer_pms -> (
      match exp.e with
      | Fetch { base = Var var; offset = NoOffset; _ } ->
          Some (sanitize_var env.var_env sanitizer_pms var)
      | _ -> Some env.var_env)

let add_taint_to_strid_in_env var_env strid taints =
  if Taints.is_empty taints then var_env
  else
    VarMap.update strid
      (function
        | None -> Some taints
        (* THINK: couldn't we just replace the existing taints? *)
        | Some taints' -> Some (Taints.union taints taints'))
      var_env

(* Add `var -> taints` to `var_env`. *)
let add_taint_to_var_in_env var_env var taints =
  let taints =
    let var_tok = snd var.ident in
    if Parse_info.is_fake var_tok then taints
    else taints |> Taints.map (fun t -> { t with tokens = var_tok :: t.tokens })
  in
  add_taint_to_strid_in_env var_env (str_of_name var) taints

let handle_taint_propagators env x taints =
  (* We propagate taints via an auxiliary variable (the propagator id). This is
   * simple but it has limitations, we can only propagate "forward" and, within
   * an instruction node, we can only propagate in the order in which we visit
   * the subexpressions. E.g. in `x.f(y,z)` we can propagate taint from `y` or
   * `z` to `x`, or from `y` to `z`; but we cannot propagate taint from `x` to
   * `y` or `z`, or from `z` to `y`. *)
  let var_env = env.var_env in
  let propagators =
    match x with
    | `Var var ->
        let _, tok = var.ident in
        if Parse_info.is_origintok tok then env.config.is_propagator (G.Tk tok)
        else []
    | `Exp exp -> env.config.is_propagator (any_of_orig exp.eorig)
    | `Ins ins -> env.config.is_propagator (any_of_orig ins.iorig)
  in
  let propagate_froms, propagate_tos =
    List.partition (fun p -> p.spec.kind = `From) propagators
  in
  let var_env =
    (* `x` is the source (the "from") of propagation, we add its taints to
     * the environment. *)
    List.fold_left
      (fun var_env prop ->
        add_taint_to_strid_in_env var_env prop.spec.var taints)
      var_env propagate_froms
  in
  let taints_incoming =
    (* `x` is the destination (the "to") of propagation. we collect all the
     * incoming taints by looking for the propagator ids in the environment. *)
    List.fold_left
      (fun taints_in_acc prop ->
        let taints_strid =
          VarMap.find_opt prop.spec.var var_env
          |> Option.value ~default:Taints.empty
        in
        Taints.union taints_in_acc taints_strid)
      Taints.empty propagate_tos
  in
  let taints = Taints.union taints taints_incoming in
  let var_env =
    match x with
    | `Var var ->
        (* If `x` is a variable, then taint is propagated by side-effect. This
         * allows us to e.g. propagate taint from `x` to `y` in `f(x,y)`. *)
        add_taint_to_var_in_env var_env var taints_incoming
    | `Exp _
    | `Ins _ ->
        var_env
  in
  (taints, var_env)

(* Test whether a variable occurrence is tainted, and if it is also a sink,
 * report the finding too (by side effect). *)
let check_tainted_var env (var : IL.name) : Taints.t * var_env =
  let source_pms, sanitizer_pms, sink_pms =
    let _, tok = var.ident in
    if Parse_info.is_origintok tok then
      ( env.config.is_source (G.Tk tok),
        env.config.is_sanitizer (G.Tk tok),
        env.config.is_sink (G.Tk tok) )
    else ([], [], [])
  in
  match sanitizer_pms with
  (* TODO: We should check that taint and sanitizer(s) are unifiable. *)
  | _ :: _ ->
      let var_env' = sanitize_var env.var_env sanitizer_pms var in
      (Taints.empty, var_env')
  | [] ->
      let mut_source_pms, reg_source_pms =
        (* If the variable is an exact match (overlap > 0.99) for a source
         * annotation, then we infer that the variable itself is now tainted
         * (presumably by side-effect) and we will update the `var_env`
         * accordingly. Otherwise the variable belongs to a piece of code that
         * is a source of taint, but it is not tainted on its own. *)
        List.partition (fun src -> src.overlap > 0.99) source_pms
      in
      let taints_sources_reg = reg_source_pms |> taints_of_matches
      and taints_sources_mut = mut_source_pms |> taints_of_matches
      and taints_var_env =
        VarMap.find_opt (str_of_name var) env.var_env
        |> Option.value ~default:Taints.empty
      and taints_fun_env =
        (* TODO: Move this to check_tainted_instr ? *)
        Hashtbl.find_opt env.fun_env (str_of_name var)
        |> Option.value ~default:Taints.empty
      in
      let var_env' =
        add_taint_to_var_in_env env.var_env var taints_sources_mut
      in
      let taints_sources = Taints.union taints_sources_reg taints_sources_mut in
      let prev_taints = Taints.union taints_var_env taints_fun_env in
      let taints : Taints.t =
        prev_taints |> union_taints_filtering_labels ~new_:taints_sources
      in
      let taints, var_env' =
        handle_taint_propagators
          { env with var_env = var_env' }
          (`Var var) taints
      in
      let sinks = sink_pms |> Common.map trace_of_match in
      let findings = findings_of_tainted_sinks env taints sinks in
      report_findings env findings;
      (taints, var_env')

(* Test whether an expression is tainted, and if it is also a sink,
 * report the finding too (by side effect). *)
let rec check_tainted_expr env exp : Taints.t * var_env =
  let check env = check_tainted_expr env in
  let check_base env = function
    | Var var -> check_tainted_var env var
    | VarSpecial _ -> (Taints.empty, env.var_env)
    | Mem e -> check env e
  in
  let check_offset env = function
    | Index e -> check env e
    | NoOffset
    | Dot _ ->
        (Taints.empty, env.var_env)
  in
  let check_subexpr exp =
    match exp.e with
    | Fetch { base = VarSpecial (This, _); offset = Dot fld; _ } ->
        (* TODO: Move this to check_tainted_instr ? *)
        let taints =
          Hashtbl.find_opt env.fun_env (str_of_name fld)
          |> Option.value ~default:Taints.empty
        in
        (taints, env.var_env)
    | Fetch { base; offset; _ } ->
        let base_taints, var_env = check_base env base in
        let offset_taints, var_env = check_offset { env with var_env } offset in
        (Taints.union base_taints offset_taints, var_env)
    | FixmeExp (_, _, Some e) -> check env e
    | Literal _
    | FixmeExp (_, _, None) ->
        (Taints.empty, env.var_env)
    | Composite (_, (_, es, _))
    | Operator (_, es) ->
        union_map_taints_and_vars env check es
    | Record fields ->
        union_map_taints_and_vars env
          (fun env -> function
            | Field (_, e)
            | Spread e ->
                check env e)
          fields
    | Cast (_, e) -> check env e
  in
  match exp_is_sanitized env exp with
  | Some var_env ->
      (* TODO: We should check that taint and sanitizer(s) are unifiable. *)
      (Taints.empty, var_env)
  | None ->
      let sinks =
        orig_is_sink env.config exp.eorig |> Common.map trace_of_match
      in
      let taints_sources =
        orig_is_source env.config exp.eorig |> taints_of_matches
      in
      let taints_exp, var_env = check_subexpr exp in
      let taints =
        taints_exp |> union_taints_filtering_labels ~new_:taints_sources
      in
      let taints, var_env =
        handle_taint_propagators { env with var_env } (`Exp exp) taints
      in
      let findings = findings_of_tainted_sinks env taints sinks in
      report_findings env findings;
      (taints, var_env)

let check_function_signature env fun_exp args_taints =
  let taints_of_arg i =
    let taint_opt = List.nth_opt args_taints i in
    if Option.is_none taint_opt then
      logger#error "cannot match taint variable with function arguments";
    taint_opt
  in
  match (!hook_function_taint_signature, fun_exp) with
  | ( Some hook,
      {
        e =
          Fetch
            {
              base =
                Var
                  {
                    ident;
                    id_info =
                      {
                        G.id_resolved =
                          {
                            contents =
                              Some ((G.ImportedEntity _ | G.ResolvedName _), _);
                          };
                        _;
                      };
                    _;
                  };
              offset = _;
              _;
            };
        eorig = SameAs eorig;
        _;
      } ) ->
      let* fun_sig = hook env.config eorig in
      Some
        (fun_sig
        |> List.filter_map (function
             | T.SrcToReturn (src, tokens, _return_tok) ->
                 let src = T.Call (eorig, tokens, src) in
                 Some (Taints.singleton { orig = Src src; tokens = [] })
             | T.ArgToReturn (i, tokens, _return_tok) ->
                 let* arg_taints = taints_of_arg i in
                 Some
                   (arg_taints
                   |> Taints.map (fun taint ->
                          let tokens =
                            List.rev_append tokens (snd ident :: taint.tokens)
                          in
                          { taint with tokens }))
             | T.ArgToSink (i, tokens, sink) ->
                 let sink = T.Call (eorig, tokens, sink) in
                 let* arg_taints = taints_of_arg i in
                 arg_taints
                 |> Taints.iter (fun t ->
                        findings_of_tainted_sink env (Taints.singleton t) sink
                        |> report_findings env);
                 None
             (* THINK: Should we report something here? *)
             | T.SrcToSink _ -> None)
        |> List.fold_left Taints.union Taints.empty)
  | None, _
  | Some _, _ ->
      None

(* Test whether an instruction is tainted, and if it is also a sink,
 * report the finding too (by side effect). *)
(* TODO: This should return a new var_env rather than just taint, it
 * makes more sense given that an instruction may have side-effects.
 * It Also makes simpler to handle sanitization by side-effect. *)
let check_tainted_instr env instr : Taints.t * var_env =
  let check_expr env = check_tainted_expr env in
  let check_instr = function
    | Assign (_, e) -> check_expr env e
    | AssignAnon _ -> (Taints.empty, env.var_env) (* TODO *)
    | Call (_, e, args) ->
        let args_taints, var_env =
          args
          |> List.fold_left_map
               (fun var_env arg ->
                 check_expr { env with var_env } arg |> Common2.swap)
               env.var_env
          |> Common2.swap
        in
        let e_taints, var_env = check_expr { env with var_env } e in
        let call_taints =
          match check_function_signature env e args_taints with
          | Some call_taints -> call_taints
          | None ->
              (* Default is to assume that the function will propagate
               * the taint of its arguments. *)
              List.fold_left Taints.union e_taints args_taints
        in
        (call_taints, var_env)
    | CallSpecial (_, _, args) -> union_map_taints_and_vars env check_expr args
    | FixmeInstr _ -> (Taints.empty, env.var_env)
  in
  let sanitizer_pms = orig_is_sanitized env.config instr.iorig in
  match sanitizer_pms with
  | _ :: _ ->
      (* TODO: We should check that taint and sanitizer(s) are unifiable. *)
      (Taints.empty, env.var_env)
  | [] ->
      let sinks =
        orig_is_sink env.config instr.iorig |> Common.map trace_of_match
      in
      let taint_sources =
        orig_is_source env.config instr.iorig |> taints_of_matches
      in
      let taints_instr, var_env' = check_instr instr.i in
      let taints =
        taints_instr |> union_taints_filtering_labels ~new_:taint_sources
      in
      let taints, var_env' =
        handle_taint_propagators
          { env with var_env = var_env' }
          (`Ins instr) taints
      in
      let findings = findings_of_tainted_sinks env taints sinks in
      report_findings env findings;
      (taints, var_env')

(* Test whether a `return' is tainted, and if it is also a sink,
 * report the finding too (by side effect). *)
let check_tainted_return env tok e : Taints.t * var_env =
  let sinks =
    env.config.is_sink (G.Tk tok) @ orig_is_sink env.config e.eorig
    |> Common.map trace_of_match
  in
  let taints, var_env' = check_tainted_expr env e in
  let findings = findings_of_tainted_sinks env taints sinks in
  report_findings env findings;
  (taints, var_env')

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
        |> Common.map (fun (pi, _) -> mapping.(pi).D.out_env)
      in
      match pred_envs with
      | [] -> VarMap.empty
      | [ penv ] -> penv
      | penv1 :: penvs -> List.fold_left union_vars penv1 penvs)

let (transfer :
      config ->
      fun_env ->
      Taints.t Dataflow_core.env ->
      string option ->
      flow:F.cfg ->
      Taints.t Dataflow_core.transfn) =
 fun config fun_env enter_env opt_name ~flow
     (* the transfer function to update the mapping at node index ni *)
       mapping ni ->
  (* DataflowX.display_mapping flow mapping show_tainted; *)
  let in' : Taints.t VarMap.t = input_env ~enter_env ~flow mapping ni in
  let node = flow.graph#nodes#assoc ni in
  let out' : Taints.t VarMap.t =
    let env = { config; fun_name = opt_name; fun_env; var_env = in' } in
    match node.F.n with
    | NInstr x -> (
        let taints, var_env' = check_tainted_instr env x in
        let var_env' =
          match LV.lvar_of_instr_opt x with
          | None -> var_env'
          | Some var ->
              (* We call `check_tainted_var` here because the assigned `var`
               * itself could be annotated as a source of taint. *)
              check_tainted_var { env with var_env = var_env' } var |> snd
        in
        match (Taints.is_empty taints, LV.lvar_of_instr_opt x) with
        (* Instruction returns safe data, remove taint from `var`. *)
        | true, Some var -> VarMap.remove (str_of_name var) var_env'
        (* Instruction returns tainted data, add taints to `var`. *)
        | false, Some var -> add_taint_to_var_in_env var_env' var taints
        (* There is no variable being assigned, presumably the Instruction
         * returns 'void'. *)
        | _, None -> var_env')
    | NReturn (tok, e) -> (
        (* TODO: Move most of this to check_tainted_return. *)
        let taints, var_env' = check_tainted_return env tok e in
        let findings = findings_of_tainted_return taints tok in
        report_findings env findings;
        let pmatches =
          taints
          |> Taints.filter (fun taint ->
                 match taint.T.orig with
                 | T.Src _ -> true
                 | T.Arg _ -> false)
        in
        match opt_name with
        | Some var ->
            (let str = var in
             match Hashtbl.find_opt fun_env str with
             | None ->
                 if not (Taints.is_empty pmatches) then
                   Hashtbl.add fun_env str pmatches
             | Some tained' ->
                 Hashtbl.replace fun_env str (Taints.union pmatches tained'));
            var_env'
        | None -> var_env')
    | _ -> in'
  in
  { D.in_env = in'; out_env = out' }

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let (fixpoint :
      ?in_env:Taints.t Dataflow_core.VarMap.t ->
      ?name:Dataflow_core.var ->
      ?fun_env:fun_env ->
      config ->
      F.cfg ->
      mapping) =
 fun ?in_env ?name:opt_name ?(fun_env = Hashtbl.create 1) config flow ->
  let init_mapping =
    DataflowX.new_node_array flow (Dataflow_core.empty_inout ())
  in
  let enter_env =
    match in_env with
    | None -> VarMap.empty
    | Some in_env -> in_env
  in
  (* THINK: Why I cannot just update mapping here ? if I do, the mapping gets overwritten later on! *)
  (* DataflowX.display_mapping flow init_mapping show_tainted; *)
  DataflowX.fixpoint ~eq:Taints.equal ~init:init_mapping
    ~trans:(transfer config fun_env enter_env opt_name ~flow)
      (* tainting is a forward analysis! *)
    ~forward:true ~flow
