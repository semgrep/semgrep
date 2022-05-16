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

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type var = Dataflow_core.var

type config = {
  filepath : Common.filename;
  rule_id : string;
  is_source : G.any -> PM.t list;
  is_sink : G.any -> PM.t list;
  is_sanitizer : G.any -> PM.t list;
  unify_mvars : bool;
  handle_findings :
    var option -> T.finding list -> T.taints Dataflow_core.env -> unit;
}

type mapping = T.taints Dataflow_core.mapping

(* HACK: Tracks tainted functions intrafile. *)
type fun_env = (var, PM.Set.t) Hashtbl.t

type env = {
  config : config;
  fun_name : var option;
  fun_env : fun_env;
  var_env : T.taints VarMap.t;
}

(*****************************************************************************)
(* Hooks *)
(*****************************************************************************)

let hook_function_taint_signature = ref None

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

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

let report_findings env findings =
  if findings <> [] then
    env.config.handle_findings env.fun_name findings env.var_env

let unify_mvars_sets mvars1 mvars2 =
  let xs =
    List.fold_left
      (fun xs (mvar, mval) ->
        let* xs = xs in
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

let union_map f xs =
  xs |> Common.map f |> List.fold_left Taints.union Taints.empty

(* Produces a finding for every taint source that is unifiable with the sink. *)
let findings_of_tainted_sink env taints (sink : T.sink) : T.finding list =
  let ( let* ) = Option.bind in
  taints |> Taints.elements
  |> List.filter_map (fun (taint : T.taint) ->
         let tokens = List.rev taint.tokens in
         match taint.orig with
         | Arg i ->
             (* We need to check unifiability at the call site. *)
             Some (T.ArgToSink (i, tokens, sink))
         | Src source ->
             let src_pm = T.pm_of_trace source in
             let sink_pm = T.pm_of_trace sink in
             let* merged_env =
               merge_source_sink_mvars env sink_pm.PM.env src_pm.PM.env
             in
             Some (T.SrcToSink { source; tokens; sink; merged_env }))

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

(*****************************************************************************)
(* Tainted *)
(*****************************************************************************)

(* Test whether a variable occurrence is tainted, and if it is also a sink,
 * report the finding too (by side effect). *)
let check_tainted_var env (var : IL.name) : T.taints =
  let source_pms, sanitizer_pms, sink_pms =
    let _, tok = var.ident in
    if Parse_info.is_origintok tok then
      ( env.config.is_source (G.Tk tok),
        env.config.is_sanitizer (G.Tk tok),
        env.config.is_sink (G.Tk tok) )
    else ([], [], [])
  in
  let taint_sources = source_pms |> T.taints_of_pms
  and taint_var_env =
    VarMap.find_opt (str_of_name var) env.var_env
    |> Option.value ~default:Taints.empty
  and taint_fun_env =
    (* TODO: Move this to check_tainted_instr ? *)
    Hashtbl.find_opt env.fun_env (str_of_name var)
    |> Option.value ~default:PM.Set.empty
    |> PM.Set.elements |> T.taints_of_pms
  in
  let taint : T.taints =
    taint_sources |> Taints.union taint_var_env |> Taints.union taint_fun_env
    (* |> PM.Set.union (is_tainted_function_hook config ((G.Id (var.ident, var.id_info)))) *)
  in
  match sanitizer_pms with
  (* TODO: We should check that taint and sanitizer(s) are unifiable. *)
  | _ :: _ -> Taints.empty
  | [] ->
      let sinks = sink_pms |> Common.map T.trace_of_pm in
      let findings = findings_of_tainted_sinks env taint sinks in
      report_findings env findings;
      taint

(* Test whether an expression is tainted, and if it is also a sink,
 * report the finding too (by side effect). *)
let rec check_tainted_expr env exp =
  let check = check_tainted_expr env in
  let check_base = function
    | Var var -> check_tainted_var env var
    | VarSpecial _ -> Taints.empty
    | Mem e -> check e
  in
  let check_offset = function
    | Index e -> check e
    | NoOffset
    | Dot _ ->
        Taints.empty
  in
  let check_subexpr exp =
    match exp.e with
    | Fetch { base = VarSpecial (This, _); offset = Dot fld; _ } ->
        (* TODO: Move this to check_tainted_instr ? *)
        Hashtbl.find_opt env.fun_env (str_of_name fld)
        |> Option.value ~default:PM.Set.empty
        |> PM.Set.elements |> T.taints_of_pms
    | Fetch { base; offset; _ } ->
        Taints.union (check_base base) (check_offset offset)
    | FixmeExp (_, _, Some e) -> check e
    | Literal _
    | FixmeExp (_, _, None) ->
        Taints.empty
    | Composite (_, (_, es, _))
    | Operator (_, es) ->
        union_map check es
    | Record fields -> union_map (fun (_, e) -> check e) fields
    | Cast (_, e) -> check e
  in
  let sanitizer_pms = orig_is_sanitized env.config exp.eorig in
  match sanitizer_pms with
  | _ :: _ ->
      (* TODO: We should check that taint and sanitizer(s) are unifiable. *)
      Taints.empty
  | [] ->
      let sinks =
        orig_is_sink env.config exp.eorig |> Common.map T.trace_of_pm
      in
      let taint_sources =
        orig_is_source env.config exp.eorig |> T.taints_of_pms
      in
      let taint = taint_sources |> Taints.union (check_subexpr exp) in
      let findings = findings_of_tainted_sinks env taint sinks in
      report_findings env findings;
      taint

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
let check_tainted_instr env instr : T.taints =
  let check_expr = check_tainted_expr env in
  let check_instr = function
    | Assign (_, e) -> check_expr e
    | AssignAnon _ -> Taints.empty (* TODO *)
    | Call (_, e, args) -> (
        let e_taints = check_expr e in
        let args_taints = Common.map check_expr args in
        match check_function_signature env e args_taints with
        | Some call_taints -> call_taints
        | None ->
            (* Default is to assume that the function will propagate
             * the taint of its arguments. *)
            List.fold_left Taints.union Taints.empty args_taints
            |> Taints.union e_taints)
    | CallSpecial (_, _, args) -> union_map check_expr args
    | FixmeInstr _ -> Taints.empty
  in
  let sanitizer_pms = orig_is_sanitized env.config instr.iorig in
  match sanitizer_pms with
  | _ :: _ ->
      (* TODO: We should check that taint and sanitizer(s) are unifiable. *)
      Taints.empty
  | [] ->
      let sinks =
        orig_is_sink env.config instr.iorig |> Common.map T.trace_of_pm
      in
      let taint_sources =
        orig_is_source env.config instr.iorig |> T.taints_of_pms
      in
      let taint = taint_sources |> Taints.union (check_instr instr.i) in
      let findings = findings_of_tainted_sinks env taint sinks in
      report_findings env findings;
      taint

(* Test whether a `return' is tainted, and if it is also a sink,
 * report the finding too (by side effect). *)
let check_tainted_return env tok e =
  let sinks =
    env.config.is_sink (G.Tk tok) @ orig_is_sink env.config e.eorig
    |> Common.map T.trace_of_pm
  in
  let taint = check_tainted_expr env e in
  let findings = findings_of_tainted_sinks env taint sinks in
  report_findings env findings;
  taint

(*****************************************************************************)
(* Transfer *)
(*****************************************************************************)

let union_env = Dataflow_core.varmap_union Taints.union

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
      | penv1 :: penvs -> List.fold_left union_env penv1 penvs)

let (transfer :
      config ->
      fun_env ->
      T.taints Dataflow_core.env ->
      string option ->
      flow:F.cfg ->
      T.taints Dataflow_core.transfn) =
 fun config fun_env enter_env opt_name ~flow
     (* the transfer function to update the mapping at node index ni *)
       mapping ni ->
  (* DataflowX.display_mapping flow mapping show_tainted; *)
  let in' : T.taints VarMap.t = input_env ~enter_env ~flow mapping ni in
  let node = flow.graph#nodes#assoc ni in
  let out' : T.taints VarMap.t =
    let env = { config; fun_name = opt_name; fun_env; var_env = in' } in
    match node.F.n with
    | NInstr x -> (
        let taint = check_tainted_instr env x in
        match (Taints.is_empty taint, LV.lvar_of_instr_opt x) with
        | true, Some var -> VarMap.remove (str_of_name var) in'
        | false, Some var ->
            let taint =
              let var_tok = snd var.ident in
              if Parse_info.is_fake var_tok then taint
              else
                taint
                |> Taints.map (fun t -> { t with tokens = var_tok :: t.tokens })
            in
            VarMap.update (str_of_name var)
              (function
                | None -> Some taint
                (* THINK: Why can't we just replace the existing taint? *)
                | Some taint' -> Some (Taints.union taint taint'))
              in'
        | _, None -> in')
    | NReturn (tok, e) -> (
        (* TODO: Move most of this to check_tainted_return. *)
        let taints = check_tainted_return env tok e in
        let findings = findings_of_tainted_return taints tok in
        report_findings env findings;
        let pmatches =
          taints |> Taints.elements
          |> List.filter_map (fun (taint : T.taint) ->
                 match taint.T.orig with
                 | T.Src src -> Some (T.pm_of_trace src)
                 | T.Arg _ -> None)
          |> PM.Set.of_list
        in
        match opt_name with
        | Some var ->
            (let str = var in
             match Hashtbl.find_opt fun_env str with
             | None ->
                 if not (PM.Set.is_empty pmatches) then
                   Hashtbl.add fun_env str pmatches
             | Some tained' ->
                 Hashtbl.replace fun_env str (PM.Set.union pmatches tained'));
            in'
        | None -> in')
    | _ -> in'
  in
  { D.in_env = in'; out_env = out' }

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let (fixpoint :
      ?in_env:T.taints Dataflow_core.VarMap.t ->
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
