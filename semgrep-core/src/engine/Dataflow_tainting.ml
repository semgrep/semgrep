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

(* TODO: Given that the analysis is path-insensitive, the trace should capture
 * all potential paths. So a set of tokens seems more appropriate than a list.
 *)
type trace = G.tok list [@@deriving show]

type deep_match = PM of Pattern_match.t | Call of G.expr * trace * deep_match
[@@deriving show]

type source = deep_match [@@deriving show]
type sink = deep_match [@@deriving show]
type arg_pos = int [@@deriving show]

type source_to_sink = {
  source : source;
  trace : trace;
  sink : sink;
  merged_env : Metavariable.bindings;
}
[@@deriving show]

type finding =
  | SrcToSink of source_to_sink
  | SrcToReturn of source * trace * tok
  | ArgToSink of arg_pos * trace * sink
  | ArgToReturn of arg_pos * trace * tok
[@@deriving show]

type taint_orig = Src of source | Arg of arg_pos [@@deriving show]
type taint = { orig : taint_orig; rev_trace : trace } [@@deriving show]

(* We use a set simply to avoid duplicate findings.
 * THINK: Should we just let them pass here and be filtered out later on? *)
module Taint = Set.Make (struct
  type t = taint

  let compare_pm pm1 pm2 =
    (* If the pattern matches are obviously different (have different ranges),
     * we are done. If their ranges are the same, we compare their metavariable
     * environments. This is not robust to reordering metavariable environments,
     * e.g.: [("$A",e1);("$B",e2)] is not equal to [("$B",e2);("$A",e1)]. This
     * is a potential source of duplicate findings, but that is OK.
     *)
    match compare pm1.PM.range_loc pm2.PM.range_loc with
    | 0 -> compare pm1.PM.env pm2.PM.env
    | c -> c

  let rec compare_dm dm1 dm2 =
    match (dm1, dm2) with
    | PM p, PM q -> compare_pm p q
    | PM _, Call _ -> -1
    | Call _, PM _ -> 1
    | Call (c1, _t1, d1), Call (c2, _t2, d2) ->
        let c_cmp = Int.compare c1.e_id c2.e_id in
        if c_cmp <> 0 then c_cmp else compare_dm d1 d2

  (* TODO: Rely on ppx_deriving.ord ? *)
  let compare_orig t1 t2 =
    match (t1, t2) with
    | Arg i, Arg j -> Int.compare i j
    | Src p, Src q -> compare_dm p q
    | Arg _, Src _ -> -1
    | Src _, Arg _ -> 1

  (* TODO: Right now we disregard the trace so we only keep one potential path.
   *       This may have to become a map (orig -> trace) rather than a set, so
   *       that we can merge the traces when merging taint at join points. *)
  let compare t1 t2 = compare_orig t1.orig t2.orig
end)

type config = {
  filepath : Common.filename;
  rule_id : string;
  is_source : G.any -> PM.t list;
  is_sink : G.any -> PM.t list;
  is_sanitizer : G.any -> PM.t list;
  unify_mvars : bool;
  handle_findings :
    var option -> finding list -> Taint.t Dataflow_core.env -> unit;
}

type mapping = Taint.t Dataflow_core.mapping

(* HACK: Tracks tainted functions intrafile. *)
type fun_env = (var, PM.Set.t) Hashtbl.t

type env = {
  config : config;
  fun_name : var option;
  fun_env : fun_env;
  var_env : Taint.t VarMap.t;
}

(*****************************************************************************)
(* Hooks *)
(*****************************************************************************)

let hook_function_taint_signature = ref None

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let rec pm_of_dm = function
  | PM pm -> pm
  | Call (_, _, dm) -> pm_of_dm dm

let dm_of_pm pm = PM pm
let src_of_pm pm = Src (PM pm)
let taint_of_pm pm = { orig = src_of_pm pm; rev_trace = [] }
let taint_of_pms pms = pms |> Common.map taint_of_pm |> Taint.of_list

(* Debug *)
let show_taint_set taint =
  taint |> Taint.elements |> Common.map show_taint |> String.concat ", "
  |> fun str -> "{ " ^ str ^ " }"

(* Debug *)
let _show_env =
  let (env_to_str : ('a -> string) -> 'a VarMap.t -> string) =
   fun val2str env ->
    VarMap.fold (fun dn v s -> s ^ dn ^ ":" ^ val2str v ^ " ") env ""
  in
  env_to_str show_taint_set

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
  xs |> Common.map f |> List.fold_left Taint.union Taint.empty

(* Produces a finding for every taint source that is unifiable with the sink. *)
let findings_of_tainted_sink env (taint : Taint.t) (sink : sink) : finding list
    =
  let ( let* ) = Option.bind in
  taint |> Taint.elements
  |> List.filter_map (fun taint ->
         let trace = List.rev taint.rev_trace in
         match taint.orig with
         | Arg i ->
             (* We need to check unifiability at the call site. *)
             Some (ArgToSink (i, trace, sink))
         | Src source ->
             let src_pm = pm_of_dm source in
             let sink_pm = pm_of_dm sink in
             let* merged_env =
               merge_source_sink_mvars env sink_pm.PM.env src_pm.PM.env
             in
             Some (SrcToSink { source; trace; sink; merged_env }))

(* Produces a finding for every unifiable source-sink pair. *)
let findings_of_tainted_sinks env (taint : Taint.t) (sinks : sink list) :
    finding list =
  sinks |> List.concat_map (findings_of_tainted_sink env taint)

let findings_of_tainted_return (taint : Taint.t) return_tok : finding list =
  taint |> Taint.elements
  |> Common.map (fun taint ->
         let trace = List.rev taint.rev_trace in
         match taint.orig with
         | Arg i -> ArgToReturn (i, trace, return_tok)
         | Src src -> SrcToReturn (src, trace, return_tok))

(*****************************************************************************)
(* Tainted *)
(*****************************************************************************)

(* Test whether a variable occurrence is tainted, and if it is also a sink,
 * report the finding too (by side effect). *)
let check_tainted_var env (var : IL.name) : Taint.t =
  let source_pms, sanitizer_pms, sink_pms =
    let _, tok = var.ident in
    if Parse_info.is_origintok tok then
      ( env.config.is_source (G.Tk tok),
        env.config.is_sanitizer (G.Tk tok),
        env.config.is_sink (G.Tk tok) )
    else ([], [], [])
  in
  let taint_sources = source_pms |> taint_of_pms
  and taint_var_env =
    VarMap.find_opt (str_of_name var) env.var_env
    |> Option.value ~default:Taint.empty
  and taint_fun_env =
    (* TODO: Move this to check_tainted_instr ? *)
    Hashtbl.find_opt env.fun_env (str_of_name var)
    |> Option.value ~default:PM.Set.empty
    |> PM.Set.elements |> taint_of_pms
  in
  let taint : Taint.t =
    taint_sources |> Taint.union taint_var_env |> Taint.union taint_fun_env
    (* |> PM.Set.union (is_tainted_function_hook config ((G.Id (var.ident, var.id_info)))) *)
  in
  match sanitizer_pms with
  (* TODO: We should check that taint and sanitizer(s) are unifiable. *)
  | _ :: _ -> Taint.empty
  | [] ->
      let sinks = sink_pms |> Common.map dm_of_pm in
      let findings = findings_of_tainted_sinks env taint sinks in
      report_findings env findings;
      taint

(* Test whether an expression is tainted, and if it is also a sink,
 * report the finding too (by side effect). *)
let rec check_tainted_expr env exp =
  let check = check_tainted_expr env in
  let check_base = function
    | Var var -> check_tainted_var env var
    | VarSpecial _ -> Taint.empty
    | Mem e -> check e
  in
  let check_offset = function
    | Index e -> check e
    | NoOffset
    | Dot _ ->
        Taint.empty
  in
  let check_subexpr exp =
    match exp.e with
    | Fetch { base = VarSpecial (This, _); offset = Dot fld; _ } ->
        (* TODO: Move this to check_tainted_instr ? *)
        Hashtbl.find_opt env.fun_env (str_of_name fld)
        |> Option.value ~default:PM.Set.empty
        |> PM.Set.elements |> taint_of_pms
    | Fetch { base; offset; _ } ->
        Taint.union (check_base base) (check_offset offset)
    | FixmeExp (_, _, Some e) -> check e
    | Literal _
    | FixmeExp (_, _, None) ->
        Taint.empty
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
      Taint.empty
  | [] ->
      let sinks = orig_is_sink env.config exp.eorig |> Common.map dm_of_pm in
      let taint_sources = orig_is_source env.config exp.eorig |> taint_of_pms in
      let taint = taint_sources |> Taint.union (check_subexpr exp) in
      let findings = findings_of_tainted_sinks env taint sinks in
      report_findings env findings;
      taint

let check_function_signature env fun_exp args_taint =
  let taint_of_arg i =
    let taint_opt = List.nth_opt args_taint i in
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
             | SrcToReturn (dm, trace, _return_tok) ->
                 let dm = Call (eorig, trace, dm) in
                 Some (Taint.singleton { orig = Src dm; rev_trace = [] })
             | ArgToReturn (i, trace, _return_tok) ->
                 let* arg_taint = taint_of_arg i in
                 Some
                   (arg_taint
                   |> Taint.map (fun taint ->
                          let rev_trace =
                            List.rev_append trace (snd ident :: taint.rev_trace)
                          in
                          { taint with rev_trace }))
             | ArgToSink (i, trace, sink) ->
                 let sink = Call (eorig, trace, sink) in
                 let* arg_taint = taint_of_arg i in
                 arg_taint
                 |> Taint.iter (fun t ->
                        findings_of_tainted_sink env (Taint.singleton t) sink
                        |> report_findings env);
                 None
             (* THINK: Should we report something here? *)
             | SrcToSink _ -> None)
        |> List.fold_left Taint.union Taint.empty)
  | None, _
  | Some _, _ ->
      None

(* Test whether an instruction is tainted, and if it is also a sink,
 * report the finding too (by side effect). *)
(* TODO: This should return a new var_env rather than just taint, it
 * makes more sense given that an instruction may have side-effects.
 * It Also makes simpler to handle sanitization by side-effect. *)
let check_tainted_instr env instr : Taint.t =
  let check_expr = check_tainted_expr env in
  let check_instr = function
    | Assign (_, e) -> check_expr e
    | AssignAnon _ -> Taint.empty (* TODO *)
    | Call (_, e, args) -> (
        let e_taint = check_expr e in
        let args_taint = Common.map check_expr args in
        match check_function_signature env e args_taint with
        | Some call_taint -> call_taint
        | None ->
            (* Default is to assume that the function will propagate
             * the taint of its arguments. *)
            List.fold_left Taint.union Taint.empty args_taint
            |> Taint.union e_taint)
    | CallSpecial (_, _, args) -> union_map check_expr args
    | FixmeInstr _ -> Taint.empty
  in
  let sanitizer_pms = orig_is_sanitized env.config instr.iorig in
  match sanitizer_pms with
  | _ :: _ ->
      (* TODO: We should check that taint and sanitizer(s) are unifiable. *)
      Taint.empty
  | [] ->
      let sinks = orig_is_sink env.config instr.iorig |> Common.map dm_of_pm in
      let taint_sources =
        orig_is_source env.config instr.iorig |> taint_of_pms
      in
      let taint = taint_sources |> Taint.union (check_instr instr.i) in
      let findings = findings_of_tainted_sinks env taint sinks in
      report_findings env findings;
      taint

(* Test whether a `return' is tainted, and if it is also a sink,
 * report the finding too (by side effect). *)
let check_tainted_return env tok e =
  let sinks =
    env.config.is_sink (G.Tk tok) @ orig_is_sink env.config e.eorig
    |> Common.map dm_of_pm
  in
  let taint = check_tainted_expr env e in
  let findings = findings_of_tainted_sinks env taint sinks in
  report_findings env findings;
  taint

(*****************************************************************************)
(* Transfer *)
(*****************************************************************************)

let union_env = Dataflow_core.varmap_union Taint.union

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

let add_tainted_var in_ var taint =
  let taint =
    let var_tok = snd var.ident in
    if Parse_info.is_fake var_tok then taint
    else
      taint
      |> Taint.map (fun t -> { t with rev_trace = var_tok :: t.rev_trace })
  in
  VarMap.update (str_of_name var)
    (function
      | None -> Some taint
      (* THINK: Why can't we just replace the existing taint? *)
      | Some taint' -> Some (Taint.union taint taint'))
    in_

let (transfer :
      config ->
      fun_env ->
      Taint.t Dataflow_core.env ->
      string option ->
      flow:F.cfg ->
      Taint.t Dataflow_core.transfn) =
 fun config fun_env enter_env opt_name ~flow
     (* the transfer function to update the mapping at node index ni *)
       mapping ni ->
  (* DataflowX.display_mapping flow mapping show_tainted; *)
  let in' : Taint.t VarMap.t = input_env ~enter_env ~flow mapping ni in
  let node = flow.graph#nodes#assoc ni in
  let out' : Taint.t VarMap.t =
    let env = { config; fun_name = opt_name; fun_env; var_env = in' } in
    match node.F.n with
    | NInstr x -> (
        let taint = check_tainted_instr env x in
        match (Taint.is_empty taint, LV.lvar_of_instr_opt x) with
        | true, Some var -> VarMap.remove (str_of_name var) in'
        | false, Some var -> add_tainted_var in' var taint
        | false, None -> (
            match x.i with
            (* Method call `var.f(args)` that returns void, we conservatively
               * assume that it may be updating `var`; e.g. we may be updating
               * a string buffer. So if `args` are tainted, so it is `var`. *)
            | Call
                ( None,
                  { e = Fetch { base = Var var; offset = Dot _fld; _ }; _ },
                  _args ) ->
                add_tainted_var in' var taint
            | _ -> in')
        | true, None -> in')
    | NReturn (tok, e) -> (
        (* TODO: Move most of this to check_tainted_return. *)
        let taint = check_tainted_return env tok e in
        let findings = findings_of_tainted_return taint tok in
        report_findings env findings;
        let pmatches =
          taint |> Taint.elements
          |> List.filter_map (fun taint ->
                 match taint.orig with
                 | Src src -> Some (pm_of_dm src)
                 | Arg _ -> None)
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
      ?in_env:Taint.t Dataflow_core.VarMap.t ->
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
  DataflowX.fixpoint ~eq:Taint.equal ~init:init_mapping
    ~trans:(transfer config fun_env enter_env opt_name ~flow)
      (* tainting is a forward analysis! *)
    ~forward:true ~flow
