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
module D = Dataflow
module VarMap = Dataflow.VarMap
module PM = Pattern_match

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Tainting dataflow analysis.
 *
 * This is a very rudimentary tainting analysis. Just intraprocedural,
 * very coarse grained (taint whole array/object).
 * This is step1 for taint tracking support in semgrep.
 * This was originally in semgrep-core/src/analyze, but it now depends on Pattern_match,
 * so it was moved to semgrep-core/src/engine
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type mapping = PM.Set.t Dataflow.mapping
(** Map for each node/var of all the pattern matches that originated its taint.
    Anything not included in the map is not tainted. Currently we only strictly need
    the metavariable environment in these pattern matches, but we plan to make use of
    the full pattern match information eventually.
*)

(* Tracks tainted functions. *)
type fun_env = (Dataflow.var, PM.Set.t) Hashtbl.t

(* is_source/sink/sanitizer returns a list of ways that some piece of code can be matched as a source/sink/sanitizer *)
type config = {
  is_source : G.any -> PM.t list;
  is_sink : G.any -> PM.t list;
  is_sanitizer : G.any -> PM.t list;
  found_tainted_sink : PM.Set.t -> PM.Set.t Dataflow.env -> unit;
}
(** This can use semgrep patterns under the hood. Note that a source can be an
  * instruction but also an expression. *)

module DataflowX = Dataflow.Make (struct
  type node = F.node

  type edge = F.edge

  type flow = (node, edge) CFG.t

  let short_string_of_node n = Display_IL.short_string_of_node_kind n.F.n
end)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let str_of_name name = spf "%s:%d" (fst name.ident) name.sid

let set_opt_to_set = function
  | None -> PM.Set.empty
  | Some x -> x

(* [unify_meta_envs env1 env1] returns [Some (union env1 env2)] if [env1] and [env2] contain no conflicting metavariable assignments, otherwise [None]. *)
let unify_meta_envs env1 env2 =
  let ( let* ) = Option.bind in
  let xs =
    List.fold_left
      (fun xs (mvar, mval) ->
        let* xs = xs in
        match List.assoc_opt mvar env2 with
        | None -> Some ((mvar, mval) :: xs)
        | Some mval' ->
            if Metavariable.equal_mvalue mval mval' then
              Some ((mvar, mval) :: xs)
            else None)
      (Some []) env1
  in
  let ys =
    List.filter (fun (mvar, _) -> not @@ List.mem_assoc mvar env1) env2
  in
  Option.map (fun xs -> xs @ ys) xs

let set_concat_map f xs =
  xs |> List.map f |> List.fold_left PM.Set.union PM.Set.empty

let set_filter_map f pm_set =
  PM.Set.fold
    (fun pm pm_set ->
      match f pm with
      | Some pm' -> PM.Set.add pm' pm_set
      | None -> pm_set)
    pm_set PM.Set.empty

(* @param sink_pm Pattern match of a sink.
   @param src_pms Set of pattern matches corresponding to sources.
   @returns PM.Set.t containing a copy [sink_pm] with an updated metavariable environment for each PM in [src_pms] whose env unifies with [sink_pm]s. *)
let update_meta_envs sink_pm src_pms =
  let ( let* ) = Option.bind in
  set_filter_map
    (fun src_pm ->
      let* env = unify_meta_envs sink_pm.PM.env src_pm.PM.env in
      Some { sink_pm with env })
    src_pms

(* @param sink_pms List of sink pattern matches.
   @param src_pms Set of source pattern matches.
   @returns PM.Set.t of all the possible tainted sink pattern matches with their metavariable environments updated
   to include the bindings from the source whose environment unified with it.
 *)
let make_tainted_sink_matches sink_pms src_pms =
  sink_pms
  |> List.map (fun pm -> update_meta_envs pm src_pms)
  |> List.fold_left PM.Set.union PM.Set.empty

(*****************************************************************************)
(* Tainted *)
(*****************************************************************************)

(* Test whether an expression is tainted, and if it is also a sink,
 * report the finding too (by side effect).
 *
 *)

let rec check_tainted_expr config (fun_env : fun_env) (env : PM.Set.t VarMap.t)
    exp =
  let check = check_tainted_expr config fun_env env in
  let sink_pms = config.is_sink (G.E exp.eorig) in
  let check_base = function
    | Var var ->
        let var_tok_pms =
          let _, tok = var.ident in
          if Parse_info.is_origintok tok then config.is_source (G.Tk tok)
          else []
        in
        PM.Set.of_list var_tok_pms
        |> PM.Set.union (set_opt_to_set (VarMap.find_opt (str_of_name var) env))
        |> PM.Set.union
             (set_opt_to_set (Hashtbl.find_opt fun_env (str_of_name var)))
    | VarSpecial _ -> PM.Set.empty
    | Mem e -> check e
  in
  let check_offset = function
    | Index e -> check e
    | NoOffset
    | Dot _ ->
        PM.Set.empty
  in
  let check_subexpr = function
    | Fetch { base = VarSpecial (This, _); offset = Dot fld; _ } ->
        set_opt_to_set (Hashtbl.find_opt fun_env (str_of_name fld))
    | Fetch { base; offset; _ } ->
        PM.Set.union (check_base base) (check_offset offset)
    | Literal _
    | FixmeExp _ ->
        PM.Set.empty
    | Composite (_, (_, es, _))
    | Operator (_, es) ->
        set_concat_map check es
    | Record fields -> set_concat_map (fun (_, e) -> check e) fields
    | Cast (_, e) -> check e
  in
  let sanitized_pms = config.is_sanitizer (G.E exp.eorig) in
  match sanitized_pms with
  | _ :: _ -> PM.Set.empty
  | [] ->
      let tainted_pms =
        PM.Set.union (check_subexpr exp.e)
          (PM.Set.of_list (config.is_source (G.E exp.eorig)))
      in
      let found = make_tainted_sink_matches sink_pms tainted_pms in
      if not (PM.Set.is_empty found) then config.found_tainted_sink found env;
      tainted_pms

(* Test whether an instruction is tainted, and if it is also a sink,
 * report the finding too (by side effect). *)
let check_tainted_instr config fun_env env instr : PM.Set.t =
  let sink_pms = config.is_sink (G.E instr.iorig) in
  let check_expr = check_tainted_expr config fun_env env in
  let tainted_args = function
    | Assign (_, e) -> check_expr e
    | AssignAnon _ -> PM.Set.empty (* TODO *)
    | Call (_, e, args) ->
        let e_tainted_pms = check_expr e in
        let args_tainted_pms = set_concat_map check_expr args in
        PM.Set.union e_tainted_pms args_tainted_pms
    | CallSpecial (_, _, args) -> set_concat_map check_expr args
    | FixmeInstr _ -> PM.Set.empty
  in
  let sanitized_pm_opt = config.is_sanitizer (G.E instr.iorig) in
  match sanitized_pm_opt with
  | _ :: _ -> PM.Set.empty
  | [] ->
      let tainted_pms =
        PM.Set.union (tainted_args instr.i)
          (PM.Set.of_list (config.is_source (G.E instr.iorig)))
      in
      let found = make_tainted_sink_matches sink_pms tainted_pms in
      if not (PM.Set.is_empty found) then config.found_tainted_sink found env;
      tainted_pms

(* Test whether a `return' is tainted, and if it is also a sink,
 * report the finding too (by side effect). *)
let check_tainted_return config fun_env env tok e =
  let orig_return = G.s (G.Return (tok, Some e.eorig, tok)) in
  let sink_pms = config.is_sink (G.S orig_return) in
  let e_tainted_pms = check_tainted_expr config fun_env env e in
  let found = make_tainted_sink_matches sink_pms e_tainted_pms in
  if not (PM.Set.is_empty found) then config.found_tainted_sink found env;
  e_tainted_pms

(*****************************************************************************)
(* Transfer *)
(*****************************************************************************)

let union = Dataflow.varmap_union PM.Set.union

let (transfer :
      config ->
      fun_env ->
      IL.name option ->
      flow:F.cfg ->
      PM.Set.t Dataflow.transfn) =
 fun config fun_env opt_name ~flow
     (* the transfer function to update the mapping at node index ni *)
       mapping ni ->
  let in' =
    (flow.graph#predecessors ni)#fold
      (fun acc (ni_pred, _) -> union acc mapping.(ni_pred).D.out_env)
      VarMap.empty
  in
  let node = flow.graph#nodes#assoc ni in
  let out' =
    match node.F.n with
    | NInstr x -> (
        let tainted = check_tainted_instr config fun_env in' x in
        match (PM.Set.is_empty tainted, IL.lvar_of_instr_opt x) with
        | true, Some var -> VarMap.remove (str_of_name var) in'
        | false, Some var ->
            VarMap.update (str_of_name var)
              (function
                | None -> Some tainted
                | Some tainted' -> Some (PM.Set.union tainted tainted'))
              in'
        | _, None -> in')
    | NReturn (tok, e) -> (
        let tainted = check_tainted_return config fun_env in' tok e in
        match opt_name with
        | Some var ->
            (let str = str_of_name var in
             match Hashtbl.find_opt fun_env str with
             | None -> Hashtbl.add fun_env str tainted
             | Some tained' ->
                 Hashtbl.replace fun_env str (PM.Set.union tainted tained'));
            in'
        | None -> in')
    | _ -> in'
  in
  { D.in_env = in'; out_env = out' }

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let (fixpoint : config -> fun_env -> IL.name option -> F.cfg -> mapping) =
 fun config fun_env opt_name flow ->
  DataflowX.fixpoint ~eq:PM.Set.equal
    ~init:(DataflowX.new_node_array flow (Dataflow.empty_inout ()))
    ~trans:
      (transfer config fun_env opt_name ~flow)
      (* tainting is a forward analysis! *)
    ~forward:true ~flow
