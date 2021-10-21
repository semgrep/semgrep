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
(** Map for each node/var whether a variable is "tainted" *)

(* Tracks tainted functions. *)
type fun_env = (Dataflow.var, PM.Set.t) Hashtbl.t

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

let str_of_name ((s, _tok), sid) = spf "%s:%d" s sid

let set_opt_to_set = function
  | None -> PM.Set.empty
  | Some x -> x

let ( <::> ) x = Option.map (fun xs -> x :: xs)

let unify_meta_envs env1 env2 =
  let xs =
    List.fold_left
      (fun xs (mvar, mval) ->
        match List.assoc_opt mvar env2 with
        | None -> (mvar, mval) <::> xs
        | Some mval' ->
            if Metavariable.equal_mvalue mval mval' then (mvar, mval) <::> xs
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

let ( <|> ) = PM.Set.union

let update_meta_envs pm =
  set_filter_map (fun pm' ->
      Option.bind (unify_meta_envs pm.PM.env pm'.PM.env) (fun env ->
          Some { pm with env }))

(* Takes a list of pattern matches from is_sink and a set of pattern matches
   indicating a variable is tainted, and tries to unify the metavariable
   environment in each sink match with all the tainted matches. If the envs
   unify, the resulting set will contain the sink pattern match with an updated
   metavariable env. The resulting sets are all unioned together
*)
let update_tainted_sinks pm_list pm_set =
  pm_list
  |> List.map (fun pm -> update_meta_envs pm pm_set)
  |> List.fold_left PM.Set.union PM.Set.empty

let varmap_update env x data f =
  match VarMap.find_opt x env with
  | None -> VarMap.add x data env
  | Some data' -> VarMap.add x (f data') env

let hashtbl_update env x data f =
  match Hashtbl.find_opt env x with
  | None -> Hashtbl.add env x data
  | Some data' -> Hashtbl.add env x (f data')

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
          let (_, tok), _ = var in
          if Parse_info.is_origintok tok then config.is_source (G.Tk tok)
          else []
        in
        PM.Set.of_list var_tok_pms
        <|> set_opt_to_set (VarMap.find_opt (str_of_name var) env)
        <|> set_opt_to_set (Hashtbl.find_opt fun_env (str_of_name var))
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
    | Fetch { base; offset; _ } -> check_base base <|> check_offset offset
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
        check_subexpr exp.e
        <|> PM.Set.of_list (config.is_source (G.E exp.eorig))
      in
      let found = update_tainted_sinks sink_pms tainted_pms in
      if PM.Set.is_empty found then tainted_pms
      else (
        config.found_tainted_sink found env;
        tainted_pms)

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
        e_tainted_pms <|> args_tainted_pms
    | CallSpecial (_, _, args) -> set_concat_map check_expr args
    | FixmeInstr _ -> PM.Set.empty
  in
  let sanitized_pm_opt = config.is_sanitizer (G.E instr.iorig) in
  match sanitized_pm_opt with
  | _ :: _ -> PM.Set.empty
  | [] ->
      let tainted_pms =
        tainted_args instr.i
        <|> PM.Set.of_list (config.is_source (G.E instr.iorig))
      in
      let found = update_tainted_sinks sink_pms tainted_pms in
      if PM.Set.is_empty found then tainted_pms
      else (
        config.found_tainted_sink found env;
        tainted_pms)

(* Test whether a `return' is tainted, and if it is also a sink,
 * report the finding too (by side effect). *)
let check_tainted_return config fun_env env tok e =
  let orig_return = G.s (G.Return (tok, Some e.eorig, tok)) in
  let sink_pms = config.is_sink (G.S orig_return) in
  let e_tainted_pms = check_tainted_expr config fun_env env e in
  let found = update_tainted_sinks sink_pms e_tainted_pms in
  if PM.Set.is_empty found then e_tainted_pms
  else (
    config.found_tainted_sink found env;
    e_tainted_pms)

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
        | _, Some var ->
            varmap_update in' (str_of_name var) tainted (PM.Set.union tainted)
        | _ -> in')
    | NReturn (tok, e) -> (
        let tainted = check_tainted_return config fun_env in' tok e in
        match opt_name with
        | Some var ->
            hashtbl_update fun_env (str_of_name var) tainted
              (PM.Set.union tainted);
            in'
        | _ -> in')
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
