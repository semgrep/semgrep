(* Iago Abal
 *
 * Copyright (C) 2024-2025 Semgrep Inc.
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
module ILH = IL_helpers

(*****************************************************************************)
(* Environment *)
(*****************************************************************************)

type env = {
  root : IL.lambdas_cfgs;
      (** Lambda declarations in the main function under analysis. *)
  in_lambda : (IL.name * IL.lambdas_cfgs) list;
      (** Lambda scopes, stack, most recent at the top. *)
  used_somewhere : IL.NameSet.t;
      (** Lambdas that we know are used somewhere.

      If a lambda is *not* use anywhere, then we instantiate it already at
      declaration site, so it's useful to have this pre-computed.. *)
  needed_vars : IL.NameSet.t;
      (** Vars that we need to track in the current function/lambda under analysis,
      other vars can be filtered out, see 'Dataflow_tainting.fixpoint_lambda' as
      well as 'live_vars_needed_for_taint'. *)
}

(*****************************************************************************)
(* Pre-analysis to find used lambdas *)
(*****************************************************************************)

let rec lambdas_used_in_cfg ?(lambdas = IL.NameMap.empty) (fun_cfg : IL.fun_cfg)
    =
  let union_all seq = seq |> Seq.fold_left IL.NameSet.union IL.NameSet.empty in
  let lambdas =
    (* A previously declared lambda may be used inside another lambda, so we
      need union the lambda "environments". There should be no name clashing, but
      if there was, we would keep the most recent declaration. *)
    IL.NameMap.union
      (fun _lname _lambda1 lambda2 -> Some lambda2)
      lambdas fun_cfg.lambdas
  in
  let lambdas_in_node node =
    ILH.rlvals_of_node node.IL.n
    |> List.to_seq
    |> Seq.filter_map (ILH.lval_is_lambda lambdas)
    |> Seq.map (fun (lname, _) -> lname)
    |> IL.NameSet.of_seq
  in
  let used_in_main =
    fun_cfg.cfg |> CFG.reachable_nodes |> Seq.map lambdas_in_node |> union_all
  in
  let used_in_lambdas =
    fun_cfg.lambdas |> IL.NameMap.to_seq
    |> Seq.map (fun (lname, lcfg) ->
           (* Take into account recursive definitions:

                  foo = (...) => ... foo(...) ...

            *)
           lambdas_used_in_cfg ~lambdas lcfg |> IL.NameSet.remove lname)
    |> union_all
  in
  used_in_main |> IL.NameSet.union used_in_lambdas

(*****************************************************************************)
(* Pre-analysis to find vars to track across lambdas *)
(*****************************************************************************)

let visitor_for_find_used_vars =
  object (_self : 'self)
    inherit [_] IL.iter

    method! visit_Var env name =
      if Tok.is_origintok (snd name.ident) then env := IL.NameSet.add name !env;
      ()
  end

let findvars_used_outside_lamdas (fun_cfg : IL.fun_cfg) =
  let acc = ref IL.NameSet.empty in
  let visit_node node =
    match node.IL.n with
    | NInstr { i = AssignAnon (_, Lambda _); _ } -> ()
    | __else__ -> visitor_for_find_used_vars#visit_node acc node
  in
  fun_cfg.cfg |> CFG.reachable_nodes |> Seq.iter visit_node;
  !acc

let find_vars_used_in_multiple_lambdas (fun_cfg : IL.fun_cfg) =
  let count_acc = ref IL.NameMap.empty in
  let visit_node node =
    let used_acc = ref IL.NameSet.empty in
    (match node.IL.n with
    | NInstr { i = AssignAnon (_, Lambda _); _ } ->
        visitor_for_find_used_vars#visit_node used_acc node
    | __else__ -> ());
    !used_acc
    |> IL.NameSet.iter (fun var ->
           let n = IL.NameMap.find_opt var !count_acc ||| 0 in
           count_acc := IL.NameMap.add var (n + 1) !count_acc)
  in
  fun_cfg.cfg |> CFG.reachable_nodes |> Seq.iter visit_node;
  !count_acc |> IL.NameMap.to_seq
  (* Keep variables used in more than in one lambda. *)
  |> Seq.filter (fun (_var, n) -> n > 1)
  |> Seq.map (fun (var, _n) -> var)
  |> IL.NameSet.of_seq

(* If we analyzed a lambda in 'fun_cfg', these would be the variables that
  we need to track outside that lambda, so we can discard other variables. *)
let find_vars_to_track_across_lambdas fun_cfg =
  findvars_used_outside_lamdas fun_cfg
  |> IL.NameSet.union (find_vars_used_in_multiple_lambdas fun_cfg)

(*****************************************************************************)
(* Ops and queries on the environment *)
(*****************************************************************************)

let new_env fcfg =
  let used_somewhere = lambdas_used_in_cfg fcfg in
  let needed_vars = find_vars_to_track_across_lambdas fcfg in
  { root = fcfg.lambdas; in_lambda = []; used_somewhere; needed_vars }

let top { root; in_lambda; _ } =
  match in_lambda with
  | (_name, lambdas) :: _ -> lambdas
  | [] -> root

let push { root; in_lambda; used_somewhere; needed_vars } lname fcfg =
  {
    root;
    in_lambda = (lname, fcfg.IL.lambdas) :: in_lambda;
    used_somewhere;
    needed_vars =
      needed_vars |> IL.NameSet.union (find_vars_to_track_across_lambdas fcfg);
  }

let find_lambda_cfg_in_current_scope env (lval : IL.lval) =
  match lval with
  | { base = Var name; rev_offset = [] } ->
      let* lambda_cfg = IL.NameMap.find_opt name (top env) in
      Some (name, lambda_cfg)
  | { base = Var _ | VarSpecial _ | Mem _; rev_offset = _ } ->
      (* Lambdas are only assigned to plain variables without any offset. *)
      None

let find { root; in_lambda; _ } name =
  let rec go_find scopes =
    match scopes with
    | [] ->
        (* Could not find in nested lambdas, try in the "root" scope *)
        IL.NameMap.find_opt name root
    | (name_i, lambdas_i) :: scopes -> (
        if IL.equal_name name name_i then
          (* We are looking for a lambda that is already under analysis,
            this is probably a recursive call, which we don't support. *)
          None
        else
          match IL.NameMap.find_opt name lambdas_i with
          | Some res -> Some res
          | None -> go_find scopes)
  in
  let* cfg = go_find in_lambda in
  Some (name, cfg)

let is_used_somewhere env name = IL.NameSet.mem name env.used_somewhere

(* Determine if a lambda ref/use should be instantiated. *)
let is_use_of_lambda env lval =
  match lval with
  | IL.{ base = Var name; rev_offset = [] } ->
      if is_used_somewhere env name then find env name else None
  | { base = Var _ | VarSpecial _ | Mem _; rev_offset = _ } ->
      (* Lambdas are only assigned to plain variables without any offset. *)
      None

let find_lambdas_used_in_node env node =
  ILH.rlvals_of_node node.IL.n |> List_.filter_map (is_use_of_lambda env)

let find_lambdas_to_analyze_in_node env node =
  let lambdas = top env in
  let unused_lambda_def =
    (* If the node declares a lambda, and this lambda is not used anywhere,
      we want to instantiate it here. *)
    let* instr =
      match node.IL.n with
      | NInstr i -> Some i
      | __else__ -> None
    in
    let* lval = ILH.lval_of_instr_opt instr in
    let* ((lname, _) as lambda) = ILH.lval_is_lambda lambdas lval in
    if is_used_somewhere env lname then None else Some lambda
  in
  Option.to_list unused_lambda_def @ find_lambdas_used_in_node env node

let live_vars_needed_for_taint env = env.needed_vars
