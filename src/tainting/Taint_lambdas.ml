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

type in_parent_scope = IL.NameSet.t
(** Lambda names visible in the parent scope. *)

type env = {
  root : Fun_CFG.lambdas_cfgs;
      (** Lambda declarations in the main function under analysis. *)
  in_lambda : (IL.name * Fun_CFG.lambdas_cfgs * in_parent_scope) list;
      (** Stack of lambda scopes, most recent at the top. *)
  used_somewhere : IL.NameSet.t;
      (** Lambdas that we know are used somewhere.

        If a lambda is *not* used anywhere, then we instantiate it already at
        declaration site, so it's useful to have this pre-computed.

        Recursive calls are not considered as uses here.
      *)
  needed_vars : IL.NameSet.t;
      (** Vars that we need to track in the current function/lambda under analysis,
      other vars can be filtered out, see 'Dataflow_tainting.fixpoint_lambda' as
      well as 'live_vars_needed_for_taint'. *)
}

(*****************************************************************************)
(* Pre-analysis to find used lambdas *)
(*****************************************************************************)

let rec lambdas_used_in_cfg ?(lambdas = Fun_CFG.empty_lambdas)
    (fun_cfg : Fun_CFG.t) =
  let union_all seq = seq |> Seq.fold_left IL.NameSet.union IL.NameSet.empty in
  let lambdas =
    (* A previously declared lambda may be used inside another lambda, so we
      need to union the lambda "environments". There should be no name clashing,
      but if there was, we would keep the most recent declaration. *)
    fun_cfg.lambdas |> Fun_CFG.union_lambdas ~base:lambdas
  in
  let lambdas_used_in_node node =
    ILH.rlvals_of_node node.IL.n
    |> List.to_seq
    |> Seq.filter_map (fun lval ->
           (* We only consider a lambda ref as an use if we can uniquely
              identify its corresponding lambda declaration. *)
           Fun_CFG.is_lambda lambdas lval |> Result.to_option)
    |> Seq.map (fun (lname, _) -> lname)
    |> IL.NameSet.of_seq
  in
  let used_in_main =
    fun_cfg.cfg |> CFG.reachable_nodes
    |> Seq.map lambdas_used_in_node
    |> union_all
  in
  let used_in_lambdas =
    fun_cfg.lambdas |> Fun_CFG.seq_of_lambdas
    |> Seq.map (fun (lname, _pos, lcfg) ->
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

let visitor_to_find_used_vars =
  object (_self : 'self)
    inherit [_] IL.iter

    method! visit_Var env name =
      if Tok.is_origintok (snd name.ident) then env := IL.NameSet.add name !env;
      ()
  end

let find_vars_used_outside_lamdas (fun_cfg : Fun_CFG.t) =
  let acc = ref IL.NameSet.empty in
  let visit_node node =
    match node.IL.n with
    | NNestedDef { name = EN name; _ }
      when Result.is_ok (Fun_CFG.find_lambda fun_cfg.lambdas name) ->
        (* don't look inside lambdas *) ()
    | __else__ -> visitor_to_find_used_vars#visit_node acc node
  in
  fun_cfg.cfg |> CFG.reachable_nodes |> Seq.iter visit_node;
  !acc

let find_vars_used_in_multiple_lambdas (fun_cfg : Fun_CFG.t) =
  let count_acc = ref IL.NameMap.empty in
  let visit_node node =
    let used_acc = ref IL.NameSet.empty in
    (match node.IL.n with
    | NNestedDef { name = EN name; _ } -> (
        match Fun_CFG.find_lambda fun_cfg.lambdas name with
        | Ok fcfg ->
            fcfg.fdef
            |> Option.iter (fun fdef ->
                   visitor_to_find_used_vars#visit_function_definition used_acc
                     fdef)
        | Error _ -> ())
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
  find_vars_used_outside_lamdas fun_cfg
  |> IL.NameSet.union (find_vars_used_in_multiple_lambdas fun_cfg)

(*****************************************************************************)
(* Ops and queries on the environment *)
(*****************************************************************************)

let new_env fcfg =
  let used_somewhere = lambdas_used_in_cfg fcfg in
  let needed_vars = find_vars_to_track_across_lambdas fcfg in
  { root = fcfg.lambdas; in_lambda = []; used_somewhere; needed_vars }

let current { root; in_lambda; _ } =
  match in_lambda with
  | (_name, lambdas, _in_parent) :: _ -> lambdas
  | [] -> root

let in_scope { root; in_lambda; _ } =
  match in_lambda with
  | (_name, lambdas, in_parent) :: _ ->
      Fun_CFG.lambdas_names lambdas |> IL.NameSet.union in_parent
  | [] -> Fun_CFG.lambdas_names root

let push ({ root; in_lambda; used_somewhere; needed_vars } as env) lname
    (fcfg : Fun_CFG.t) =
  {
    root;
    in_lambda = (lname, fcfg.lambdas, in_scope env) :: in_lambda;
    used_somewhere;
    needed_vars =
      needed_vars |> IL.NameSet.union (find_vars_to_track_across_lambdas fcfg);
  }

let find_lambda_cfg_in_current_scope env (lval : IL.lval) =
  match lval with
  | { base = Var name; rev_offset = [] } ->
      let* lambda_cfg =
        Fun_CFG.find_lambda (current env) name |> Result.to_option
      in
      Some (name, lambda_cfg)
  | { base = Var _ | VarSpecial _ | Mem _; rev_offset = _ } ->
      (* Lambdas are only assigned to plain variables without any offset. *)
      None

let find { root; in_lambda; _ } name =
  let rec go_find scopes =
    match scopes with
    | [] ->
        (* Could not find in nested lambdas, try in the "root" scope *)
        Fun_CFG.find_lambda root name |> Result.to_option
    | (name_i, lambdas_i, in_parent) :: scopes -> (
        if IL.equal_name name name_i then
          (* We are looking for a lambda that is already under analysis,
            this is probably a recursive call, which we don't support. *)
          None
        else
          match Fun_CFG.find_lambda lambdas_i name with
          | Ok res when not (IL.NameSet.mem name in_parent) (* unique *) ->
              Some res
          | Ok _ (* not unique *)
          | Error `Multi ->
              (*
              E.g.

                  foo = () => ...;
                  foo = () => ...;

              or

                  foo = () => ...;
                  bar = function () {
                    foo = () => ...;
                    ...
                  };
            *)
              None
          | Error `NotLambda ->
              (*
              E.g. in the body of lambda `bar` below we have a call to `fn`,
              but to find the declaration of `fn` we need to look into the
              outer scope.

                  const fn = () => sink(source());
                  bar = function () { fn(baz) };
            *)
              go_find scopes)
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
  ILH.rlvals_of_node node.IL.n |> List.filter_map (is_use_of_lambda env)

let check_if_node_defines_unused_lambda env ~lambdas node =
  match node.IL.n with
  | NNestedDef { name = EN lname; _ } ->
      let* lambda_cfg = Fun_CFG.find_lambda lambdas lname |> Result.to_option in
      if is_used_somewhere env lname then None else Some (lname, lambda_cfg)
  | NNestedDef { name = FixmeEntity _; _ }
  | NInstr _
  | NCond _
  | NGoto _
  | NReturn _
  | NThrow _
  | NMatch _
  | NCase _
  | NOther _
  | NTodo _
  | Enter
  | Exit
  | Join
  | TrueNode _
  | FalseNode _ ->
      None

let find_lambdas_to_analyze_in_node env node =
  let lambdas = current env in
  let unused_lambda_def =
    (* If the node declares a lambda, and this lambda is not used anywhere,
      we want to instantiate it here. *)
    check_if_node_defines_unused_lambda env ~lambdas node
  in
  Option.to_list unused_lambda_def @ find_lambdas_used_in_node env node

let live_vars_needed_for_taint env = env.needed_vars
