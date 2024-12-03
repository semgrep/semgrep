(* Iago Abal
 *
 * Copyright (C) 2024 r2c
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

(*****************************************************************************)
(* Vars to track across lambdas *)
(*****************************************************************************)

let used_vars_visitor =
  object (_self : 'self)
    inherit [_] IL.iter

    method! visit_Var env name =
      if Tok.is_origintok (snd name.ident) then env := IL.NameSet.add name !env;
      ()
  end

let vars_used_outside_lamdas (fun_cfg : IL.fun_cfg) =
  let acc = ref IL.NameSet.empty in
  let visit_node node =
    match node.IL.n with
    | NInstr { i = AssignAnon (_, Lambda _); _ } -> ()
    | __else__ -> used_vars_visitor#visit_node acc node
  in
  fun_cfg.cfg |> CFG.reachable_nodes |> Seq.iter visit_node;
  !acc

let vars_used_in_multiple_lambdas (fun_cfg : IL.fun_cfg) =
  let count_acc = ref IL.NameMap.empty in
  let visit_node node =
    let used_acc = ref IL.NameSet.empty in
    (match node.IL.n with
    | NInstr { i = AssignAnon (_, Lambda _); _ } ->
        used_vars_visitor#visit_node used_acc node
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

let find_vars_to_track_across_lambdas fun_cfg =
  vars_used_outside_lamdas fun_cfg
  |> IL.NameSet.union (vars_used_in_multiple_lambdas fun_cfg)
