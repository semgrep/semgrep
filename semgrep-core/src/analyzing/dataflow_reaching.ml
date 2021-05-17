(* Iain Proctor, Yoann Padioleau, Jiao Li
 *
 * Copyright (C) 2009-2010 Facebook
 * Copyright (C) 2019 r2c
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
module F = Controlflow
module D = Dataflow
module V = Controlflow_visitor
module NodeiSet = Dataflow.NodeiSet
module VarMap = Dataflow.VarMap
module VarSet = Dataflow.VarSet

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Reaching definitions dataflow analysis.
 *
 * A definition will "reach" another program point if there is no
 * intermediate assignment between this definition and this program point.
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* For a reaching definitions analysis, the dataflow result is
 * a map from each program point (as usual), to a map from each
 * variable (as usual), to a set of nodes that define this variable
 * that are visible at this program point.
 *
 * For instance on:
 *
 * 1: $a = 1;
 * 2: if(...) {
 * 3:   $a = 2;
 * 4: } else {
 * 5:   $a = 3;
 * 6: }
 * 7: echo $a;
 *
 * then at the program point (node index) 7, then for $a the nodei set
 * is {3, 5}, but not '1'.
 *)
type mapping = Dataflow.NodeiSet.t Dataflow.mapping

(*****************************************************************************)
(* Gen/Kill *)
(*****************************************************************************)

let (defs : F.flow -> NodeiSet.t Dataflow.env) =
 fun flow ->
  (* the long version, could use F.fold_on_expr *)
  flow#nodes#fold
    (fun env (ni, node) ->
      let xs = V.exprs_of_node node in
      xs
      |> List.fold_left
           (fun env e ->
             let lvals = Lrvalue.lvalues_of_expr e in
             let vars = lvals |> List.map (fun ((s, _tok), _idinfo) -> s) in
             vars
             |> List.fold_left
                  (fun env var -> Dataflow.add_var_and_nodei_to_env var ni env)
                  env)
           env)
    VarMap.empty

module DataflowX = Dataflow.Make (struct
  type node = F.node

  type edge = F.edge

  type flow = (node, edge) Ograph_extended.ograph_mutable

  let short_string_of_node = F.short_string_of_node
end)

let (gens : F.flow -> VarSet.t array) =
 fun flow ->
  let arr = DataflowX.new_node_array flow VarSet.empty in
  V.fold_on_node_and_expr
    (fun (ni, _nd) e arr ->
      let lvals = Lrvalue.lvalues_of_expr e in
      let vars = lvals |> List.map (fun ((s, _tok), _idinfo) -> s) in
      vars |> List.iter (fun var -> arr.(ni) <- VarSet.add var arr.(ni));
      arr)
    flow arr

let (kills : NodeiSet.t Dataflow.env -> F.flow -> NodeiSet.t Dataflow.env array)
    =
 fun defs flow ->
  let arr = DataflowX.new_node_array flow (Dataflow.empty_env ()) in
  V.fold_on_node_and_expr
    (fun (ni, _nd) e () ->
      let lvals = Lrvalue.lvalues_of_expr e in
      let vars = lvals |> List.map (fun ((s, _tok), _idinfo) -> s) in
      vars
      |> List.iter (fun var ->
             let set = NodeiSet.remove ni (VarMap.find var defs) in
             arr.(ni) <- VarMap.add var set arr.(ni)))
    flow ();
  arr

(*****************************************************************************)
(* Transfer *)
(*****************************************************************************)

let union = Dataflow.union_env

let diff = Dataflow.diff_env

(*
 * This algorithm is taken from Modern Compiler Implementation in ML, Appel,
 * 1998, pp. 382.
 *
 * The transfer is setting:
 *  - in'[n]  = U_{p in pred[n]} out[p]
 *  - out'[n] = gen[n] U (in[n] - kill[n])
 *)
let (transfer :
      gen:VarSet.t array ->
      kill:NodeiSet.t Dataflow.env array ->
      flow:F.flow ->
      NodeiSet.t Dataflow.transfn) =
 fun ~gen ~kill ~flow
     (* the transfer function to update the mapping at node index ni *)
       mapping ni ->
  let in' =
    (flow#predecessors ni)#fold
      (fun acc (ni_pred, _) -> union acc mapping.(ni_pred).D.out_env)
      VarMap.empty
  in
  let in_minus_kill = diff in' kill.(ni) in
  let out' = Dataflow.add_vars_and_nodei_to_env gen.(ni) ni in_minus_kill in
  { D.in_env = in'; out_env = out' }

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let (fixpoint : F.flow -> mapping) =
 fun flow ->
  let gen = gens flow in
  let kill = kills (defs flow) flow in

  DataflowX.fixpoint ~eq:NodeiSet.equal
    ~init:(DataflowX.new_node_array flow (Dataflow.empty_inout ()))
    ~trans:(transfer ~gen ~kill ~flow)
    ~forward:true ~flow
