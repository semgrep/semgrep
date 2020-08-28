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

module VarMap = Dataflow.VarMap

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Liveness dataflow analysis.
 *
 * A variable is *live* if it holds a value that may be needed in the future.
 * So a variable is "dead" if it holds a value that is not used later.
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* For a liveness analysis, the dataflow result is
 * a map from each program point (as usual), to a map from each
 * variable (as usual), to whether the variable is live at that point.
 *
 * For instance on:
 *
 * 1: a = 1;
 *  : do {
 * 2:   b = a + 1;
 * 3:   c = c + b;
 * 4:   a = b * 2;
 * 5: } while (a < N)
 * 6: return c
 *
 * TODO?? what is live?
 *)
type mapping = unit Dataflow.mapping

module DataflowX = Dataflow.Make (struct
  type node = F.node
  type edge = F.edge
  type flow = (node, edge) Ograph_extended.ograph_mutable
  let short_string_of_node = F.short_string_of_node
end)

(*****************************************************************************)
(* Gen/Kill *)
(*****************************************************************************)

(* "Any use of a variable generates liveness".
 * note that unit Dataflow.env is really simple a VarSet.t but
 * it's convenient to still use a VarMap (Dataflow.env) so we can
 * reuse Dataflow.varmap_union and Dataflow.varmap_diff.
*) 
let (gens: F.flow -> (unit Dataflow.env) array) = fun flow ->
  let arr = DataflowX.new_node_array flow VarMap.empty in
  V.fold_on_node_and_expr (fun (ni, _nd) e () ->
    (* rvalues here, to get the use of variables *)
    let rvals = Lrvalue.rvalues_of_expr e in
    (* note that Appel's book p385 says gen(x) is 
     * something - kill(x) but this is wrong, as shown
     * p214 which contradicts p385.
     *)
    let rvars = rvals |> List.map (fun ((s,_tok), _idinfo) -> s) in
    rvars |> List.iter (fun var ->
      arr.(ni) <- VarMap.add var () arr.(ni);
    )
  ) flow ();
  arr

(* "Any definition kills liveness". Indeed, if you assign a new value
 * in a variable b, and you don't use the previous value of b in this
 * assignment, then the previous value of b is indeed not needed just before
 * this assignment.
 *)
let (kills: F.flow -> (unit Dataflow.env) array) =
 fun flow -> 
  let arr = DataflowX.new_node_array flow (Dataflow.empty_env()) in
  V.fold_on_node_and_expr (fun (ni, _nd) e () ->
    let lvals = Lrvalue.lvalues_of_expr e in
    let vars = lvals |> List.map (fun ((s,_tok), _idinfo) -> s) in
    vars |> List.iter (fun var ->
      arr.(ni) <- VarMap.add var () arr.(ni);
    )
  ) flow ();
  arr

(*****************************************************************************)
(* Transfer *)
(*****************************************************************************)

let union = 
  Dataflow.varmap_union (fun () () -> ())
let diff =
  Dataflow.varmap_diff (fun () () -> ()) (fun () -> true)

(*
 * This algorithm is taken from Modern Compiler Implementation in ML, Appel,
 * 1998, pp. 385
 *
 * The transfer is setting:
 *  - in[n] = gen[n] U (out[n] - kill[n])
 *  - out[n] = U_{s in succ[n]} in[s]
 *)
let (transfer:
   gen:(unit Dataflow.env) array ->
   kill:(unit Dataflow.env) array ->
   flow:F.flow ->
   unit Dataflow.transfn) =
 fun ~gen ~kill ~flow ->
  (* the transfer function to update the mapping at node index ni *)
  fun mapping ni ->

  let out' = 
    (flow#successors ni)#fold (fun acc (ni_succ, _) ->
       union acc mapping.(ni_succ).D.in_env
     ) VarMap.empty in
  let out_minus_kill = diff out' kill.(ni) in
  let in' = union gen.(ni) out_minus_kill in
  {D. in_env = in'; out_env = out'}


(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let (fixpoint: F.flow -> mapping) = fun flow ->
  let gen = gens flow in
  let kill = kills flow in

  DataflowX.fixpoint
    ~eq:(fun () () -> true)
    ~init:(DataflowX.new_node_array flow (Dataflow.empty_inout ()))
    ~trans:(transfer ~gen ~kill ~flow)
    (* liveness is a backward analysis! *)
    ~forward:false
    ~flow
