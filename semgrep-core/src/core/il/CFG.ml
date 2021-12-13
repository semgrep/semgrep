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

type nodei = Ograph_extended.nodei

module NodeiSet = Set.Make (Int)

(* A graph with its entry point and (for convenience) the pre-computed set of
 * reachable nodes. Since the node type is abstract, we do need the id of the
 * entry node to compute the set of reachable nodes in the graph. *)
type ('node, 'edge) t = {
  graph : ('node, 'edge) Ograph_extended.ograph_mutable;
  entry : nodei;
  reachable : NodeiSet.t;
}

type ('node, 'edge) cfg = ('node, 'edge) t

let make (graph : _ Ograph_extended.ograph_mutable) entry : _ t =
  let rec aux nodei seen =
    if NodeiSet.mem nodei seen then seen
    else
      let seen = NodeiSet.add nodei seen in
      let succs =
        (graph#successors nodei)#fold
          (fun s (ni, _) -> NodeiSet.add ni s)
          NodeiSet.empty
      in
      NodeiSet.fold aux succs seen
  in
  { graph; entry; reachable = aux entry NodeiSet.empty }

(* Predecessors of a node (that can be reached from the entry node). *)
let predecessors cfg nodei : (nodei * 'node) list =
  (cfg.graph#predecessors nodei)#tolist
  |> List.filter (fun (pi, _) -> NodeiSet.mem pi cfg.reachable)

(* Successors of a node (returns an empty list for unreachable nodes). *)
let successors cfg nodei : (nodei * 'node) list =
  if NodeiSet.mem nodei cfg.reachable then (cfg.graph#successors nodei)#tolist
  else []
