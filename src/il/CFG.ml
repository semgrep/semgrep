(* Yoann Padioleau
 *
 * Copyright (C) 2019-2021 Semgrep Inc.
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

(* TODO: we should use ocamlgraph instead of ograph and its Ograph_extended.ml
 *)

type nodei = Ograph_extended.nodei

module NodeiSet = Set.Make (Int)

(* A graph with its entry point and (for convenience) the pre-computed set of
 * reachable nodes. Since the node type is abstract, we do need the id of the
 * entry node to compute the set of reachable nodes in the graph. *)
type ('node, 'edge) t = {
  graph : ('node, 'edge) Ograph_extended.ograph_mutable;
  entry : nodei;
  exit : nodei;
  reachable : NodeiSet.t;
}

type ('node, 'edge) cfg = ('node, 'edge) t

let make (graph : _ Ograph_extended.ograph_mutable) entry exit : _ t =
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
  { graph; entry; exit; reachable = aux entry NodeiSet.empty }

let reachable_nodes cfg =
  cfg.reachable |> NodeiSet.to_seq |> Seq.map cfg.graph#nodes#assoc

(* Predecessors of a node (that can be reached from the entry node). *)
let predecessors cfg nodei : (nodei * 'node) list =
  (cfg.graph#predecessors nodei)#tolist
  |> List.filter (fun (pi, _) -> NodeiSet.mem pi cfg.reachable)

(* Successors of a node (returns an empty list for unreachable nodes). *)
let successors cfg nodei : (nodei * 'node) list =
  if NodeiSet.mem nodei cfg.reachable then (cfg.graph#successors nodei)#tolist
  else []
