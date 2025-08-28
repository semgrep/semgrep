(*
   Copyright (c) 2022-2025 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
type nodei = int

(* graph structure:
 *  - node: index -> nodevalue
 *  - arc: (index * index) * edgevalue
 *
 * How ? matrix ? but no growing array :(
 *
 * When need index ? Must have an index when can't just use the nodevalue
 * as a key, cos sometimes may have 2 times the same key, but it must
 * be 2 different nodes. For instance in a C program 'f(); f();' we want 2
 * nodes, one per 'f();' hence the index. If each node is different, then
 * no problem, can omit index.
 *)

class ['node, 'edge] ograph_mutable : object ('o)
  method add_node : 'node -> nodei
  method add_nodei : nodei -> 'node -> unit
  method replace_node : nodei * 'node -> unit
  method del_node : nodei -> unit
  method add_arc : (nodei * nodei) * 'edge -> unit
  method del_arc : (nodei * nodei) * 'edge -> unit
  method nodes : 'node Maps.Int_map.t
  method successors : nodei -> (nodei * 'edge) Set_.t
  method predecessors : nodei -> (nodei * 'edge) Set_.t
  method nb_nodes : int
end
