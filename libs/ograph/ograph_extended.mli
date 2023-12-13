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

class ['node, 'edge] ograph_extended : object ('o)
  method add_node : 'node -> 'o * nodei
  method add_nodei : nodei -> 'node -> 'o * nodei
  method replace_node : nodei * 'node -> 'o
  method del_node : nodei -> 'o
  method add_arc : (nodei * nodei) * 'edge -> 'o
  method del_arc : (nodei * nodei) * 'edge -> 'o
  method nodes : (nodei, 'node) Oassoc.oassoc
  method successors : nodei -> (nodei * 'edge) Oset.oset
  method predecessors : nodei -> (nodei * 'edge) Oset.oset
  method allsuccessors : (nodei, (nodei * 'edge) Oset.oset) Oassoc.oassoc
end

class ['node, 'edge] ograph_mutable : object ('o)
  method add_node : 'node -> nodei
  method add_nodei : nodei -> 'node -> unit
  method replace_node : nodei * 'node -> unit
  method del_node : nodei -> unit
  method add_arc : (nodei * nodei) * 'edge -> unit
  method del_arc : (nodei * nodei) * 'edge -> unit
  method nodes : (nodei, 'node) Oassoc.oassoc
  method successors : nodei -> (nodei * 'edge) Oset.oset
  method predecessors : nodei -> (nodei * 'edge) Oset.oset
  method allsuccessors : (nodei, (nodei * 'edge) Oset.oset) Oassoc.oassoc
  method nb_nodes : int
  method nb_edges : int
end

val dfs_iter : nodei -> (nodei -> unit) -> ('node, 'edge) ograph_mutable -> unit

val dfs_iter_with_path :
  nodei ->
  (nodei -> nodei list -> unit) ->
  ('node, 'edge) ograph_mutable ->
  unit
