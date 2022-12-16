
type graph = {
  name_to_i: (Graph_code.node, int) Hashtbl.t;
  i_to_name: Graph_code.node array;

  has_children: (int list) array;
  use: (int list) array;
}

val convert: Graph_code.t -> graph

val nb_nodes: graph -> int

val children:     Graph_code.node -> graph -> Graph_code.node list
val all_children: Graph_code.node -> graph -> Graph_code.node list

val has_node: Graph_code.node -> graph -> bool

val adjust_graph_pack_some_children_under_dotdotdot:
  Graph_code.node -> Graph_code.node list -> graph -> graph * Graph_code.node
