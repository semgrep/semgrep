
(* dependency structure matrix *)
type dm = {
  matrix: int array array;
  i_to_name: Graph_code.node array;
  name_to_i: (Graph_code.node, int) Hashtbl.t;
  config: config;
}
and config = tree
and tree =
  | Node of Graph_code.node * tree list

type partition_constraints =
  (string, string list) Hashtbl.t
type cell_coord =
  (int * int)

val verbose: bool ref

val explain_cell_list_use_edges:
  cell_coord -> dm -> Graph_code_opti.graph ->
  (Graph_code.node * Graph_code.node) list

(* just the expanded root *)
val basic_config: Graph_code.t -> config
val basic_config_opti: Graph_code_opti.graph -> config

type config_path_elem =
  | Expand of Graph_code.node
  | Focus of Graph_code.node * deps_style
and deps_style =
  | DepsIn
  | DepsOut
  | DepsInOut
type config_path = config_path_elem list

val string_of_config_path: config_path -> string

(* tree config manipulation *)
val expand_node:
  Graph_code.node -> tree -> Graph_code.t -> tree
val expand_node_opti:
  Graph_code.node -> tree -> Graph_code_opti.graph -> tree
val focus_on_node:
  Graph_code.node -> deps_style -> tree -> dm -> tree


(* matrix analysis *)
val is_dead_column:
  int -> dm -> bool
val is_dead_line:
  int -> dm -> bool
val is_internal_helper:
  int -> dm -> bool

val score_upper_triangle:
  dm -> Graph_code.node list (* exclude list *) -> int
val score_downer_triangle:
  dm -> Graph_code.node list (* exclude list *) -> int
val score_upper_triangle_nodes:
  dm -> (Graph_code.node * int) list
val score_upper_triangle_cells:
  dm -> (cell_coord * int) list

(* APIs useful for other to use *)
val final_nodes_of_tree: tree -> Graph_code.node list

(* should be used only by test code *)
val distance_entity:
  cell_coord -> Graph_code.node list array -> int
val parents_of_indexes:
  dm -> Graph_code.node list array

(* poor's man DSM visualizer (use codegraph for real visualization) *)
val display:
  dm -> unit
