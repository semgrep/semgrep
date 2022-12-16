open Dependencies_matrix_code

(* pack less relevant directories under an intermediate "..." subdirectory *)
val threshold_pack: int ref

(* we now return also a new graph because we may have modified the
 * input graph to add some extra "..." nodes.
 * The 'config' parameter passed is a default configuration but the returned
 * config in dm.config will probably be a better ordering of the
 * nodes that minimizes backward dependencies.
*)
val build:
  config -> partition_constraints option -> Graph_code_opti.graph ->
  dm * Graph_code_opti.graph

val info_orders:
  dm -> unit

(* we return a gopti because of threshold_pack that may alter the graph *)
val config_of_path:
  config_path -> Graph_code_opti.graph -> config * Graph_code_opti.graph

(* internals *)
val build_with_tree:
  config -> Graph_code_opti.graph -> dm
val partition_matrix:
  Graph_code.node list -> dm -> Graph_code.node list

(* heuristics for ordering the nodes *)
val sort_by_count_rows_low_columns_high_first:
  Graph_code.node list -> int array array -> dm -> Graph_code.node list
val hill_climbing:
  Graph_code.node list -> dm -> Graph_code.node list
