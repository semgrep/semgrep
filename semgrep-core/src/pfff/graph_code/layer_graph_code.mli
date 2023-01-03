val gen_rank_heatmap_layer :
  Graph_code.t ->
  (Graph_code.node, int) Hashtbl.t ->
  output:Common.filename ->
  unit

val gen_statistics_layer :
  root:Common.dirname -> Graph_code.statistics -> output:Common.filename -> unit

val actions : unit -> Common.cmdline_actions
