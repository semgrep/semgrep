(* moving around directories to have less backward dependencies *)
type adjust = string * string

(* skip certain edges that are marked as ok regarding backward dependencies *)
type dependency = Graph_code.node * Graph_code.node
type whitelist = dependency list

(* adjustments *)
val load_adjust : Fpath.t -> adjust list
val load_whitelist : Fpath.t -> whitelist
val save_whitelist : whitelist -> Fpath.t -> Graph_code.t -> unit

(* does side effect on the graph *)
val adjust_graph : Graph_code.t -> adjust list -> whitelist -> unit
