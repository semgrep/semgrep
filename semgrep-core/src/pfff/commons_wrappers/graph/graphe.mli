
(* graph of polymorphic nodes *)
type 'a graph

(* graph construction *)
val create :
  unit -> 'a graph
val add_vertex_if_not_present :
  'a -> 'a graph -> unit
val add_edge :
  'a -> 'a -> 'a graph -> unit
(* this will also remove its associated edges *)
val remove_vertex:
  'a -> 'a graph -> unit
val remove_edge :
  'a -> 'a -> 'a graph -> unit
(* many algorithms works by side effect on the graph so need a copy function *)
val copy:
  'a graph -> 'a graph


(* graph access *)
val nodes: 'a graph -> 'a list
val succ: 'a -> 'a graph -> 'a list
val pred: 'a -> 'a graph -> 'a list (* slow! *)
val out_degree: 'a -> 'a graph -> int
val in_degree: 'a -> 'a graph -> int (* slow! *)
val has_node: 'a -> 'a graph -> bool

val nb_nodes: 'a graph -> int
val nb_edges: 'a graph -> int

val iter_edges: ('a -> 'a -> unit) -> 'a graph -> unit
val iter_nodes: ('a -> unit) -> 'a graph -> unit

val entry_nodes: 'a graph -> 'a list

(* internal vertex number *)
val ivertex: 'a -> 'a graph -> int

(* algorithms *)
val shortest_path:
  'a -> 'a -> 'a graph -> 'a list
val transitive_closure:
  'a graph -> 'a graph
val mirror:
  'a graph -> 'a graph
val strongly_connected_components:
  'a graph -> ('a list array * ('a, int) Hashtbl.t)
(* result will be a DAG *)
val strongly_connected_components_condensation:
  'a graph -> ('a list array * ('a, int) Hashtbl.t) -> int graph
(* assumes a DAG *)
val depth_nodes:
  'a graph -> ('a, int) Hashtbl.t


(* debugging support *)
val print_graph_generic :
  ?launch_gv:bool ->
  ?extra_string:string ->
  str_of_key:('a -> string) ->
  Common.filename (* dot file *) ->
  'a graph -> unit

val display_with_gv:
  'a graph -> unit
val display_strongly_connected_components :
  str_of_key:('a -> string) -> ('a, int) Hashtbl.t -> 'a graph -> unit

(* internals *)
val stat:
  'a graph -> unit
