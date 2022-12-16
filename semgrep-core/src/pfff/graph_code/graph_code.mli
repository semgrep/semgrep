
(* the main types *)
type node = entity_name * Entity_code.entity_kind
and entity_name = string

type nodeinfo = {
  (* the filename embedded inside token_location can be a readable path *)
  pos: Parse_info.token_location;
  props: Entity_code.property list;
  typ: string option;
}
type edge = Has | Use
type edgeinfo = {
  write: bool;
  read: bool;
}
(* !! the main type!! the graph!! really an hypergraph actually *)
type t

(* error and statistics *)
type error =
  | NodeAlreadyPresent of node
exception Error of error
val string_of_error: error -> string

type statistics = {
  parse_errors: Common.filename list ref;
  (* could be Parse_info.token_location*)
  lookup_fail: (Parse_info.t * node) list ref;
  method_calls: (Parse_info.t * resolved) list ref;
  field_access: (Parse_info.t * resolved) list ref;
  unresolved_class_access: Parse_info.t list ref;
  unresolved_calls: Parse_info.t list ref;
}
and resolved = bool
val empty_statistics: unit -> statistics

(* moving around directories to have less backward dependencies *)
type adjust = (string * string)
(* skip certain edges that are marked as ok regarding backward dependencies *)
type dependency = (node * node)
type whitelist = dependency list


(* to bump when change of format. load() should detect when loading old graphs*)
val version: int

(* IO *)
val load: Common.filename -> t
val save: t -> Common.filename -> unit
val default_filename: string


val root: node
val pb: node
val not_found: node
val dupe: node
(* val stdlib: node *)

(* similar API to graph.ml *)

(* graph construction *)
val create: unit -> t
(* may raise NodeAlreadyPresent *)
val add_node: node -> t -> unit
val add_nodeinfo: node -> nodeinfo -> t -> unit
val add_edge: (node * node) -> edge -> t -> unit
val add_edgeinfo: (node * node) -> edge -> edgeinfo -> t -> unit
val remove_edge: (node * node) -> edge -> t -> unit

(* graph construction helpers *)
val create_initial_hierarchy: t -> unit
val create_intermediate_directories_if_not_present:
  t -> Common.dirname -> unit
val remove_empty_nodes: t -> node list -> unit
(* useful for bytecode <-> source file heuristic matching *)
val basename_to_readable_disambiguator:
  (Common.filename list) -> root:Common.dirname ->
  (string (* basename *) -> Common.filename list)

(* graph access *)
val has_node: node -> t -> bool
val succ: node -> edge -> t -> node list
(* this is slow, take care, or use mk_eff_use_pred below *)
val pred: node -> edge -> t -> node list
val mk_eff_use_pred: t -> (node -> node list)
(* can raise a Not_found exception *)
val parent: node -> t -> node
val parents: node -> t -> node list
val children: node -> t -> node list
val node_and_all_children: node -> t -> node list
(* may raise Not_found *)
val nodeinfo: node -> t -> nodeinfo
val nodeinfo_opt: node -> t -> nodeinfo option
val edgeinfo_opt: (node * node) -> edge -> t -> edgeinfo option
(* should be in readable path if you want your codegraph to be "portable" *)
val file_of_node: node -> t -> Common.filename
val privacy_of_node: node -> t -> Entity_code.privacy
val shortname_of_node: node -> string
val gensym: string -> string

(* iteration *)
val iter_use_edges: (node -> node -> unit) -> t -> unit
val iter_has_edges: (node -> node -> unit) -> t -> unit
val iter_nodes: (node -> unit) -> t -> unit

val all_use_edges: t -> (node * node) list
val all_has_edges: t -> (node * node) list
val all_nodes: t -> node list

(* statistics *)
val nb_nodes: t -> int
val nb_use_edges: t -> int
val print_statistics: statistics -> t -> unit

(* algorithms *)
val group_edges_by_files_edges:
  (node * node) list -> t ->
  ((Common.filename * Common.filename) * (node * node) list) list
val strongly_connected_components_use_graph:
  t -> (node list array * (node, int) Hashtbl.t)
(* bottom nodes have 0 for their numbering *)
val bottom_up_numbering:
  t -> (node, int) Hashtbl.t
(* top nodes have 0 for their numbering, less useful in practice *)
val top_down_numbering:
  t -> (node, int) Hashtbl.t

(* example builder *)
val graph_of_dotfile: Common.filename -> t


(* debugging support *)
val string_of_node: node -> string
val display_with_gv: t -> unit

(* adjustments *)
val load_adjust: Common.filename -> adjust list
val load_whitelist: Common.filename -> whitelist
val save_whitelist: whitelist -> Common.filename -> t -> unit
(* does side effect on the graph *)
val adjust_graph: t -> adjust list -> whitelist -> unit
