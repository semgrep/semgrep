(* A "code database" data structure represented as a graph of code entities.
 * This module is used mostly by https://github.com/aryx/codegraph and
 * https://github.com/aryx/codemap but is now also used in semgrep-pro.
 *)

(* The entity_name string must be unique, so it usually contains
 * the "fully qualified name" (FQN) of the entity (e.g., "Foo.Bar.foo_method").
 * See gensym() also below to help define unique enties.
 *)
type node = entity_name * Entity_code.kind
and entity_name = string

type nodeinfo = {
  (* the filename embedded inside location can be a readable path.
   * TODO: use an actual Pos.t? otherwise rename to loc.
   *)
  pos : Tok.location;
  (* extra info about an entity *)
  props : Entity_code.property list;
  typ : string option;
}

(* A code entity can be related to another entity either via containment (Has),
 * such as a field contained inside a class (itself contained inside a package),
 * or via reference (Use), such as a function calling another function or
 * a class inheriting from another class.
 *
 * In theory we just need those two kinds of edges; the source and destination of
 * an edge should be enough to understand the relation. For example,
 * a Use between two functions is probably a call, an Has between two
 * functions is probably the definition of a nested function, a Use between two
 * classes is probably an inheritance.
 *)
type edge = Has | Use

(* extra info about an edge, can be useful for fancy analysis *)
type edgeinfo = { write : bool; read : bool }

(* !! the main type!! the graph!! really an hypergraph actually *)
type t

(* error and statistics *)
type error = NodeAlreadyPresent of node

exception Error of error

val string_of_error : error -> string

type statistics = {
  parse_errors : Fpath.t list ref;
  (* could be Parse_info.token_location*)
  lookup_fail : (Tok.t * node) list ref;
  method_calls : (Tok.t * resolved) list ref;
  field_access : (Tok.t * resolved) list ref;
  unresolved_class_access : Tok.t list ref;
  unresolved_calls : Tok.t list ref;
}

(* ?? *)
and resolved = bool

val empty_statistics : unit -> statistics

(* to bump when change of format. load() should detect when loading old graphs*)
val version : int

(* IO *)
val load : Fpath.t -> t
val save : t -> Fpath.t -> unit
val default_filename : Fpath.t
val root : node
val pb : node
val not_found : node
val dupe : node
(* val stdlib: node *)

(* similar API to Graphe.ml *)

(* graph construction *)
val create : unit -> t

(* may raise NodeAlreadyPresent *)
val add_node : node -> t -> unit
val add_nodeinfo : node -> nodeinfo -> t -> unit
val add_edge : node * node -> edge -> t -> unit
val add_edgeinfo : node * node -> edge -> edgeinfo -> t -> unit
val remove_edge : node * node -> edge -> t -> unit

(* graph construction helpers *)
val create_initial_hierarchy : t -> unit

val create_intermediate_directories_if_not_present :
  t ->
  string
  (* filename *)
  (* a dir *) ->
  unit

val remove_empty_nodes : t -> node list -> unit

(* useful for bytecode <-> source file heuristic matching *)
val basename_to_readable_disambiguator :
  string (* filename *) list ->
  root:
    string
    (* filename *)
    (* a dir *) ->
  string (* basename *) ->
  string (* filename *) list

(* graph access *)
val has_node : node -> t -> bool
val succ : node -> edge -> t -> node list

(* this is slow, take care, or use mk_eff_use_pred below *)
val pred : node -> edge -> t -> node list
val mk_eff_use_pred : t -> node -> node list

(* can raise a Not_found exception *)
val parent : node -> t -> node
val parents : node -> t -> node list
val children : node -> t -> node list
val node_and_all_children : node -> t -> node list

(* may raise Not_found *)
val nodeinfo : node -> t -> nodeinfo
val nodeinfo_opt : node -> t -> nodeinfo option
val edgeinfo_opt : node * node -> edge -> t -> edgeinfo option

(* should be in readable path if you want your codegraph to be "portable" *)
val file_of_node : node -> t -> Fpath.t
val privacy_of_node : node -> t -> Entity_code.privacy
val shortname_of_node : node -> string

(* Helper to generate unique enties by suffix the first parameter
 * with __<counter>
 *)
val gensym : string -> string

(* iteration *)
val iter_use_edges : (node -> node -> unit) -> t -> unit
val iter_has_edges : (node -> node -> unit) -> t -> unit
val iter_nodes : (node -> unit) -> t -> unit
val all_use_edges : t -> (node * node) list
val all_has_edges : t -> (node * node) list
val all_nodes : t -> node list

(* statistics *)
val nb_nodes : t -> int
val nb_use_edges : t -> int

(* use Logs.info() (and not Log_graph_code.info) to log statistics *)
val log_statistics : statistics -> t -> unit

(* algorithms *)
val group_edges_by_files_edges :
  (node * node) list -> t -> ((Fpath.t * Fpath.t) * (node * node) list) list

val strongly_connected_components_use_graph :
  t -> node list array * (node, int) Hashtbl.t

(* bottom nodes have 0 for their numbering *)
val bottom_up_numbering : t -> (node, int) Hashtbl.t

(* top nodes have 0 for their numbering, less useful in practice *)
val top_down_numbering : t -> (node, int) Hashtbl.t

(* example builder *)
val graph_of_dotfile : Fpath.t -> t

(* debugging support, to be used in Logs *)
val string_of_node : node -> string
val display_with_gv : t -> unit
