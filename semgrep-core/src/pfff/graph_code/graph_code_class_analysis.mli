
(* It can actually be a DAG, as with interfaces and traits one can fake
 * multiple inheritance.
 * The direction of the graph is efficient to get the children
 * of a class: Parent -> Children.
 *
*)
type class_hierarchy = Graph_code.node Graphe.graph

val class_hierarchy: Graph_code.t -> class_hierarchy

(* Return the toplevel methods for each method name in the graph.
 * The returned hashtbl uses the Hashtbl.find_all property.
*)
val toplevel_methods:
  Graph_code.t -> class_hierarchy ->
  (string, Graph_code.node(*list*)) Hashtbl.t

(* Return the possible dispatched methods. *)
val dispatched_methods:
  Graph_code.t -> class_hierarchy -> Graph_code.node (* a method *) ->
  Graph_code.node list


(* print optimization opportunities, also print dead fields *)
val protected_to_private_candidates: Graph_code.t -> unit
