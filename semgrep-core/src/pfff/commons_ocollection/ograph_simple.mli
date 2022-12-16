(*s: ograph_simple.mli *)

(* essentially a convenient way to access a hash and its reverse hash *)

class ['key, 'node, 'edge] ograph_mutable :
  object ('o)
    method add_node : 'key -> 'node -> unit
    method del_node : 'key -> unit
    method replace_node: 'key -> 'node -> unit
    method add_node_if_not_present: 'key -> 'node -> unit


    method add_arc : ('key * 'key) -> 'edge -> unit
    method del_arc : ('key * 'key) -> 'edge -> unit

    method nodes : ('key, 'node) Oassoc.oassoc

    method successors : 'key -> ('key * 'edge) Oset.oset
    method predecessors : 'key -> ('key * 'edge) Oset.oset
    method allsuccessors : ('key, ('key * 'edge) Oset.oset) Oassoc.oassoc


    method del_leaf_node_and_its_edges: 'key -> unit
    method ancestors : 'key -> 'key Oset.oset
    method leaf_nodes : unit -> 'key Oset.oset

  end

val print_ograph_generic:
  str_of_key:('key -> string) ->
  str_of_node:('key -> 'node -> string) ->
  Common.filename ->
  ('key, 'node,'edge) ograph_mutable ->
  unit

(*e: ograph_simple.mli *)
