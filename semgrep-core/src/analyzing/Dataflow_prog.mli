type nodei = int

(* The comparison function uses only the name of a variable (a string), so
 * two variables at different positions in the code will be agglomerated
 * correctly in the Set or Map.
 *)
type var = string

module VarMap : Map.S with type key = String.t
module VarSet : Set.S with type elt = String.t
module DC = Dataflow_core

module Make (F : Dataflow_core.Flow) : sig
  module ProgFlow : sig
    type node = IL.node
    and edge = IL.edge
    and flow = IL.cfg

    val short_string_of_node : node -> string
  end

  module CoreDataflow : module type of Dataflow_core.Make (ProgFlow)

  val fixpoint :
    enter_env:'a DC.env ->
    eq:('a -> 'a -> bool) ->
    init:'a DC.mapping ->
    trans:(Dataflow_core.var option -> IL.cfg -> 'a DC.env -> 'a DC.transfn) ->
    flow:IL.cfg ->
    meet:('a DC.env -> IL.cfg -> 'a DC.mapping -> nodei -> 'config -> 'a DC.env) ->
    modify_env:('a DC.env -> IL.node -> 'config -> 'a DC.env) ->
    config:'config ->
    forward:bool ->
    name:Dataflow_core.var option ->
    conclude:(IL.cfg -> 'a DC.mapping -> unit) ->
    'a DC.mapping

  val new_node_array : IL.cfg -> 'a -> 'a array

  (* debugging output *)
  val display_mapping :
    IL.cfg -> 'a Dataflow_core.mapping -> ('a -> string) -> unit
end
