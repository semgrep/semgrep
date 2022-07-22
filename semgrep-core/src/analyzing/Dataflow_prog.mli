type nodei = int

(* The comparison function uses only the name of a variable (a string), so
 * two variables at different positions in the code will be agglomerated
 * correctly in the Set or Map.
 *)
type var = string

module VarMap : Map.S with type key = String.t
module VarSet : Set.S with type elt = String.t

module Make (F : Dataflow_core.Flow) : sig
  module ProgFlow : sig
    type node = Reg of F.node | Func of flow
    and edge = F.edge
    and flow = (node, edge) CFG.t

    val short_string_of_node : node -> string
  end

  module ProgDataflow : module type of Dataflow_core.Make (ProgFlow)

  val fixpoint :
    eq:('a -> 'a -> bool) ->
    init:'a Dataflow_core.mapping ->
    trans:'a Dataflow_core.transfn ->
    flow:ProgFlow.flow ->
    get_input_env:('a Dataflow_core.mapping -> nodei -> 'a Dataflow_core.env) ->
    forward:bool ->
    'a Dataflow_core.mapping
end
