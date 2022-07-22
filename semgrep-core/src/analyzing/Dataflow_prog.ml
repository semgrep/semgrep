module DC = Dataflow_core

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* This is a wrapper around Dataflow_core, specifically specialized to deal
   with analyses on programs.
   Specifically, our notion of CFGs currently has a "second-class" representation
   for functions, which results in a lot of code where we need to tip-toe around
   function definitions especially. This is harmful, because it leads to bugs when
   dealing with any analyses that deal with bringing top-level domains into the
   functions.
   Dataflow_prog has a "first-class" notion of functions via "function nodes",
   which are mandated to be such that the dataflow analysis will run as normal on
   these special nodes, but the out-set of these function nodes must be the same as
   the in-set to the function node.
*)

type nodei = int
type var = string

module VarMap = Dataflow_core.VarMap
module VarSet = Dataflow_core.VarSet

module Make (F : Dataflow_core.Flow) = struct
  module ProgFlow = struct
    type node = IL.node
    type edge = IL.edge
    type flow = IL.cfg

    let short_string_of_node { IL.n } =
      match n with
      | IL.Reg node -> [%show: IL.node_kind] node
      | Func _ -> "<func>"
  end

  module ProgDataflow = Dataflow_core.Make (ProgFlow)

  let rec fixpoint :
      eq:('a -> 'a -> bool) ->
      init:'a DC.mapping ->
      trans:'a DC.transfn ->
      flow:IL.cfg ->
      get_input_env:('a DC.mapping -> nodei -> 'a DC.env) ->
      forward:bool ->
      'a DC.mapping =
   fun ~eq ~init ~trans ~flow ~get_input_env ~forward ->
    ProgDataflow.fixpoint ~eq ~init
      ~trans:(fun mapping ni ->
        let { IL.n } = flow.graph#nodes#assoc ni in
        match n with
        | IL.Reg _ -> trans mapping ni
        | Func { cfg = flow; _ } ->
            (* TODO: change mapping here depending on the function inputs *)
            fixpoint ~eq ~init:mapping ~trans ~flow ~get_input_env ~forward
            |> ignore;
            (* Environment does not change for a function node. *)
            let env = get_input_env mapping ni in
            { in_env = env; out_env = env })
      ~flow ~forward

  let new_node_array = ProgDataflow.new_node_array
  let display_mapping = ProgDataflow.display_mapping
end
