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

let ( let* ) = Option.bind

module VarMap = Dataflow_core.VarMap
module VarSet = Dataflow_core.VarSet

module Make (F : Dataflow_core.Flow) = struct
  module ProgFlow = struct
    type node = IL.node
    type edge = IL.edge
    type flow = IL.cfg

    let short_string_of_node { IL.n } = [%show: IL.node_kind] n
  end

  module CoreDataflow = Dataflow_core.Make (ProgFlow)

  let str_of_name name = Common.spf "%s:%d" (fst name.IL.ident) name.sid

  let rec fixpoint :
      enter_env:'a DC.env ->
      eq:('a -> 'a -> bool) ->
      init:'a DC.mapping ->
      trans:(Dataflow_core.var option -> IL.cfg -> 'a DC.env -> 'a DC.transfn) ->
      flow:IL.cfg ->
      get_input_env:
        ('a DC.env -> IL.cfg -> 'a DC.mapping -> nodei -> 'config -> 'a DC.env) ->
      config:'config ->
      forward:bool ->
      name:Dataflow_core.var option ->
      'a DC.mapping =
   fun ~enter_env ~eq ~init ~trans ~flow ~get_input_env ~config ~forward ~name ->
    let enter_new_cfg ~env ~name ~flow:new_flow =
      let new_mapping =
        CoreDataflow.new_node_array new_flow (Dataflow_core.empty_inout ())
      in
      fixpoint ~enter_env:env ~eq ~init:new_mapping ~trans ~flow:new_flow
        ~get_input_env ~config ~forward ~name
    in
    CoreDataflow.fixpoint ~eq ~init
      ~trans:(fun mapping ni ->
        (* Input env gets the entering environment in case that it encounters
           an Enter node.
           It returns the proper entering environment (given parameters to functions, etc)
           for that node.
        *)
        let env = get_input_env enter_env flow mapping ni config in
        match (flow.graph#nodes#assoc ni).n with
        | NFunc { cfg = new_flow; ent; _ } ->
            (* We want the entrance env to this function node, computed via looking at
               the current node within the old flow, along with the function definition.
            *)
            let name =
              let* name = AST_to_IL.name_of_entity ent in
              Some (str_of_name name)
            in
            enter_new_cfg ~env ~name ~flow:new_flow |> ignore;
            (* Environment does not change for a function node. *)
            { in_env = env; out_env = env }
        | NClass cfg
        | NModule cfg ->
            enter_new_cfg ~env ~name:None ~flow:cfg |> ignore;
            (* Assume that environment does not change for a class node *)
            { in_env = env; out_env = env }
        | Enter
        | Exit
        | TrueNode
        | FalseNode
        | Join
        | NInstr _
        | NCond _
        | NGoto _
        | NReturn _
        | NThrow _
        | NOther _
        | NTodo _ ->
            trans name flow env mapping ni)
      ~flow ~forward

  let new_node_array = CoreDataflow.new_node_array
  let display_mapping = CoreDataflow.display_mapping
end
