
type context =
  | NoCtx
  | CallCtx of Graph_code.node
  | AssignCtx of Graph_code.node

val hook_use_edge_for_prolog: 
  context -> bool -> 
  (Graph_code.node * Graph_code.node) -> Graph_code.graph -> 
  Parse_info.token_location ->
  unit

val build: Graph_code.graph -> Prolog_code.fact list
