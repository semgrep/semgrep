
val graph_to_gefx:
  str_of_node:('a -> string) ->
  output:Common.filename ->
  tree:('a, 'a) Common2.tree option ->
  weight_edges: ('a * 'a, float) Hashtbl.t option ->
  'a Graph.graph ->
  unit
