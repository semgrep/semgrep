
val create_graph:
  ?show_progress:bool -> ?strict:bool ->
  Common.filename list -> Env_interpreter_php.code_database ->
  Callgraph_php2.callgraph
