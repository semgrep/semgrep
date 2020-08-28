
val build:
  ?verbose:bool -> Common.dirname -> Common.filename list ->
  Graph_code.graph

(* if want prolog facts generation *)
val hook_use_edge: 
  (Graph_code_prolog.context -> bool (* in_assign *) -> 
  (Graph_code.node * Graph_code.node) -> 
   Graph_code.graph -> 
   Parse_info.token_location ->
   unit)
  ref

val hook_def_node:
  (Graph_code.node -> Graph_code.graph -> unit) ref

(* if want datalog facts generation *)
val facts: Datalog_c.fact list ref option ref
