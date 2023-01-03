val build :
  ?verbose:bool -> Common.dirname -> Common.filename list -> Graph_code.t

(* if want prolog facts generation *)
val hook_use_edge :
  (Graph_code_prolog.context ->
  bool (* in_assign *) ->
  Graph_code.node * Graph_code.node ->
  Graph_code.t ->
  Parse_info.token_location ->
  unit)
  ref

val hook_def_node : (Graph_code.node -> Graph_code.t -> unit) ref

(* if want datalog facts generation *)
val facts : Datalog_c.fact list ref option ref
