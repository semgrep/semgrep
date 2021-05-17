(* map for each node/var whether a variable is "live" *)
type mapping = unit Dataflow.mapping

(* main entry point *)
val fixpoint : Controlflow.flow -> mapping
