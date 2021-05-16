(* map for each node/var a set of nodes containing a var assignment
 * that can reach that node
 *)
type mapping = Dataflow.NodeiSet.t Dataflow.mapping

(* main entry point *)
val fixpoint : Controlflow.flow -> mapping
