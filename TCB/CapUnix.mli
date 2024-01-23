(* Capability-aware wrappers of the dangerous functions in Unix.ml *)

val execvp : Cap.Exec.t -> string -> string array -> 'a
