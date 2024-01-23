(* Capability-aware wrappers of the dangerous functions in Stdlib.ml *)

val exit : Cap.Process.exit -> int -> 'a
