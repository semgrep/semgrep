(* Capability-aware wrappers of the dangerous functions in Sys.ml *)

val argv : Cap.Process.argv -> string array
val set_signal : Cap.Process.signal -> int -> Sys.signal_behavior -> unit
val chdir : Cap.Process.chdir -> string -> unit
