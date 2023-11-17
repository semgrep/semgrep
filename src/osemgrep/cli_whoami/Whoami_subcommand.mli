(*
   Show the current identity of the user running the command.
*)
val main : string array -> Exit_code.t

(* internal *)
val run : Whoami_CLI.conf -> Exit_code.t
