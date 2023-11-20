(*
   Show the current identity of the user running the command.
*)

type identity = Identity | Deployment

(* internal *)
val invoke : identity -> Exit_code.t
