(*
   Show the current identity of the user running the command.
*)

type identity_kind = Identity | Deployment

val print : identity_kind -> Exit_code.t
