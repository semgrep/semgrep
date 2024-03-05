(*
   Show the current identity of the user running the command.
*)

type identity_kind = Identity | Deployment

(* TODO: actually it's using Logs.app which prints on stderr *)
val print : < Cap.network ; Cap.stdout > -> identity_kind -> Exit_code.t
