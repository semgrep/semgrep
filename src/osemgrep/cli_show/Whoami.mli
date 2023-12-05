(*
   Show the current identity of the user running the command.
*)

type identity_kind = Identity | Deployment

(* TODO: actually it's using Logs.app which prints on stderr *)
val print :
  < network : Cap.Network.t ; stdout : Cap.Console.stdout ; .. > ->
  identity_kind ->
  Exit_code.t
