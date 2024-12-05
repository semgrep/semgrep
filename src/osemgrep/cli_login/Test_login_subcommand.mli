val tests : Login_subcommand.caps -> Testo.t list

(* to be reused in other tests *)

val with_fake_deployment_response : string -> (unit -> 'a) -> 'a
val with_semgrep_logged_in : (unit -> 'a) -> 'a
val fake_token : string
val fake_deployment : string
