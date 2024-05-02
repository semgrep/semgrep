(* alt: use Login_subcommand.caps *)
val tests : < Cap.network ; Cap.stdout > -> Testo.t list

(* to be reused in other tests *)

val with_login_test_env : (unit -> 'a) -> unit -> 'a
val with_fake_deployment_response : string -> (unit -> 'a) -> 'a
val fake_token : string
val fake_deployment : string
