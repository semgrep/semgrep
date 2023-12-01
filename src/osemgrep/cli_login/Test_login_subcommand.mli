(* alt: use Login_subcommand.caps *)
val tests :
  < network : Cap.Network.t ; stdout : Cap.Console.stdout > ->
  Testutil.test list

(* to be reused in other tests *)

type result = { exit_code : Exit_code.t; logs : string }

val with_logs : f:(unit -> Exit_code.t) -> final:(result -> unit) -> unit
val with_login_test_env : (unit -> 'a) -> unit -> 'a
val with_fake_deployment_response : string -> (unit -> unit) -> unit
val fake_token : string
val fake_deployment : string
