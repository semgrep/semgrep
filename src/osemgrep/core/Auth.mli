(* A Semgrep.dev token representing your identity.
 * This is stored in ~/.semgrep/settings.yml and initially fetched
 * from https://semgrep.dev during 'semgrep loging'
 *)
type token

(* to be used in headers *)
val string_of_token : token -> string

(* ("Authorisation:", "Bearer <token>") *)
val auth_header_of_token : token -> string * string

(* TODO: should require a semgrep_dev capability or semgrep_settings capability *)
val unsafe_token_of_string : string -> token

val cap_token_and_network :
  token ->
  < network : Cap.Network.t ; .. > ->
  < token : token ; network : Cap.Network.t >
