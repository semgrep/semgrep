(* A Semgrep.dev token representing your identity.
 * This is stored in ~/.semgrep/settings.yml and initially fetched
 * from https://semgrep.dev during 'semgrep loging'
 *)
type token

(* to be used in headers *)
val string_of_token : token -> string

(* ("Authorisation:", "Bearer <token>") *)
val auth_header_of_token : token -> string * string

(* TODO: should require a semgrep_dev capa or semgrep_settings capa *)
val unsafe_token_of_string : string -> token

(* TODO? rename semgrep_token? *)
type cap_token = < token : token >

val cap_token_and_network :
  token -> < Cap.network ; .. > -> < cap_token ; Cap.network >

val cap_token_and_network_and_tmp :
  token ->
  < Cap.network ; Cap.tmp ; .. > ->
  < cap_token ; Cap.network ; Cap.tmp >
