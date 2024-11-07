type shared_secret = Uuidm.t
(** [shared_secret] is shared secret between the Semgrep App and the CLI.
  We use a UUID (a sufficiently random and therefore exceedingly unlikely for
  both accidental collisions and brute force attacks) as the shared secret.
  Depending on the user flow, the CLI will either generate a new UUID and
  ask the App DB to create and store a new access token keyed by that UUID,
  or the App will generate a UUID and corresponidng access token and ask the CLI
  to retrieve the access token for that UUID. In both cases, the CLI will have
  the responsibility of storing the access token in the user's settings file.
  *)

type login_session = shared_secret * Uri.t
(** [login_session] is a request token and request url tuple.*)

val support_url : string
(** [support_url] is the default url users are directed to for support *)

val make_login_url : unit -> login_session
(** [make_login_url ()] will generate a request token and the url to request.
  * The token is just a UUID token, while the url changes based on the run
  * environment (gha, cli etc.)
  *)

(* need the network to first check whether the token is valid *)
val save_token_async :
  ?ident:string ->
  < network : Cap.Network.t ; token : Auth.token > ->
  (Semgrep_output_v1_t.deployment_config, string) result Lwt.t

val save_token :
  ?ident:string ->
  < network : Cap.Network.t ; token : Auth.token > ->
  (Semgrep_output_v1_t.deployment_config, string) result
(** [save_token ?ident token] will save the token to the user's settings file.
  * If it fails, it will return an error message.
  * [ident] is the login identifier that can be used as an opaque UUID once
  * hashed [token] (auth token) is the token to save for future API calls
  *)

val fetch_token :
  ?min_wait_ms:int ->
  ?next_wait_ms:int ->
  ?max_retries:int ->
  ?wait_hook:(int -> unit) ->
  < network : Cap.Network.t ; .. > ->
  shared_secret ->
  (Auth.token * string, string) result
(** [fetch_token ?min_wait_ms ?next_wait_ms ?max_retries wait_hook shared_secret] will
  * fetch the token using the request token and url the login session. It will retry up to [max_retries]
  * times, waiting [min_wait_ms] ms between each retry, and increasing the
  * wait time by [next_wait_ms] ms each time. If it fails, it will return an
  * error message. These will give users ~2 minutes to login
  * [wait_hook] is a function that will be called before each retry
  *)

val fetch_token_async :
  ?min_wait_ms:int ->
  ?next_wait_ms:int ->
  ?max_retries:int ->
  ?wait_hook:(int -> unit Lwt.t) ->
  < network : Cap.Network.t ; .. > ->
  shared_secret ->
  (Auth.token * string, string) result Lwt.t
(** [fetch_token_async ?min_wait_ms ?next_wait_ms ?max_retries wait_hook
  * shared_secret] will fetch the token using the request token and url the
  * login session. It will retry up to [max_retries] times, waiting
  * [min_wait_ms] ms between each retry, and increasing the wait time
  * by [next_wait_ms] ms each time, returning a promise if successful. If it
  * fails, it will return an error message. These will give users ~2 minutes
  * to login [wait_hook] is a function that will be called before each retry
  *)

val verify_token_async :
  < network : Cap.Network.t ; token : Auth.token > -> bool Lwt.t
(** [verify_token_async] verifies that a token is valid with the Semgrep App. *)

val verify_token : < network : Cap.Network.t ; token : Auth.token > -> bool
(** [verify_token] verifies that a token is valid with the Semgrep App. *)

val is_logged_in_weak : unit -> bool
(** this does not really check whether the user is logged in; it just checks
 * whether a token is defined in ~/.semgrep/settings.yml (or in
 * SEMGREP_APP_TOKEN.
 *)
