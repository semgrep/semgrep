type login_session = Uuidm.t * Uri.t
(** [login_session] is a request token and request url tuple.*)

val support_url : string
(** [support_url] is the default url users are directed to for support *)

val make_login_url : unit -> login_session
(** [make_login_url ()] will generate a request token and the url to request.
  * The token is just a UUID token, while the url changes based on the run
  * environment (gha, cli etc.)
  *)

val save_token : string -> (unit, string) result
(** [save_token token] will save the token to the user's home directory.
  * If it fails, it will return an error message.
  *)

val is_logged_in : unit -> bool
(** [is_logged_in ()] will check if the user is logged in by checking if a
  * valid token in the settings file exists.
  *)

val fetch_token :
  ?min_wait:int ->
  ?next_wait:int ->
  ?max_retries:int ->
  ?wait_hook:(unit -> unit) ->
  login_session ->
  (string * string, string) result
(** [fetch_token ?min_wait ?next_wait ?max_retries wait_hook login_session] will
  * fetch the token using the request token and url the login session. It will retry up to [max_retries]
  * times, waiting [min_wait] seconds between each retry, and increasing the
  * wait time by [next_wait] seconds each time. If it fails, it will return an
  * error message.
  * [wait_hook] is a function that will be called before each retry
  *)
