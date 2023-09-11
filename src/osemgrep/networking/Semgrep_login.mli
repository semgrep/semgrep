type login_session = Uuidm.t * Uri.t
(** [login_session] is a request token and request url tuple.*)

val support_url : string
(** [support_url] is the default url users are directed to for support *)

val make_login_url : unit -> login_session
(** [make_login_url ()] will generate a request token and the url to request.
  * The token is just a UUID token, while the url changes based on the run
  * environment (gha, cli etc.)
  *)

val save_token : ?ident:string option -> string -> (unit, string) result
(** [save_token ?ident token] will save the token to the user's settings file.
  * If it fails, it will return an error message.
  * [ident] is the login identifier to be used as an opaque UUID once hashed
  * [token] (auth token) is the token to save for future API calls
  *)

val is_logged_in : unit -> bool
(** [is_logged_in ()] will check if the user is logged in by checking if a
  * token in the settings file exists.
  *)

val fetch_token :
  ?min_wait_ms:int ->
  ?next_wait_ms:int ->
  ?max_retries:int ->
  ?wait_hook:(int -> unit) ->
  login_session ->
  (string * string, string) result
(** [fetch_token ?min_wait_ms ?next_wait_ms ?max_retries wait_hook login_session] will
  * fetch the token using the request token and url the login session. It will retry up to [max_retries]
  * times, waiting [min_wait_ms] ms between each retry, and increasing the
  * wait time by [next_wait_ms] ms each time. If it fails, it will return an
  * error message. These will give users ~2 minutes to login
  * [wait_hook] is a function that will be called before each retry
  *)
