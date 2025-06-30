val command : string
(** command name *)

type t = { path : string; fingerprint : string } [@@deriving yojson]
(** which finding to ignore*)

val create : path:string -> fingerprint:string -> Lsp.Types.Command.t
(** [create ~path ~fingerprint] creates a [Command.t] command to ignore a finding at [path] with [fingerprint] *)

val command_handler :
  Legacy_session.t ->
  Yojson.Safe.t list ->
  Legacy_session.t * Legacy_lsp_.Reply.t option
(** [command_handler session params] handles the ignore finding command *)
