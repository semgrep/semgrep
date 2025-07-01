val supported_commands : string list
(** [supported_commands] is a list of all the LSP commands that are supported by the server. *)

val handle_execute_request :
  Legacy_session.t ->
  string ->
  Yojson.Safe.t list ->
  Legacy_session.t * Legacy_lsp_.Reply.t option
(** [handle_execute_request server command args] handles an LSP Command, and returns a response option, and updated server.
    LSP Commands are usually triggered through code actions, such as autofix, or ignoring findings. *)
