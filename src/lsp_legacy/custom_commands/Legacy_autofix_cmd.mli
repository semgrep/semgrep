val command : string

val create : unit -> Lsp.Types.Command.t
(** [create ()] creates a new command that will let the LS know a fix was applied*)

val command_handler :
  Legacy_session.t -> 'a -> Legacy_session.t * Legacy_lsp_.Reply.t option
(** [command_handler session state] records the fact that a fix was applied in the state *)
