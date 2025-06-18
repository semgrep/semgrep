val command : string

val create : unit -> Lsp.Types.Command.t
(** [create ()] creates a new command that will let the LS know a fix was applied*)

val command_handler : Session.t -> 'a -> Session.t * Lsp_.Reply.t option
(** [command_handler session state] records the fact that a fix was applied in the state *)
