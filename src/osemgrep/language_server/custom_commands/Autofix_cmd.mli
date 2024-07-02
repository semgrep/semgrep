val command : string

val create : unit -> Lsp.Types.Command.t
(** [create ()] creates a new command that will let the LS know a fix was applied*)

val command_handler : RPC_server.t -> 'a -> RPC_server.t
(** [command_handler rpc_server state] records the fact that a fix was applied in the state *)
