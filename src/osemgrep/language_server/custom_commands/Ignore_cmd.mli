val command : string
(** command name *)

type t = { path : string; fingerprint : string } [@@deriving yojson]
(** which finding to ignore*)

val create : path:string -> fingerprint:string -> Lsp.Types.Command.t
(** [create ~path ~fingerprint] creates a [Command.t] command to ignore a finding at [path] with [fingerprint] *)

val command_handler : RPC_server.t -> Yojson.Safe.t list -> RPC_server.t
(** [command_handler rpc_server params] handles the ignore finding command *)
