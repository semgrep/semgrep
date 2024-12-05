val capabilities : Lsp.Types.ServerCapabilities.t
(** The capabilities of the server. This is used to inform the client of what
    the server can do. Exposed for testing *)

val start : < Session.caps ; .. > -> unit Lwt.t
(** Entry point of the language server. This will start the server, and
    communicate over stdin/out using the Language Server Protocol *)
