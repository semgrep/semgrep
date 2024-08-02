val run_core_search :
  Match_env.xconfig ->
  Rule.search_rule ->
  Fpath.t ->
  Core_result.processed_match list option
(** [run_core_search] runs a search intended for the /semgrep/search IDE
    search command, by hooking lower into the Match_search_mode matching
    process, bypassing the CLI.
  *)

val scan_workspace : Session.t -> Lsp_.Reply.t
(** [scan_workspace server] scans the workspace of the given session. *)

val scan_open_documents : Session.t -> Lsp_.Reply.t
(** [scan_open_documents server] scans the open documents of the given session. *)

val scan_file : Session.t -> Lsp__Uri0.t -> Lsp_.Reply.t
(** [scan_file server] scans the given file. If [content] is provided, it will
  * be used as the content of the file. Otherwise, the content will be read
  * from the file system.
  *)

val refresh_rules : Session.t -> Lsp_.Reply.t
(** [refresh_rules server] refreshes the rules of the given session. *)
