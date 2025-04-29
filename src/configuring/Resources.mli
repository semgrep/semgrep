(*
   Gather and print system resources available to semgrep.
*)

type t = {
  num_jobs : int;
      (** Number of parallel jobs recommended to run semgrep.
        This is the default value for '-j'. *)
}

val resources : t
(** Resources record obtained by inspecting system resources at
    module initialization time. *)

val show : unit -> string
(** Text output showing resources (CPU, memory, ...) for the
    'semgrep show resources' subcommand. *)

val to_json : unit -> string
(** JSON output showing resources (CPU, memory, ...) for the
    'semgrep show resources --json' subcommand. *)
