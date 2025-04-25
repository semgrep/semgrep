(*
   Gather and print system resources available to semgrep.
*)

type t = {
  cpu : Num_jobs.t;
      (** Recommendations derived from CPU info and cgroup quotas *)
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
