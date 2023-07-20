(** Collect information about the project contributions from git log. *)

val get_git_logs : unit -> string list
val get_contributions : unit -> Semgrep_output_v1_j.contributions
