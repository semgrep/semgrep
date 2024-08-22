(** Collect information about the project contributions from git log. *)

val get_contributions : < Cap.exec > -> Semgrep_output_v1_j.contribution list
