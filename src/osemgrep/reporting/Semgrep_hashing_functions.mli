module Out = Semgrep_output_v1_t

val syntactic_id : Out.cli_match -> string
(** A unique key designed with notification user experience in mind.
    Results in fewer unique findings than core_unique_key.

    This uses the Murmur3 128 hash, and is used e.g. in Gitlab_sast and
    Gitlab_secrets output. *)

val match_based_id_partial :
  Rule.t -> Rule_ID.t -> Out.metavars option -> string -> string
(** The fingerprint used to uniquely identify a match. Since this is used by the
    backend, it is crucial to have identical output as in pysemgrep. *)

(* for unit testing *)
val match_formula_interpolated_str :
  Rule.t -> Semgrep_output_v1_t.metavars option -> string
