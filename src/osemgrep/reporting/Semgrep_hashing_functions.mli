module OutT = Semgrep_output_v1_t

val ci_unique_key : ?index:int -> OutT.cli_match -> string
(** A unique key designed with notification user experience in mind.
    Results in fewer unique findings than cli_unique_key.

    This uses the Murmur3 128 hash, and is used e.g. in Gitlab_sast and
    Gitlab_secrets output. *)

val match_based_id_partial :
  Rule.t -> Rule_ID.t -> OutT.metavars option -> string -> string
(** The fingerprint used to uniquely identify a match. Since this is used by the
    backend, it is crucial to have identical output as in pysemgrep. *)
