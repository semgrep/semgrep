(** Helpers to prepare data for Opentelemetry tracing *)

(* Types *)

type analysis_flags = {
  secrets_validators : bool;
  allow_all_origins : bool;
  historical_scan : bool;
  deep_intra_file : bool;
  deep_inter_file : bool;
}
[@@derving show]

(* Helpers *)

val no_analysis_features : unit -> analysis_flags

val get_top_level_data :
  string -> analysis_flags -> (string * Tracing.user_data) list
