(** Helpers to prepare data for Opentelemetry tracing *)

(* Types *)

type analysis_flags = {
  secrets_validators : bool;  (** True when secrets validators are enabled *)
  allow_all_origins : bool;
      (** True when secrets validators from any origin may be used. This value
          is discarded if secrets_validators is false *)
  historical_scan : bool;  (** True when historical scans are enabled *)
  deep_intra_file : bool;
      (** True when deep intrafile scans (aka interproc taint) is enabled *)
  deep_inter_file : bool;
      (** True when interfile scans are enabled. Only one of `deep_inter_file`
         and `deep_intra_file` should be true. *)
}
[@@derving show]

(* Helpers *)

val no_analysis_features : unit -> analysis_flags
(** For analysis run with the oss engine, we know all the flags will be false *)

val get_top_level_data :
  string -> analysis_flags -> (string * Tracing.user_data) list
(** Create the tags for the top level span. These tags make it easy to see
    the traces we care about *)
