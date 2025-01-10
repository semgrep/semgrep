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

(* constants *)
module Attributes : sig
  val semgrep_managed_scan : string
  val engine : string
  val repo_name : string
  val jobs : string
  val job : string
  val folder : string
  val pro_secrets_validators : string
  val pro_historical_scanning : string
  val pro_deep_intrafile : string
  val pro_deep_interfile : string
  val pro_secrets_allowed_origins : string
end

(* Helpers *)

val no_analysis_features : unit -> analysis_flags
(** For analysis run with the oss engine, we know all the flags will be false *)

val data_of_languages : Analyzer.t list -> (string * Tracing.user_data) list
(** Convenience function to turn a list of interfile languages into otel data *)

val get_resource_attrs :
  ?env:string ->
  engine:string ->
  analysis_flags:analysis_flags ->
  jobs:int ->
  unit ->
  (string * Tracing.user_data) list
(** [get_resource_data ~engine:"oss" ~env:"prod" ~analysis_flags () ] creates
    tags for the resource we report traces to. This is essentially info about
    the "service" itself, that is immutable once the service/program starts.
    This data is usually useful for grouping large sets of
    logs/traces/errors/metrics and discovering or investigating other macro
    trends about Semgrep. Example: Service Version, OCaml runtime version,
    telemetry sdk version. See module commentary for more info

    Other data besides what's passed in as flags to this function may be
    gathered from the environment such as Semgrep's version number.

    [engine] is the engine we are using, e.g. "oss" or "pro"

    [env] is the environment we are working in ("prod","dev2" etc.). Defaults to
    "prod"

    [analysis_flags] see {!analysis_flags}
  *)
