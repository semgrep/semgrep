(** Quick (core) profiling data.

  This is summarized profiling data that is **always** tracked by Semgrep,
  so it needs to be quick to obtain and cheap to keep around. This is in
  contrast with 'Core_profiling', that tracks rather fine-grained data (e.g.
  parsing time for every single file), and it can be expensive for very large
  repos, so it is only enabled with `-json_time` (or `--time` from the CLI).

  In `Core_result` we have both `Core_quick_profiling.t` as well as
  `Core_profiling.t`, and we combine them into Semgrep's output `time` field
  when generating the JSON output.

 *)

module Parsing_stats : Summary_stats.S with type key = Fpath.t
(** Parsing time per-file. *)

module Scanning_stats : Summary_stats.S with type key = Fpath.t
(** Scanning time per-file. For CE and `--pro-intrafile` this includes
  parsing time; it's just simpler that way given that the file is parsed
  lazily by the 'target_handler'. For `--pro` this does not include
  parsing time. *)

module Matching_stats : Summary_stats.S with type key = Fpath.t * Rule_ID.t
(** Matching time per-file and per-rule. Included in the "scanning time";
  it shows the cost of matching a rule against a file. This also includes
  the matching performed as part of evaluating a taint rule, see
  'Match_taint_spec.taint_config_of_rule'. *)

module Tainting_stats :
  Summary_stats.S with type key = Fpath.t * Pos.t * Rule_ID.t
(** Tainting time per-def and per-rule. Included in the "scanning time";
  it shows the cost of running the dataflow analysis. To respect our
  privacy policy, we only record the position of the definition within
  the file, but we do not record its name.

  This also includes the analysis of object initializers and the analysis of
  the top-level statements of a file. *)

module Prefiltering_stats : sig
  type t [@@deriving show]

  val zero : t
  val combine : t -> t -> t
  val to_ratio_stats : t -> Semgrep_output_v1_t.prefiltering_stats
end

type t = {
  parsing_stats : Parsing_stats.t;
  scanning_stats : Scanning_stats.t;
  matching_stats : Matching_stats.t;
  tainting_stats : Tainting_stats.t;
  prefiltering_stats : Prefiltering_stats.t;
}
[@@deriving show]

val zero : t
val combine : t -> t -> t
val combine_opt : t option -> t option -> t option
val map_opt : (t -> 'a) -> t option -> 'a option
val add_parse_time : Fpath.t -> float -> t -> t

val add_run_time : Fpath.t -> float -> t -> t
(** Add the scanning time for a file. *)

val add_match_time : Fpath.t -> Rule_ID.t -> float -> t -> t
(** Add the matching time for a file and rule. *)

val add_taint_time : Fpath.t -> Pos.t -> Rule_ID.t -> float -> t -> t
(** Add the tainting time for a definition and rule. *)

val add_project_level_time : float -> t -> t
val add_file_level_time : float -> t -> t
val add_rules : t -> int -> t
val add_rules_with_project_prefilters : t -> int -> t
val add_rules_with_file_prefilters : t -> int -> t
val add_rules_selected : t -> int -> t
val add_rules_matched : t -> int -> t
