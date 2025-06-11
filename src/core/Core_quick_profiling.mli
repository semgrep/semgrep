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

type t = { parsing_stats : Summary_stats.t } [@@deriving show]

val zero : t
val combine : t -> t -> t
val combine_opt : t option -> t option -> t option
val map_opt : (t -> 'a) -> t option -> 'a option
val add_parse_time : Fpath.t -> float -> t -> t
