(** Profiling (summary) statistics *)

type 'key time = { key : 'key; time : float } [@@deriving show]

val to_file_time : Fpath.t time -> Semgrep_output_v1_t.file_time

val to_file_rule_time :
  (Fpath.t * Rule_ID.t) time -> Semgrep_output_v1_t.file_rule_time

val to_def_rule_time :
  (Fpath.t * Pos.t * Rule_ID.t) time -> Semgrep_output_v1_t.def_rule_time

type 'key stats = {
  count : int;
  sum : float;
  mean : float;
  m2 : float;
  very_slow_count : int;
  very_slow_sum : float;
  very_slow : 'key time list;
}
[@@deriving show]

module type Key = sig
  type t [@@deriving show]

  val very_slow_threshold : float
  (** The processing time threshold (in seconds) to consider a file/rule/etc
      as "very slow".  *)

  val very_slow_top_size : int
  (** From all the "very slow" files/rules/etc, we report the "top N" ones.
    This number should be fairly small, since the functions that handle the
    lists of "very slow" things are not optimized to handle too many elements. *)
end

module type S = sig
  type key
  type t = key stats [@@deriving show]

  val zero : t
  val update : t -> key -> float -> t
  val combine : t -> t -> t

  val to_output_v1 :
    to_out_time:(key time -> 'out_time) ->
    key stats ->
    Semgrep_output_v1_t.summary_stats
    * Semgrep_output_v1_t.very_slow_stats
    * 'out_time list
end

module Make (Key : Key) : S with type key = Key.t
