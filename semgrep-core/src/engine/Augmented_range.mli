type range_kind = Plain | Inside | Regexp [@@deriving show]

(* range with metavars *)
type range_with_mvars = {
  r : Range.t;
  mvars : Metavariable.bindings;
  kind : range_kind;
  origin : Pattern_match.t;
}
[@@deriving show]

type ranges = range_with_mvars list [@@deriving show]

(* Functions *)

val match_result_to_range : Pattern_match.t -> range_with_mvars

val intersect_ranges :
  Config_semgrep_t.t -> bool (* debug_matches *) -> ranges -> ranges -> ranges

val difference_ranges : Config_semgrep_t.t -> ranges -> ranges -> ranges
