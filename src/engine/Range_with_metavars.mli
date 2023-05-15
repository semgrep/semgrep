(* Used in Match_rules to represent a range and all necessary information *)
(* to return a match *)

(* Since pattern-inside is different from just anding two patterns, *)
(* we use range_kind to distinguish whether this came from matching *)
(* a pattern-inside, normal pattern, or regexp *)
type range_kind = Plain | Inside | Regexp [@@deriving show]

type t = {
  r : Range.t;
  mvars : Metavariable.bindings;
  kind : range_kind;
  origin : Pattern_match.t;
}
[@@deriving show]

type ranges = t list [@@deriving show]

(* Functions *)

val match_result_to_range : Pattern_match.t -> t
val range_to_pattern_match_adjusted : Rule.rule -> t -> Pattern_match.t

(* Set functions *)

val intersect_ranges :
  Rule_options.t -> bool (* debug_matches *) -> ranges -> ranges -> ranges

val difference_ranges : Rule_options.t -> ranges -> ranges -> ranges
