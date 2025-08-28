(*
   Copyright (c) 2021-2024 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
(* Used in Match_rules to represent a range and all necessary information *)
(* to return a match *)

(* Since pattern-inside is different from just anding two patterns, *)
(* we use range_kind to distinguish whether this came from matching *)
(* a pattern-inside, normal pattern, or regexp *)
type range_kind = Plain | Inside | Anywhere | Regexp [@@deriving show]

type t = {
  r : Range.t;
  mvars : Metavariable.bindings;
  kind : range_kind;
  origin : Core_match.t;
}
[@@deriving show]

type ranges = t list [@@deriving show]

(* Functions *)

val match_result_to_range : Core_match.t -> t
val range_to_pattern_match_adjusted : Rule.rule -> t -> Core_match.t

(* Set functions *)

val intersect_ranges :
  Rule_options.t -> debug_matches:bool -> ranges -> ranges -> ranges

val difference_ranges : Rule_options.t -> ranges -> ranges -> ranges
