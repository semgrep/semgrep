(*
   Copyright (c) 2024 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
type formula_matches = Range_with_metavars.t list * Matching_explanation.t list

type t
(**
   The type of the specialized formual cache used for inter-rule
   match sharing.
*)

val mk_specialized_formula_cache : Rule.taint_rule list -> t
(**
   These formula caches are only safe to use to share results between
   runs of rules on the same target!

   Right now they are only used by [Match_tainting_mode.taint_config_of_rule].
*)

val cached_find_opt :
  t -> Rule.formula -> get_matches:(unit -> formula_matches) -> formula_matches
