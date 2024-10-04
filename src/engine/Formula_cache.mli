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
