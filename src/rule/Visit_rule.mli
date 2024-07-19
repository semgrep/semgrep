val visit_formula : (Rule.formula -> unit) -> Rule.formula -> unit

val visit_xpatterns :
  (Xpattern.t -> inside:bool -> unit) -> Rule.formula -> unit

val xpatterns_of_rule : Rule.t -> Xpattern.t list
