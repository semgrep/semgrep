(* coupling: copied from Eval_generic so the metavariable values can
   be used here *)
type value = Eval_generic_partial.value

(* annotates the cfg with facts *)
val annotate_facts : IL.cfg -> unit

(* checks if any of the facts satisfies the when condition (e) *)
val facts_satisfy_e :
  (Metavariable.mvar, value) Hashtbl.t ->
  AST_generic.facts ->
  AST_generic.expr ->
  bool
