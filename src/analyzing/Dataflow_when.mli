(* annotates the cfg with facts *)
val hook_annotate_facts : (IL.cfg -> unit) option ref

(* checks if any of the facts satisfies the when condition (e) *)
val hook_facts_satisfy_e :
  (Metavariable.bindings -> AST_generic.facts -> AST_generic.expr -> bool)
  option
  ref

val hook_path_sensitive : bool ref
val with_pro_hooks : (unit -> 'a) -> 'a
