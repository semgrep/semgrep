(* annotates the cfg with facts *)
val hook_annotate_facts : (IL.cfg -> unit) option Hook.t

(* checks if any of the facts satisfies the when condition (e) *)
val hook_facts_satisfy_e :
  (Metavariable.bindings -> AST_generic.facts -> AST_generic.expr -> bool)
  option
  Hook.t

(* TODO: Can't use Hook.ml yet as this is set in Pro_core_CLI.ml *)
val hook_path_sensitive : bool ref
val with_pro_hooks : (unit -> 'a) -> 'a
