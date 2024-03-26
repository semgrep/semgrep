type pro_hook_ref

val pro_hooks_refs : pro_hook_ref list

(* Saves current Pro hooks, and temporarily resets them for running a function.
 * When the function has completed, it restores the saved Pro hooks. This
 * confines the reach of the "reset", making it easier to reason about, and
 * causing fewer surprises, thus it is preferred over 'reset_pro_hooks' which
 * has the opposite properties.
 *)
val save_pro_hooks_and_reset : (unit -> 'a) -> 'a

(* DO NOT USE! use save_pro_hooks_and_reset() instead. *)
val reset_pro_hooks : unit -> unit
