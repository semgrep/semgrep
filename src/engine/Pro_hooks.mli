(* Saves current Pro hooks, and temporarily resets them for running a function.
 * When the function has completed, it restores the saved Pro hooks. This
 * confines the reach of the "reset", making it easier to reason about, and
 * causing fewer surprises.
 *)
val save_pro_hooks_and_reset : (unit -> 'a) -> 'a
