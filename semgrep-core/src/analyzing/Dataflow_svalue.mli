type mapping = AST_generic.svalue Dataflow_core.mapping

val fixpoint : Lang.t -> IL.name list -> IL.cfg -> mapping
(** Flow-sensitive constant-propagation.
 * !Note that this assumes Naming_AST.resolve has been called before!
*)

val update_svalue : IL.cfg -> mapping -> unit
(**
 * Updates the [IL.lval.svalue] refs according to the mapping.
 * Note that the svalue refs in IL are shared with the Generic AST, so
 * running this analysis also updates svalue info in the Generic AST.
 * The update respects previous constant propagation passes, updating
 * svalue info when we have deduced more specific facts, but leaving it
 * untouched otherwise.
*)
