
type mapping = AST_generic.constness Dataflow.mapping

val string_of_constness : AST_generic.constness -> string

(** Flow-sensitive constant-propagation.
 * !Note that this assumes Naming_AST.resolve has been called before!
*)
val fixpoint : IL.cfg -> mapping

(**
 * Updates the [IL.lval.constness] refs according to the mapping.
 * Note that the constness refs in IL are shared with the Generic AST, so
 * running this analysis also updates constness info in the Generic AST.
 * The update respects previous constant propagation passes, updating
 * constness info when we have deduced more specific facts, but leaving it
 * untouched otherwise.
*)
val update_constness : IL.cfg -> mapping -> unit
