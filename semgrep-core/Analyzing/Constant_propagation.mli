(* Works by side effect on the generic AST by modifying its refs.
 * We pass the lang because some constant propagation algorithm may be
 * specific to a language.
*)
(* !Note that this assumes Naming_AST.resolve has been called before! *)
val propagate_basic: Lang.t -> AST.program -> unit

val propagate_dataflow: AST.program -> unit
