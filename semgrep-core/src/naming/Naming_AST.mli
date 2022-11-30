(* Returns true if the string denotes the name of an Angular decorator in
   Javascript.
   This is exposed for DeepSemgrep, which needs to do some naming work for
   particular decorated class definitions, to not duplicate logic.
*)
val is_js_angular_decorator : string -> bool

(* Works by side effect on the generic AST by modifying its refs.
 * We pass the lang because some name resolution algorithm may be
 * specific to a language.
 *)
val resolve : Lang.t -> AST_generic.program -> unit
