(* Works by side effect on the generic AST by modifying its refs.
 * We pass the lang because some name resolution algorithm may be
 * specific to a language.
 *)
val resolve : Lang.t -> AST_generic.program -> unit

(* We expose this language-specific (and framework-specific!)
   function here because it is needed in Semgrep Pro.
   This lets us avoid having to duplicate the logic of this function.

   There's not really a better place to put this, so we'll just
   let it exist here.
*)
val is_js_angular_decorator : string -> bool
