(* limited to expressions for now (AST_Generic.E case) *)
val pattern_to_string : Lang.t -> Pattern.t -> string

(* old:
(* limited to expressions for now.
 *
 * Note that it's ok if the expression contains a metavariable not present
 * in the binding; that just means we actually want to pretty-print a pattern
 * in which case we should just pretty print the metavariable as is.
 * Passing the binding is useful only for the autofixing, for synthesizing
 * the binding should always be empty.
 *)
val expr_to_string :
  Lang.t -> Metavariable.bindings -> AST_generic.expr -> string
 *)
