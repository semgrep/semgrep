(*s: checking_php.mli *)

(* Mainly checking the arity of function calls. Does a function call matches
 * the function prototype "signature". Handle also static method calls
 * as it is easy to determine statically to what they correspond. Also
 * handle method calls using $this-> as it's also easy to determine statically
 * to what they correspond.
 * 
 * pre: program without self/parent
 * 
 * todo: not only check arity but types ...
 *)
val check_program: 
  Entity_php.entity_finder -> Cst_php.program -> unit

(* used also by check_classes_php.ml to check new Xxx() calls *)
val check_args_vs_params:
  (Cst_php.name * Cst_php.argument list) ->
  (Cst_php.ident * Cst_php.parameter list) ->
  unit
val contain_func_name_args_like: Cst_php.any -> bool
(*x: checking_php.mli *)
(*e: checking_php.mli *)
