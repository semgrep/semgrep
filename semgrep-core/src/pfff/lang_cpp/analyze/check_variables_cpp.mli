
(* Use/def variables related checks, e.g.
 *  - unused var,
 *  - use of undefined var
 *  - ...
 * Does some side effect on program to set the scope ref of variables.
 * Also does side effects on Error_cpp._errors
 *
 * update: can now pass a hook to find class definitions as
 * some access to variables can be legit if the superclass (defined
 * in another file) has defined those protected variables.
*)
val check_and_annotate_program:
  (*?find_entity: Ast_entity_php.entity_finder option -> *)
  Ast_cpp.program -> unit
