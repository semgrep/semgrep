
(* Use/Def variables-related checks, e.g.
 *  - unused var, 
 *  - use of undefined var (which in PHP also accounts for use before defined)
 * 
 * Does also some side effects on program to set the scope ref of variables.
 * Also does side effects on Error_php._errors. Dependent on Error_php.strict.
 * 
 * update: can now pass a hook to find functions/methods definitions
 * to remove false positives because of variables passed by reference.
 *)
val check_and_annotate_program: 
  Entity_php.entity_finder option -> Cst_php.program -> unit
