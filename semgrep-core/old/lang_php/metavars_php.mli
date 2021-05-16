type mvar = string

type metavars_binding = (mvar, Cst_php.any) Common.assoc

val is_metavar_name : string -> bool

val is_metavar_variable_name : string -> bool

val is_metavar_manyargs_name : string -> bool

val metavar_regexp_string : string

val metavar_variable_regexp_string : string

(* will throw a Failure if the pattern contains some legacy code
 * (e.g. the old lvalue metavariables $V)
 *)
val check_pattern : Cst_php.any -> Cst_php.any
