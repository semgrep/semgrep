(*s: namespace_php.mli *)

type nameS = 
  | NameS of string
  | NameQualifiedS of string * string

type dnameS = 
  | DNameS of string (* without the dollar *)


val name_to_nameS_wrap: Cst_php.name -> nameS Cst_php.wrap

val dnameS_of_dname: Cst_php.dname -> dnameS

val nameS: nameS -> string
val dnameS: dnameS -> string
(*x: namespace_php.mli *)
(*e: namespace_php.mli *)
