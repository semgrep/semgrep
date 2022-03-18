(*
   This module is a hack for evaluating string literals.
*)

(*
   HACK!

   Remove enclosing single quotes or double quotes from a string, and
   assume all string literals of all languages are like this, which is wrong.

   TODO: apply the language's quoting and escaping rules rather than
   assuming they all use the same rules.
   TODO: evaluate escape sequences such as \n or \\
*)
val evaluate : string -> string
