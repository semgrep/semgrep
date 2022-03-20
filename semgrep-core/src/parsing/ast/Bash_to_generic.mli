(*
   Convert Bash-specific AST to generic AST.
*)

(*
   Convert a target program to the generic AST.
   May raise AST_generic.Error.
*)
val program : AST_bash.input_kind -> AST_bash.program -> AST_generic.program
val any : AST_bash.input_kind -> AST_bash.program -> AST_generic.any
