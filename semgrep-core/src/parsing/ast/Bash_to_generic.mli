(*
   Convert Bash-specific AST to generic AST.
*)

(* may raise AST_generic.Error *)
val program : AST_bash.program -> AST_generic.program

val any : AST_bash.program -> AST_generic.any
