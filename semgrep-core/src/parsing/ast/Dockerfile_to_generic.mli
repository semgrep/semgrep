(*
   Convert Dockerfile-specific AST to generic AST.
*)

(* may raise AST_generic.Error *)
val program :
  AST_bash.input_kind -> AST_dockerfile.program -> AST_generic.program

val any : AST_bash.input_kind -> AST_dockerfile.program -> AST_generic.any
