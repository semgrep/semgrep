(*
   Convert Dockerfile-specific AST to generic AST.

   This relies on Bash_to_generic to convert the bash
   constructs inside the Dockerfile.
*)

(* may raise AST_generic.Error *)
val program : AST_dockerfile.program -> AST_generic.program
val any : AST_dockerfile.program -> AST_generic.any
