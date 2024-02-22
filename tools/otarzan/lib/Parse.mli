(*
   Parse an ml file and extract the type definitions
*)
val extract_typedefs_from_ml_file :
  Fpath.t -> AST_ocaml.type_declaration list list

(* helpers used also in Test_otarzan.ml *)
val parse : Fpath.t -> AST_ocaml.program
