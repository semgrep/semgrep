(*
   Parse a document.
*)

val of_lexbuf : Lexing.lexbuf -> Doc_AST.t

val of_src : Src_file.t -> Doc_AST.t
