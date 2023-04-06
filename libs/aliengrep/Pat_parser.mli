(*
   Pattern parser.

   It doesn't use menhir of ocamlyacc because those tools work with
   a Lexing.lexbuf, which we don't use since we don't use ocamllex.
   The implementation isn't very hard since all we do is match braces.
*)

val parse : Pat_lexer.token list -> Pat_AST.t
