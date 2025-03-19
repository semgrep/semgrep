val token : Lexing.lexbuf -> Parser.token
(** Lexes a single token from the given buffer.
    Can raise {! Syntax_error }. *)

exception Syntax_error of string
