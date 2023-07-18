type t = AST_generic.stmt list

val extend : AST_generic.stmt -> t -> t
val location : t -> (Tok.location * Tok.location) option
val list_original_tokens : t -> Tok.t list
