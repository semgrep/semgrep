type span = {
  left_stmts : AST_generic.stmt list;
  right_stmts : AST_generic.stmt list;
}

type t = Empty | Span of span

val extend : AST_generic.stmt -> t -> t
val location : t -> (Tok.location * Tok.location) option
val list_original_tokens : t -> Tok.t list
