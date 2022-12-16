
type t =
  | Global | Local  | Param | Static | Class

  | LocalExn | LocalIterator
  | ListBinded
  | Closed

  | NoScope

val string_of_scope: t -> string
val vof_scope: t -> OCaml.v
