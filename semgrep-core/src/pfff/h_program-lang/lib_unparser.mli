
type elt =
  | OrigElt of string
  | Removed of string
  | Added of string
  | Esthet2 of (Parse_info.esthet * string)

(* helpers *)
val elts_of_any:
  (Parse_info.token_kind * Parse_info.t) list -> elt list

(* debugging *)
val vof_elt: elt -> OCaml.v

(* heuristics *)
val drop_esthet_between_removed: elt list -> elt list
val drop_whole_line_if_only_removed: elt list -> elt list

val debug: bool ref

(* main entry point *)
val string_of_toks_using_transfo:
  (Parse_info.token_kind * Parse_info.t) list -> string
