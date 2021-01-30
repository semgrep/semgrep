
type charpos = int

type t = {
  start: charpos;
  end_: charpos;
}

exception NotValidRange of string

(* included or equal *)
val ($<=$): t -> t -> bool
(* disjoint *)
val ($<>$): t -> t -> bool

val range_of_linecol_spec: string -> Common.filename -> t

val range_of_token_locations:
  Parse_info.token_location -> Parse_info.token_location -> t

val range_of_tokens: Parse_info.t list -> t option

(* Note that the file content is memoized, so multiple calls to
 * content_at_range will not read_file again and again the same file.
*)
val content_at_range: Common.filename -> t -> string
