(* charpos is 0-indexed. First char of a file is at charpos:0
 * (unlike in Emacs where point starts at 1).
 *)
type charpos = int

(* The range is inclusive, {start = 0; end_ = 4} means [0..4] not [0..4[,
 * so the first range of the first character of the file will be
 * {start = 0; end_ = 0} (which also means there are no empty ranges).
 *)
type t = { start : charpos; end_ : charpos }

val pp : Format.formatter -> t -> unit

exception NotValidRange of string

(* included or equal *)
val ( $<=$ ) : t -> t -> bool

(* disjoint *)
val ( $<>$ ) : t -> t -> bool

val range_of_linecol_spec : string -> Common.filename -> t

val range_of_token_locations :
  Parse_info.token_location -> Parse_info.token_location -> t

val range_of_tokens : Parse_info.t list -> t option

(* Note that the file content is memoized, so multiple calls to
 * content_at_range will not read_file again and again the same file.
 *)
val content_at_range : Common.filename -> t -> string
