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

val equal : t -> t -> bool
val compare : t -> t -> int

(* included or equal *)
val ( $<=$ ) : t -> t -> bool

(* included strictly*)
val ( $<$ ) : t -> t -> bool

(* disjoint *)
val ( $<>$ ) : t -> t -> bool

(* start_line - end_line -> filename -> range *)
val range_of_line_spec : string -> Fpath.t -> t

(* start_line:start_col - end_line:end_col -> filename -> range *)
val range_of_linecol_spec : string -> Fpath.t -> t
val range_of_token_locations : Tok.location -> Tok.location -> t
val range_of_tokens : Tok.t list -> t option

(* Note that the file content is memoized, so multiple calls to
 * content_at_range will not read_file again and again the same file.
 *)
val content_at_range : Fpath.t -> t -> string
