(*
   A representation of the input document, used to recover code snippets
   from locations.
*)

type t

val of_string : string -> t
val of_channel : in_channel -> t
val of_stdin : unit -> t
val of_file : string -> t

val to_lexbuf : t -> Lexing.lexbuf

(*
   Extract the lines containing a pair of positions.
   This includes the beginning of the first line and the end of the last line
   even if they're outside the requested range.
*)
val lines_of_pos_range : t -> Lexing.position -> Lexing.position -> string

(*
   Extract the lines containing a pair of locations.
   A location itself is a range of positions, typically associated with
   an input token.
*)
val lines_of_loc_range : t -> Loc.t -> Loc.t -> string
