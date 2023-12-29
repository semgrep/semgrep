(*
   Text highlighting
*)

type color = Default | Red | Green | Yellow | Cyan | Bold | Faint

val color : color -> string -> string

(*
   Turn a string containing no newlines into a framed string.
   The result is LF-terminated.
*)
val frame : string -> string

(*
   Return an LF-terminated line.
*)
val horizontal_line : unit -> string

(*
   Pad text to be the correct width for the left column.
*)
val left_col : string -> string
