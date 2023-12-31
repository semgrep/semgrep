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

(*
   Print multiline text with an indentation. Always adds a trailing newline.

   Pros: disambiguates the quoted text from other output
   Cons: interferes with proper copy-pasting (since it inserts whitespace)
*)
val quote_multiline_text : string -> string
