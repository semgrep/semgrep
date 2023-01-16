(*
   A crude heuristic to determine if a file is human-readable.
*)

type t =
  | Text (* looks like source code *)
  | Short (* text of short length that doesn't look like source code *)
  | Minified (* looks like source code from which whitespace was removed *)
  | Binary (* doesn't look like source code *)

let to_string = function
  | Text -> "text"
  | Short -> "short"
  | Minified -> "minified"
  | Binary -> "binary"

(* A Short file has at most than many bytes.

   The goal is to allow spacegrep to process short text files even
   if they contain no newlines and no indentation. The problem with
   files that have no 2D layout is that spacegrep can be slow to process them
   and it's wasteful because they're not human-readable source code anyway.
*)
let max_short_length = 500

let classify src =
  let contents = Src_file.contents src in
  let length = String.length contents in
  let num_lines, ascii_printable, ascii_not_printable =
    let newlines = ref 0 in
    let ascii_printable = ref 0 in
    let ascii_not_printable = ref 0 in
    String.iter
      (function
        (* LF *)
        | '\n' ->
            incr newlines;
            incr ascii_printable
        (* ascii and printable, other than LF *)
        | '\t'
        | '\r'
        | ' ' .. '~' ->
            incr ascii_printable
        (* non-ascii *)
        | '\x80' .. '\xff' -> ()
        (* ascii and not printable *)
        | '\x00' .. '\x08'
        | '\x0b' .. '\x0c'
        | '\x0e' .. '\x1f'
        | '\x7f' ->
            incr ascii_not_printable)
      contents;
    let num_lines = !newlines + 1 in
    (num_lines, !ascii_printable, !ascii_not_printable)
  in

  (* If the average line is longer than 150, it's considered not
     human-readable.
     For random bytes, the average line length is 256.
     For minified source code, there may be not a single newline in the file.
  *)
  let has_enough_lines =
    length = 0 || float num_lines /. float length >= 1. /. 150.
  in

  (* We don't care how many non-ascii bytes there are. *)
  let is_binary =
    length > 0
    && float ascii_not_printable /. float (ascii_printable + ascii_not_printable)
       >= 0.01
  in

  if is_binary then Binary
  else if has_enough_lines then Text
  else if length <= max_short_length then Short
  else Minified
