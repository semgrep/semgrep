(*
   Text highlighting
*)

open Printf

type color = Default | Red | Green | Yellow | Cyan | Bold | Faint

let ansi_code_of_style = function
  | Default -> "0"
  | Red -> "31"
  | Green -> "32"
  | Yellow -> "33"
  | Cyan -> "36"
  | Bold -> "1"
  | Faint -> "2"

let color color str =
  Printf.sprintf "\027[%sm%s\027[%sm" (ansi_code_of_style color) str
    (ansi_code_of_style Default)

(* Remove formatting. It's a bit ugly but convenient to
   compute the length of a sequence of characters. *)
let strip =
  let re = Re.Pcre.regexp "\027\\[[0-9]+m" in
  fun str -> Re.replace re ~f:(fun _ -> "") str

(*
   Return the number of code points assuming UTF-8-compatible encoding
   (and no surrogate pairs i.e. WTF-8).
*)
let utf8_length str =
  let count = ref 0 in
  String.iter
    (fun c ->
      if Char.code c <= 0b01111111 || Char.code c >= 0b11000000 then incr count)
    str;
  !count

(*
   The physical length of a rendered string assuming UTF-8 encoding,
   one character per UTF-8 code point, and a fixed width of 1 per character.
   The result will be wrong if the string contains control characters such
   as newlines.
*)
let graph_length str = strip str |> utf8_length

let pad str min_len =
  let len = graph_length str in
  if len >= min_len then str else str ^ String.make (min_len - len) ' '

let left_col text = pad text 8

(* Same as String.make but repeats a multi-byte unit rather than a byte *)
let string_make len unit =
  let buf = Buffer.create (len * String.length unit) in
  for _ = 1 to len do
    Buffer.add_string buf unit
  done;
  Buffer.contents buf

let term_width = 80

let horizontal_line =
  let res = (string_make term_width "─" |> color Faint) ^ "\n" in
  fun () -> res

let frame str =
  let padded_str = pad str (term_width - 4) in
  let horizontal_line =
    string_make (String.length (strip padded_str) + 2) "─"
  in
  let top_line = sprintf "┌%s┐\n" horizontal_line |> color Faint in
  let bottom_line = sprintf "└%s┘\n" horizontal_line |> color Faint in
  let contents_line =
    sprintf "%s %s %s\n" (color Faint "│") padded_str (color Faint "│")
  in
  top_line ^ contents_line ^ bottom_line

(*
   Add a trailing newline and indent each line.
*)
let quote_multiline_text =
  (* Indent by one space, similarly to 'diff -u' output *)
  let margin = " " in
  fun str ->
    str |> String.split_on_char '\n'
    |> Helpers.list_map (fun line -> margin ^ line ^ "\n")
    |> String.concat ""
