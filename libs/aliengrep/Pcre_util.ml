(*
   PCRE-related code used both for parsing patterns and for scanning targets.
*)
[@@@alert "-deprecated"]

open Printf

(* This is used to print compact, readable character classes like [a-z]
   instead of [abcdefgh...xyz]. *)
type char_range = Single of char | Range of char * char

let close_range first_char last_char =
  if last_char <> first_char then Range (first_char, last_char)
  else Single first_char

let identify_char_ranges (chars : char list) : char_range list =
  let rec extend acc first_char prev_char chars =
    match chars with
    | [] -> close_range first_char prev_char :: acc |> List.rev
    | c :: chars ->
        if Char.code c = Char.code prev_char + 1 then
          extend acc first_char c chars
        else
          let acc = close_range first_char prev_char :: acc in
          extend acc c c chars
  in
  match List.sort Char.compare chars with
  | [] -> []
  | c :: chars -> extend [] c c chars

(* Escape a character so it can occur safely in a PCRE character class *)
let escape_char buf c =
  match c with
  | '-'
  | '^'
  | '['
  | ']' ->
      bprintf buf {|\x%02X|} (Char.code c)
  | ' ' .. '~' -> Buffer.add_char buf c
  | _ -> bprintf buf {|\x%02X|} (Char.code c)

let char_class_of_list ?(contents_only = false) chars =
  let buf = Buffer.create 100 in
  if not contents_only then Buffer.add_char buf '[';
  identify_char_ranges chars
  |> List.iter (function
       | Single c -> escape_char buf c
       | Range (first, last) ->
           escape_char buf first;
           Buffer.add_char buf '-';
           escape_char buf last);
  if not contents_only then Buffer.add_char buf ']';
  Buffer.contents buf

(*
   Escape whitespace characters and comment delimiters as needed for
   the extended mode in addition to other special characters.
*)
let quote =
  let rex = Pcre_.regexp "[[:space:]#]" in
  let subst str =
    assert (String.length str = 1);
    (* escape all characters so that it works regardless of whether they're
       in a character class ([x]) or not. *)
    match str.[0] with
    | '\n' -> {|\n|}
    | '\t' -> {|\t|}
    | c (* other whitespace or '#' *) -> sprintf {|\x%02X|} (Char.code c)
  in
  fun str -> Pcre_.quote str |> Pcre_.substitute ~rex ~subst
