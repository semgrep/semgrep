(*
   PCRE-related code used both for parsing patterns and for scanning targets.
*)

open Printf

let char_class_of_list chars =
  let buf = Buffer.create 100 in
  Buffer.add_char buf '[';
  List.iter
    (fun c ->
      match c with
      | '-'
      | '^'
      | '['
      | ']' ->
          bprintf buf {|\x%02X|} (Char.code c)
      | ' ' .. '~' -> Buffer.add_char buf c
      | _ -> bprintf buf {|\x%02X|} (Char.code c))
    chars;
  Buffer.add_char buf ']';
  Buffer.contents buf

(*
   Escape whitespace characters and comment delimiters as needed for
   the extended mode in addition to other special characters.
*)
let quote =
  let rex = SPcre.regexp "[[:space:]#]" in
  let subst str =
    assert (String.length str = 1);
    (* escape all characters so that it works regardless of whether they're
       in a character class ([x]) or not. *)
    match str.[0] with
    | '\n' -> {|\n|}
    | '\t' -> {|\t|}
    | c (* other whitespace or '#' *) -> sprintf {|\x%02X|} (Char.code c)
  in
  fun str -> Pcre.quote str |> Pcre.substitute ~rex ~subst
