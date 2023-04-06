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
