(* see https://en.wikipedia.org/wiki/Whitespace_character#Unicode
 * ocamlc unicode.ml; ./a.out > out; emacsclient out and switch to
 * M-x hexl-mode to see the hexadecimal byte values.
*)

let _ =
  let s="1:\u{0009}\u{000A}\u{000B}\u{000C}\u{000D}\u{0020}\u{0085}\u{00A0}"in
  print_string s;
  let s="2:\u{FEFF}" in
  print_string s;
  let s="3:\u{1680}\u{2000}\u{2001}\u{2002}\u{2003}\u{2004}\u{2005}" in
  print_string s;

  (* 09 0D 0A 20 *)
  let s="4:\t\r\n " in
  print_string s;

  let s="5:\u{21D2}" in
  print_string s;
  ()
