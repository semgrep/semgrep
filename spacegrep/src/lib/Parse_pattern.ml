(*
   Parse a pattern.
*)

let of_lexbuf lexbuf =
  Lexer.pattern lexbuf

let of_string s =
  let lexbuf = Lexing.from_string s in
  of_lexbuf lexbuf

let of_channel ic =
  let lexbuf = Lexing.from_channel ic in
  of_lexbuf lexbuf

let of_stdin () = of_channel stdin

let of_file file =
  let ic = open_in file in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ic)
    (fun () -> of_channel ic)
