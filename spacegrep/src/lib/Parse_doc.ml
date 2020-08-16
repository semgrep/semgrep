(*
   Parse a document.
*)

let of_lexbuf lexbuf =
  Parse_pattern.of_lexbuf ~is_doc:true lexbuf
  |> Doc_AST.of_pattern

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
