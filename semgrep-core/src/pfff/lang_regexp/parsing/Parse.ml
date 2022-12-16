(*
   Public entry point for parsing regexps.
*)

let from_lexbuf conf lexbuf =
  try
    Parser.main (Lexer.token conf) lexbuf
  with Parsing.Parse_error ->
    let tok = Parse_info.tokinfo lexbuf in
    raise (Parse_info.Parsing_error tok)

let channel conf ic =
  Lexing.from_channel ic
  |> from_lexbuf conf

let file ?(conf = Dialect.default_conf) path =
  let ic = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ic)
    (fun () -> channel conf ic)

let string ?(conf = Dialect.default_conf) s =
  Lexing.from_string s
  |> from_lexbuf conf

let parse = file
