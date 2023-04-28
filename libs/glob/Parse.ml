let parse_string str =
  let lexbuf = Lexing.from_string str in
  Parser.segments Lexer.token lexbuf
