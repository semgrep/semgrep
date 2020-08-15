{
type token =
  | Atom of AST.atom
  | Open_paren | Close_paren
  | Open_bracket | Close_bracket
  | Open_curly | Close_curly

type line = {
  indent: int; (* counts 1 per space, 8 per tab *)
  tokens: token list;
}

let indent_of_string s =
  let n = ref 0 in
  for i = 0 to String.length s - 1 do
    match s.[i] with
    | '\t' -> n := !n + 8
    | _ -> incr n
  done;
  !n
}

let indent = [' ' '\t']*
let blank = [' ' '\t']+
let newline = '\r'? '\n'
let word = ['A'-'Z' 'a'-'z' '0'-'9' '_']+
let punct = [
  '!' '"' '#' '$' '%' '&' '\'' '*' '+' ',' '-' '.' '/' ':' ';' '<' '=' '>'
  '?' '@' '\\' '^' '`' '|' '~'
]

let any_punct = [
  '!' '"' '#' '$' '%' '&' '\'' '*' '+' ',' '-' '.' '/' ':' ';' '<' '=' '>'
  '?' '@' '\\' '^' '`' '|' '~'
  '(' ')'
  '[' ']'
  '{' '}'
]

rule lines = parse
  | indent as s {
      let indent = indent_of_string s in
      let tokens = tokens lexbuf in
      if tokens <> [] then
        { indent; tokens } :: lines lexbuf
      else
        lines lexbuf
    }
  | eof { [] }

and tokens = parse
  | blank { tokens lexbuf }
  | word as s { Atom (Word s) :: tokens lexbuf }

  | '(' { Open_paren :: tokens lexbuf }
  | ')' { Close_paren :: tokens lexbuf }
  | '[' { Open_bracket :: tokens lexbuf }
  | ']' { Close_bracket :: tokens lexbuf }
  | '{' { Open_curly :: tokens lexbuf }
  | '}' { Close_curly :: tokens lexbuf }
  | punct as c { Atom (Punct c) :: tokens lexbuf }
  | newline { [] }
  | _ as c { Atom (Byte c) :: tokens lexbuf }
  | eof { [] }

and pattern = parse
  | blank { pattern lexbuf }
  | newline { pattern lexbuf }
  | "...." '.'* as s { List.init
                         (String.length s)
                         (fun _ -> Pattern_AST.Punct '.')
                       @ pattern lexbuf }
  | "..." { Pattern_AST.Dots :: pattern lexbuf }
  | "$"['A'-'Z']['A'-'Z' '0'-'9']* as s { Pattern_AST.Metavar s
                                          :: pattern lexbuf }
  | word as s { Pattern_AST.Word s :: pattern lexbuf }
  | any_punct as c { Pattern_AST.Punct c :: pattern lexbuf }
  | _ as c { Pattern_AST.Byte c :: pattern lexbuf }
  | eof { [] }
