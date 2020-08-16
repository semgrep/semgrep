{
type token =
  | Atom of Pattern_AST.atom
  | Dots
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
let capitalized_word = ['A'-'Z']['A'-'Z' 'a'-'z' '0'-'9' '_']*
let punct = [
  '!' '"' '#' '$' '%' '&' '\'' '*' '+' ',' '-' '.' '/' ':' ';' '<' '=' '>'
  '?' '@' '\\' '^' '`' '|' '~'
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
  | "...." '.'* as s {
      List.init
        (String.length s)
        (fun _ -> Atom (Punct '.'))
      @ tokens lexbuf
    }
  | "..." { Dots :: tokens lexbuf }
  | '$' (capitalized_word as s) { Atom (Metavar s) :: tokens lexbuf }
  | punct as c { Atom (Punct c) :: tokens lexbuf }
  | newline { [] }
  | _ as c { Atom (Byte c) :: tokens lexbuf }
  | eof { [] }
