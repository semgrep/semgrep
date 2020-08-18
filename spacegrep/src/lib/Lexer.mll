{
type token =
  | Atom of Loc.t * Pattern_AST.atom
  | Dots of Loc.t
  | Open_paren of Loc.t | Close_paren of Loc.t
  | Open_bracket of Loc.t | Close_bracket of Loc.t
  | Open_curly of Loc.t | Close_curly of Loc.t

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

let loc lexbuf : Loc.t =
  (Lexing.lexeme_start_p lexbuf, Lexing.lexeme_end_p lexbuf)
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
  | word as s { Atom (loc lexbuf, Word s) :: tokens lexbuf }

  | '(' { Open_paren (loc lexbuf) :: tokens lexbuf }
  | ')' { Close_paren (loc lexbuf) :: tokens lexbuf }
  | '[' { Open_bracket (loc lexbuf) :: tokens lexbuf }
  | ']' { Close_bracket (loc lexbuf) :: tokens lexbuf }
  | '{' { Open_curly (loc lexbuf) :: tokens lexbuf }
  | '}' { Close_curly (loc lexbuf) :: tokens lexbuf }
  | "...." '.'* as s {
      let pos0 = Lexing.lexeme_start_p lexbuf in
      List.init
        (String.length s)
        (fun i ->
           let loc = (Loc.Pos.shift pos0 i, Loc.Pos.shift pos0 (i + 1)) in
           Atom (loc, Punct '.'))
      @ tokens lexbuf
    }
  | "..." { Dots (loc lexbuf) :: tokens lexbuf }
  | '$' (capitalized_word as s) {
      Atom (loc lexbuf, Metavar s) :: tokens lexbuf
    }
  | punct as c { Atom (loc lexbuf, Punct c) :: tokens lexbuf }
  | newline { [] }
  | _ as c { Atom (loc lexbuf, Byte c) :: tokens lexbuf }
  | eof { [] }
