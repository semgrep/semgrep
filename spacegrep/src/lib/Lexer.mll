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
  | word as s { let loc = loc lexbuf in Atom (loc, Word s) :: tokens lexbuf }

  | '(' { let loc = loc lexbuf in Open_paren loc :: tokens lexbuf }
  | ')' { let loc = loc lexbuf in Close_paren loc :: tokens lexbuf }
  | '[' { let loc = loc lexbuf in Open_bracket loc :: tokens lexbuf }
  | ']' { let loc = loc lexbuf in Close_bracket loc :: tokens lexbuf }
  | '{' { let loc = loc lexbuf in Open_curly loc :: tokens lexbuf }
  | '}' { let loc = loc lexbuf in Close_curly loc :: tokens lexbuf }
  | "...." '.'* as s {
      let pos0 = Lexing.lexeme_start_p lexbuf in
      List.init
        (String.length s)
        (fun i ->
           let loc = (Loc.Pos.shift pos0 i, Loc.Pos.shift pos0 (i + 1)) in
           Atom (loc, Punct '.'))
      @ tokens lexbuf
    }
  | "..." { let loc = loc lexbuf in Dots loc :: tokens lexbuf }
  | '$' (capitalized_word as s) {
      let loc = loc lexbuf in
      Atom (loc, Metavar s) :: tokens lexbuf
    }
  | punct as c { let loc = loc lexbuf in Atom (loc, Punct c) :: tokens lexbuf }
  | newline { Lexing.new_line lexbuf; [] }
  | _ as c { let loc = loc lexbuf in Atom (loc, Byte c) :: tokens lexbuf }
  | eof { [] }
