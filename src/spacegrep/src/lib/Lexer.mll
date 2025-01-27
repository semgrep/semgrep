{
type token =
  | Atom of Loc.t * Pattern_AST.atom
  | Dots of Loc.t * string option (* ... and $...MVAR *)
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

rule lines acc = parse
  | indent as s {
      let indent = indent_of_string s in
      let tokens = tokens [] lexbuf |> List.rev in
      if tokens <> [] then
        lines ({ indent; tokens } :: acc) lexbuf
      else
        lines acc lexbuf
    }
  | eof { acc }

and tokens acc = parse
  | blank { tokens acc lexbuf }
  | word as s { let loc = loc lexbuf in
                tokens (Atom (loc, Word s) :: acc) lexbuf }

  | '(' { let loc = loc lexbuf in tokens (Open_paren loc :: acc) lexbuf }
  | ')' { let loc = loc lexbuf in tokens (Close_paren loc :: acc) lexbuf }
  | '[' { let loc = loc lexbuf in tokens (Open_bracket loc :: acc) lexbuf }
  | ']' { let loc = loc lexbuf in tokens (Close_bracket loc :: acc) lexbuf }
  | '{' { let loc = loc lexbuf in tokens (Open_curly loc :: acc) lexbuf }
  | '}' { let loc = loc lexbuf in tokens (Close_curly loc :: acc) lexbuf }
  | "...." '.'* as s {
      let pos0 = Lexing.lexeme_start_p lexbuf in
      let elts =
        List.init
          (String.length s)
          (fun i ->
             let loc = (Loc.Pos.shift pos0 i, Loc.Pos.shift pos0 (i + 1)) in
             Atom (loc, Punct '.'))
      in
      tokens (List.rev_append elts acc) lexbuf
    }
  | "..." { let loc = loc lexbuf in
            tokens (Dots (loc, None) :: acc) lexbuf }
  | '$' "..." (capitalized_word as s) {
      let loc = loc lexbuf in
      tokens (Dots (loc, Some s) :: acc) lexbuf }
  | '$' (capitalized_word as s) {
      let loc = loc lexbuf in
      tokens (Atom (loc, Metavar s) :: acc) lexbuf
    }
  | punct as c { let loc = loc lexbuf in
                 tokens (Atom (loc, Punct c) :: acc) lexbuf }
  | newline { Lexing.new_line lexbuf; acc }
  | _ as c { let loc = loc lexbuf in
             tokens (Atom (loc, Byte c) :: acc) lexbuf }
  | eof { acc }

{
  let lines lexbuf =
    lines [] lexbuf |> List.rev
}
