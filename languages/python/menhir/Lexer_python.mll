{
(* Yoann Padioleau
 *
 * Copyright (C) 2010 Facebook
 * Copyright (C) 2011-2015 Tomohiro Matsuyama
 * Copyright (C) 2019 r2c
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file license.txt.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * license.txt for more details.
 *)
open Common

open Lexing

open Parser_python
module Flag = Flag_parsing_python

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* The Python lexer.
 *
 * original src:
 *  - https://github.com/m2ym/ocaml-pythonlib/blob/master/src/lexer.mll
 *  - some code stolen from pyre-check python lexer (itself started
 *    from ocaml-pythonlib)
 * reference:
 *    - http://docs.python.org/release/2.5.4/ref/ref.html
 * old src:
 *  - http://inst.eecs.berkeley.edu/~cs164/sp10/python-grammar.html
 *    which was itself from the python reference manual
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* shortcuts *)
let tok = Lexing.lexeme
let tokinfo = Tok.tok_of_lexbuf
let error = Parsing_error.lexical_error

let unescaped s =
  let buf = Buffer.create (String.length s) in
  let escape = ref false in
  let unescapechar c =
    if !escape then begin
      match c with
      | '\r' -> ()
      | '\n' -> escape := false
      | _ -> begin
          escape := false;
          (* TODO http://docs.python.org/reference/lexical_analysis.html#string-literals *)
          Buffer.add_char
            buf
            (match c with
             | '\\' -> '\\'
             | '\'' -> '\''
             | '"' -> '"'
             | 'a' -> Char.chr 7
             | 'b' -> '\b'
             | 'f' -> Char.chr 12
             | 'n' -> '\n'
             | 'r' -> '\r'
             | 't' -> '\t'
             | 'v' -> Char.chr 11
             | _ -> (Buffer.add_char buf '\\'; c))
        end
    end else if c =$= '\\' then
      escape := true
    else
      Buffer.add_char buf c
  in
    String.iter unescapechar s;
    Buffer.contents buf

(* ---------------------------------------------------------------------- *)
(* Lexer state *)
(* ---------------------------------------------------------------------- *)

(* this is to return space/comment tokens *)
type state_mode =
  | STATE_TOKEN
  | STATE_OFFSET
  | STATE_UNDERSCORE_TOKEN

  (* below the 'string' contains the prefix, e.g. "f", but
   * can also be "fr" in which case we must treat \ differently.
   *)
  | STATE_IN_FSTRING_SINGLE of string
  | STATE_IN_FSTRING_DOUBLE of string
  | STATE_IN_FSTRING_TRIPLE_SINGLE of string
  | STATE_IN_FSTRING_TRIPLE_DOUBLE of string

type lexer_state = {
  mutable curr_offset : int;
  offset_stack : int Stack.t;
  mutable nl_ignore : int;

  mode: state_mode list ref;
}

let create () =
  let stack = Stack.create () in
  Stack.push 0 stack;
  { curr_offset = 0;
    offset_stack = stack;
    nl_ignore = 0;
    mode = ref [STATE_TOKEN];
  }

let ignore_nl t =
  t.nl_ignore <- succ t.nl_ignore

and aware_nl t =
  t.nl_ignore <- pred t.nl_ignore

(* stack mode management *)
let top_mode state =
  match !(state.mode) with
  | [] -> failwith "Lexer_python.top_mode: empty stack"
  | x::_ -> x
let push_mode state mode = Stack_.push mode state.mode
let pop_mode state = ignore(Stack_.pop state.mode)
let set_mode state mode = begin pop_mode state; push_mode state mode end

let pr_mode mode = UCommon.pr2 (match mode with
  | STATE_TOKEN -> "token"
  | STATE_OFFSET -> "offset"
  | STATE_UNDERSCORE_TOKEN -> "_token"
  | STATE_IN_FSTRING_SINGLE _ -> "f'"
  | STATE_IN_FSTRING_DOUBLE _ -> "f\""
  | STATE_IN_FSTRING_TRIPLE_SINGLE _ -> "f'''"
  | STATE_IN_FSTRING_TRIPLE_DOUBLE _ -> "f\"\"\""
)

let pr_state state = List.iter pr_mode !(state.mode)

(* This used to be 8, but tests/python/parsing/eof_comment.py was not parsing.
 * This maybe should be a command-line parameter? Or we should fix
 * eof_comment.py in another way?
 *)
let space_per_tab = 8
}

(*****************************************************************************)
(* Regexp aliases *)
(*****************************************************************************)

(* epsilon *)
let e = ""

let newline = ('\n' | "\r\n" | '\x0C')
let whitespace = [' ' '\t']
let comment = '#' [^ '\n' '\r']*

let digit = ['0'-'9']
let octdigit = ['0'-'7']
(* python3-ext: underscore in numbers *)
let digipart = digit (('_'? digit)* )

let hexdigit = ['0'-'9' 'a'-'f' 'A'-'F']

let longintpostfix = ['l' 'L']

(* python3-ext: underscore in numbers (src: Pyre-check) *)
let integer =
  ('0' ['b' 'B'] ('_'? ['0'-'1'])+) | (* Binary. *)
  ('0' ['o' 'O'] ('_'? ['0'-'7'])+) | (* Octal. *)
  ('0' ['x' 'X'] ('_'? hexdigit)+) | (* Hexadecimal. *)
  (['1' - '9'] ('_'? digit)* | '0' ('_'? '0')* ) |  (* Decimal. *)
  ('0' digit+) (* Valid before python 3.6 *)


let intpart = digipart
let fraction = '.' digipart
let pointfloat = intpart? fraction | intpart '.'
let exponent = ['e' 'E'] ['+' '-']? digipart
let exponentfloat = (intpart | pointfloat) exponent
let floatnumber = pointfloat | exponentfloat

let imagnumber = (floatnumber | intpart) ['j' 'J']

let kind = 'b' | 'B'
let rawprefix = 'r' | 'R'
let encoding = 'u' | 'U' | rawprefix
(* (encoding encoding) for python2 legacy support *)
let stringprefix = (encoding | kind | (encoding kind) | (kind encoding) | (encoding encoding))?

(* Per https://www.python.org/dev/peps/pep-0498/, 'f' can be combined with 'r' but
 * not 'u' or 'b'
 *)
let fstringspecifier = 'f' | 'F'
let fstringprefix = fstringspecifier | (fstringspecifier rawprefix) | (rawprefix fstringspecifier)

let escapeseq = '\\' _
(* for raw fstring *)
let escapeseq2 = '\\' [^ '{']

(************************ UTF-8 boilerplate ************************)
(*
   Generic UTF-8 boilerplate.

   See https://erratique.ch/software/uucp/doc/unicode.html
   for a good explanation of how this works.

   We don't convert UTF-8-encoded data to code points. We only do the minimum
   to ensure the correct identification of the boundaries between scalar
   code points.
*)

(* 0xxxxxxx *)
let ascii = ['\000'-'\127']

(* 110xxxxx *)
let utf8_head_byte2 = ['\192'-'\223']

(* 1110xxxx *)
let utf8_head_byte3 = ['\224'-'\239']

(* 11110xxx *)
let utf8_head_byte4 = ['\240'-'\247']

(* 10xxxxxx *)
let utf8_tail_byte = ['\128'-'\191']

(* 7 bits of payload *)
let utf8_1 = ascii

(* 11 bits of payload *)
let utf8_2 = utf8_head_byte2 utf8_tail_byte

(* 16 bits of payload *)
let utf8_3 = utf8_head_byte3 utf8_tail_byte utf8_tail_byte

(* 21 bits of payload *)
let utf8_4 = utf8_head_byte4 utf8_tail_byte utf8_tail_byte utf8_tail_byte

(* Any UTF-8-encoded code point. This set includes more than it should
   for simplicity.

   - This includes encodings of the so-called surrogate code points
     used by UTF-16 and not permitted by UTF-8.
   - This includes the range 0x110000 to 0x1FFFFF which are beyond the
     range of valid Unicode code points.
*)
let utf8 = utf8_1 | utf8_2 | utf8_3 | utf8_4

let utf8_nonascii = utf8_2 | utf8_3 | utf8_4

(************************ end of UTF-8 boilerplate ************************)

(*
   https://www.python.org/dev/peps/pep-3131/ says:
   The identifier syntax is <XID_Start> <XID_Continue>*.

   TODO: use the correct character set for nonascii identifiers
   For now, we don't have an implementation of the Unicode character classes
   XID_Start and XID_Continue. We incorrectly assume that any nonascii
   code point is valid as part of an identifier. This should be fine
   as long as non-ascii characters aren't used for anything else than
   identifiers and quoted strings.
*)
let identifier =
  (* keeping the all-ascii case separate hoping it's faster this way *)
    ['a'-'z' 'A'-'Z' '_']['a'-'z' 'A'-'Z' '0'-'9' '_']*

  | (['a'-'z' 'A'-'Z' '_'] | utf8_nonascii)
    (['a'-'z' 'A'-'Z' '0'-'9' '_'] | utf8_nonascii)*

(*****************************************************************************)
(* Rule initial *)
(*****************************************************************************)

rule token python2 state = parse
  | e {
        let curr_offset = state.curr_offset in
        let last_offset = Stack.top state.offset_stack in
        match () with
        | _ when curr_offset < last_offset ->
            ignore (Stack.pop state.offset_stack);
            DEDENT (tokinfo lexbuf)
        | _ when curr_offset > last_offset ->
            Stack.push curr_offset state.offset_stack;
            INDENT (tokinfo lexbuf)
        (* curr_offset = last_offset *)
        | _ -> _token python2 state lexbuf
      }

(* this is just used to adjuste the state *)
and offset state = parse
  | e { "" }
  | ' '
     {
       state.curr_offset <- state.curr_offset + 1;
       " " ^ offset state lexbuf
     }
  | '\t'
     {
       state.curr_offset <- state.curr_offset + space_per_tab;
       "\t" ^ offset state lexbuf
     }

and _token python2 state = parse

  (* ----------------------------------------------------------------------- *)
  (* spacing/comments *)
  (* ----------------------------------------------------------------------- *)
  | ((whitespace* comment? newline)* whitespace* comment?) newline
      {
        let info = tokinfo lexbuf in
        if state.nl_ignore <= 0 then begin
          state.curr_offset <- 0;
          let s = offset state lexbuf in
          NEWLINE (Tok.tok_add_s s info)
        end else begin
         set_mode state STATE_UNDERSCORE_TOKEN;
         TCommentSpace info
        end
       }
  | '\\' newline whitespace*
      {
          let info = tokinfo lexbuf in
          let pos = lexbuf.lex_curr_p in
          lexbuf.lex_curr_p <-
            { pos with
                pos_bol = pos.pos_cnum;
                pos_lnum = pos.pos_lnum + 1 };
          set_mode state STATE_UNDERSCORE_TOKEN;
          TCommentSpace info
      }

  | whitespace+
      {
        let info = tokinfo lexbuf in
        set_mode state STATE_UNDERSCORE_TOKEN;
        TCommentSpace info
      }

  (* ----------------------------------------------------------------------- *)
  (* symbols *)
  (* ----------------------------------------------------------------------- *)
  (* symbols *)
  | ":="    { COLONEQ (tokinfo lexbuf) }
  | "+="    { ADDEQ (tokinfo lexbuf) }
  | "-="    { SUBEQ (tokinfo lexbuf) }
  | "*="    { MULTEQ (tokinfo lexbuf) }
  | "/="    { DIVEQ (tokinfo lexbuf) }
  | "%="    { MODEQ (tokinfo lexbuf) }
  | "**="   { POWEQ (tokinfo lexbuf) }
  | "//="   { FDIVEQ (tokinfo lexbuf) }
  | "&="    { ANDEQ (tokinfo lexbuf) }
  | "|="    { OREQ (tokinfo lexbuf) }
  | "^="    { XOREQ (tokinfo lexbuf) }
  | "<<="   { LSHEQ (tokinfo lexbuf) }
  | ">>="   { RSHEQ (tokinfo lexbuf) }

  | "=="    { EQUAL (tokinfo lexbuf) }
  | "!="    { NOTEQ (tokinfo lexbuf) }
  (* != equivalent to <>? *)
  | "<>"    { NOTEQ (tokinfo lexbuf) }
  | "<="    { LEQ (tokinfo lexbuf) }
  | ">="    { GEQ (tokinfo lexbuf) }
  | '<'     { LT (tokinfo lexbuf) }
  | '>'     { GT (tokinfo lexbuf) }

  | '='     { EQ (tokinfo lexbuf) }

  | "**"    { POW (tokinfo lexbuf) }
  | "//"    { FDIV (tokinfo lexbuf) }

  | '+'     { ADD (tokinfo lexbuf) }
  | '-'     { SUB (tokinfo lexbuf) }
  | '*'     { MULT (tokinfo lexbuf) }
  | '/'     { DIV (tokinfo lexbuf) }
  | '%'     { MOD (tokinfo lexbuf) }

  | '|'     { BITOR (tokinfo lexbuf) }
  | '&'     { BITAND (tokinfo lexbuf) }
  | '^'     { BITXOR (tokinfo lexbuf) }
  | '~'     { BITNOT (tokinfo lexbuf) }

  | "<<"    { LSHIFT (tokinfo lexbuf) }
  | ">>"    { RSHIFT (tokinfo lexbuf) }

  | '('     { ignore_nl state; LPAREN (tokinfo lexbuf) }
  | ')'     { aware_nl state; RPAREN (tokinfo lexbuf) }
  | '['     { ignore_nl state; LBRACK (tokinfo lexbuf) }
  | ']'     { aware_nl state; RBRACK (tokinfo lexbuf) }
  | '{'     {
      ignore_nl state;
      push_mode state STATE_UNDERSCORE_TOKEN;
      LBRACE (tokinfo lexbuf)
     }
  | '}'     {
      aware_nl state;
      pop_mode state;
      RBRACE (tokinfo lexbuf)
     }

  | ':'     { COLON (tokinfo lexbuf) }
  | ';'     { SEMICOL (tokinfo lexbuf) }
  | '.'     { DOT (tokinfo lexbuf) }
  | ','     { COMMA (tokinfo lexbuf) }
  | '`'     { BACKQUOTE (tokinfo lexbuf) }
  | '@'     { AT (tokinfo lexbuf) }
  (* part of python3 and also sgrep-ext: *)
  | "..."   { ELLIPSES (tokinfo lexbuf) }
  (* sgrep-ext: *)
  | "<..."  { Flag_parsing.sgrep_guard (LDots (tokinfo lexbuf)) }
  | "...>"  { Flag_parsing.sgrep_guard (RDots (tokinfo lexbuf)) }

  | "!" { if !(state.mode) |> List.exists (function
            | STATE_IN_FSTRING_SINGLE _
            | STATE_IN_FSTRING_DOUBLE _
            | STATE_IN_FSTRING_TRIPLE_SINGLE _
            | STATE_IN_FSTRING_TRIPLE_DOUBLE _ -> true
            | _ -> false)
          then BANG (tokinfo lexbuf)
          else begin
             error (spf "unrecognized symbols: %s" (tok lexbuf)) lexbuf;
             TUnknown (tokinfo lexbuf)
          end
      }

  (* ----------------------------------------------------------------------- *)
  (* Keywords and ident *)
  (* ----------------------------------------------------------------------- *)

  (* keywords *)
  | identifier as id
      { match id with

        | "if"       -> IF (tokinfo lexbuf)
        | "elif"     -> ELIF (tokinfo lexbuf)
        | "else"     -> ELSE (tokinfo lexbuf)

        | "for"      -> FOR (tokinfo lexbuf)
        | "while"    -> WHILE (tokinfo lexbuf)

        | "return"   -> RETURN (tokinfo lexbuf)
        | "break"    -> BREAK (tokinfo lexbuf)
        | "continue" -> CONTINUE (tokinfo lexbuf)
        | "pass"     -> PASS (tokinfo lexbuf)

        | "raise"    -> RAISE (tokinfo lexbuf)
        | "try"      -> TRY (tokinfo lexbuf)
        | "except"   -> EXCEPT (tokinfo lexbuf)
        | "finally"  -> FINALLY (tokinfo lexbuf)

        | "def"      -> DEF (tokinfo lexbuf)
        | "class"    -> CLASS (tokinfo lexbuf)
        | "lambda"   -> LAMBDA (tokinfo lexbuf)

        | "and"      -> AND (tokinfo lexbuf)
        | "or"       -> OR (tokinfo lexbuf)
        | "not"      -> NOT (tokinfo lexbuf)

        | "import"   -> IMPORT (tokinfo lexbuf)
        | "from"     -> FROM (tokinfo lexbuf)

        | "as"       -> AS (tokinfo lexbuf)
        | "in"       -> IN (tokinfo lexbuf)
        | "is"       -> IS (tokinfo lexbuf)
        | "assert"   -> ASSERT (tokinfo lexbuf)
        | "del"      -> DEL (tokinfo lexbuf)
        | "global"   -> GLOBAL (tokinfo lexbuf)
        | "with"     -> WITH (tokinfo lexbuf)
        | "yield"    -> YIELD (tokinfo lexbuf)

        (* python3-ext:
         * coupling: if python3 has more special keywords, you may need to
         * modify the heuristic in parse_python.ml that fallbacks to python2
         * in case of a parse error.
         *)
        | "None"    -> NONE (tokinfo lexbuf)
        | "True"   when not python2 -> TRUE (tokinfo lexbuf)
        | "False"  when not python2 -> FALSE (tokinfo lexbuf)

        | "async"    when not python2 -> ASYNC (tokinfo lexbuf)
        | "await"    when not python2 -> AWAIT (tokinfo lexbuf)
        | "nonlocal" when not python2 -> NONLOCAL (tokinfo lexbuf)

        (* python2: *)
        | "print" when python2 -> PRINT (tokinfo lexbuf)
        | "exec" when python2 -> EXEC (tokinfo lexbuf)
        (* python3-ext: no more: print, exec *)
        (* Note that "self" is not listed above because it's not considered
         * a Python keyword. In theory you could use different names for self.
         *)
        | _          -> NAME (id, (tokinfo lexbuf))
    }

  (* sgrep-ext: *)
  | '$' identifier
    { Flag_parsing.sgrep_guard (NAME (tok lexbuf, tokinfo lexbuf)) }
  (* sgrep-ext: *)
  | '$' "..." ['A'-'Z''_']['A'-'Z''_''0'-'9']*
     { Flag_parsing.sgrep_guard (NAME (tok lexbuf, tokinfo lexbuf)) }


  (* ----------------------------------------------------------------------- *)
  (* Constant *)
  (* ----------------------------------------------------------------------- *)

  (* literals *)
  | integer as n longintpostfix
      { LONGINT (Parsed_int.parse (n, tokinfo lexbuf)) }
  | integer as n
      { INT (Parsed_int.parse (n, tokinfo lexbuf)) }

  | floatnumber as n
      { FLOAT (float_of_string_opt n, tokinfo lexbuf) }

  | imagnumber as n
      { IMAG (n, tokinfo lexbuf) }

  (* ----------------------------------------------------------------------- *)
  (* Strings *)
  (* ----------------------------------------------------------------------- *)
  | fstringprefix as pre "'"  {
       push_mode state (STATE_IN_FSTRING_SINGLE pre);
       FSTRING_START (tokinfo lexbuf)
    }
  | fstringprefix as pre '"'  {
       push_mode state (STATE_IN_FSTRING_DOUBLE pre);
       FSTRING_START (tokinfo lexbuf)
    }
  | fstringprefix as pre "'''" {
       push_mode state (STATE_IN_FSTRING_TRIPLE_SINGLE pre);
       FSTRING_START (tokinfo lexbuf)
     }
  | fstringprefix as pre "\"\"\"" {
       push_mode state (STATE_IN_FSTRING_TRIPLE_DOUBLE pre);
       FSTRING_START (tokinfo lexbuf)
     }

  | stringprefix as pre '\''
      { sq_shortstrlit state (tokinfo lexbuf) pre lexbuf }
  | stringprefix as pre '"'
      { dq_shortstrlit state (tokinfo lexbuf) pre lexbuf }
  | stringprefix as pre "'''"
      { sq_longstrlit state (tokinfo lexbuf) pre lexbuf }
  | stringprefix as pre "\"\"\""
      { dq_longstrlit state (tokinfo lexbuf) pre lexbuf }

  (* ----------------------------------------------------------------------- *)
  (* eof *)
  (* ----------------------------------------------------------------------- *)
  | (whitespace* comment?) eof { EOF (tokinfo lexbuf) }

  | _ { error (spf "unrecognized symbol: %s" (tok lexbuf)) lexbuf;
        TUnknown (tokinfo lexbuf)
      }

(*****************************************************************************)
(* Rules on strings *)
(*****************************************************************************)

and sq_shortstrlit state pos pre = parse
  | (([^ '\\' '\r' '\n' '\''] | escapeseq)* as s) '\''
     {
       let full_str = Lexing.lexeme lexbuf in
       STR (unescaped s, pre, Tok.tok_add_s full_str pos) }
 | eof { error "EOF in string" lexbuf; EOF (tokinfo lexbuf) }
 | _  { error "unrecognized symbol in string" lexbuf; TUnknown(tokinfo lexbuf)}

(* because here we're using 'shortest', do not put a rule for '| _ { ... }' *)
and sq_longstrlit state pos pre = shortest
| (([^ '\\'] | escapeseq)* as s) "'''"
    {
      let full_str = Lexing.lexeme lexbuf in
      STR (unescaped s, pre, Tok.tok_add_s full_str pos)
    }

and dq_shortstrlit state pos pre = parse
  | (([^ '\\' '\r' '\n' '\"'] | escapeseq)* as s) '"'
     {
       let full_str = Lexing.lexeme lexbuf in
       STR (unescaped s, pre, Tok.tok_add_s full_str pos) }
 | eof { error "EOF in string" lexbuf; EOF (tokinfo lexbuf) }
 | _  { error "unrecognized symbol in string" lexbuf; TUnknown(tokinfo lexbuf)}

and dq_longstrlit state pos pre = shortest
  | (([^ '\\'] | escapeseq)* as s) "\"\"\""
      {
        let full_str = Lexing.lexeme lexbuf in
        STR (unescaped s, pre, Tok.tok_add_s full_str pos) }

(*****************************************************************************)
(* Rules on interpolated strings *)
(*****************************************************************************)

and fstring_single state pre = parse
 | "'" { pop_mode state; FSTRING_END (tokinfo lexbuf) }
 | "{{" { FSTRING_STRING (tok lexbuf, tokinfo lexbuf)}
 | '{' {
    ignore_nl state;
    push_mode state STATE_UNDERSCORE_TOKEN;
    FSTRING_LBRACE (tokinfo lexbuf)
   }
 (* using escapeseq2 here! for raw-fstring *)
 | ([^ '\\' '\r' '\n' '\'' '{'] | escapeseq2)*
    { FSTRING_STRING (tok lexbuf, tokinfo lexbuf)}
 (* ugly: for raw-fstring *)
 | "\\{" { if pre =~ ".*[rR]" then Parsing_helpers.yyback 1 lexbuf;
      FSTRING_STRING (tok lexbuf, tokinfo lexbuf)
    }

 | eof { error "EOF in string" lexbuf; EOF (tokinfo lexbuf) }
 | _  { error "unrecognized symbol in string" lexbuf; TUnknown(tokinfo lexbuf)}

and fstring_double state pre = parse
 | '"' { pop_mode state; FSTRING_END (tokinfo lexbuf) }
 | "{{" { FSTRING_STRING (tok lexbuf, tokinfo lexbuf)}
 | '{' {
    ignore_nl state;
    push_mode state STATE_UNDERSCORE_TOKEN;
    FSTRING_LBRACE (tokinfo lexbuf)
   }
 | ([^ '\\' '\r' '\n' '\"' '{'] | escapeseq2)*
    { FSTRING_STRING (tok lexbuf, tokinfo lexbuf)}
 (* ugly: for raw-fstring *)
 | "\\{" { if pre =~ ".*[rR]" then Parsing_helpers.yyback 1 lexbuf;
      FSTRING_STRING (tok lexbuf, tokinfo lexbuf)
    }

 | eof { error "EOF in string" lexbuf; EOF (tokinfo lexbuf) }
 | _  { error "unrecognized symbol in string" lexbuf; TUnknown(tokinfo lexbuf)}

and fstring_triple_single state pre = parse
 | "'''" { pop_mode state; FSTRING_END (tokinfo lexbuf) }
 | "{{" { FSTRING_STRING (tok lexbuf, tokinfo lexbuf)}
 | '{' {
    ignore_nl state;
    push_mode state STATE_UNDERSCORE_TOKEN;
    FSTRING_LBRACE (tokinfo lexbuf)
   }
 | '\'' { FSTRING_STRING (tok lexbuf, tokinfo lexbuf) }

 | ([^ '\\' '{' '\''] | escapeseq2)*
    { FSTRING_STRING (tok lexbuf, tokinfo lexbuf)}
 (* ugly: for raw-fstring *)
 | "\\{" { if pre =~ ".*[rR]" then Parsing_helpers.yyback 1 lexbuf;
      FSTRING_STRING (tok lexbuf, tokinfo lexbuf)
    }

 | eof { error "EOF in string" lexbuf; EOF (tokinfo lexbuf) }
 | _  { error "unrecognized symbol in string" lexbuf; TUnknown(tokinfo lexbuf)}

and fstring_triple_double state pre = parse
 | "\"\"\"" { pop_mode state; FSTRING_END (tokinfo lexbuf) }
 | "{{" { FSTRING_STRING (tok lexbuf, tokinfo lexbuf)}
 | '{' {
    ignore_nl state;
    push_mode state STATE_UNDERSCORE_TOKEN;
    FSTRING_LBRACE (tokinfo lexbuf)
   }
 | '"' { FSTRING_STRING (tok lexbuf, tokinfo lexbuf) }

 | ([^ '\\' '{' '"'] | escapeseq2)*
    { FSTRING_STRING (tok lexbuf, tokinfo lexbuf)}
 (* ugly: for raw-fstring *)
 | "\\{" { if pre =~ ".*[rR]" then Parsing_helpers.yyback 1 lexbuf;
      FSTRING_STRING (tok lexbuf, tokinfo lexbuf)
    }

 | eof { error "EOF in string" lexbuf; EOF (tokinfo lexbuf) }
 | _  { error "unrecognized symbol in string" lexbuf; TUnknown(tokinfo lexbuf)}
