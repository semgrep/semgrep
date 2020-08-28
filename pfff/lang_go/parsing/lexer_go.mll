{
(* Yoann Padioleau
 *
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

open Parser_go
module PI = Parse_info
module Flag = Flag_parsing
open AST_generic (* for LASOP *)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* The Go lexer.
 *
 * reference:
 *    - https://golang.org/ref/spec#Lexical_elements
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* shortcuts *)
let tok = Lexing.lexeme
let tokinfo = Parse_info.tokinfo
let error = Parse_info.lexical_error

}

(*****************************************************************************)
(* Regexp aliases *)
(*****************************************************************************)

let newline = ('\n' | "\r\n")
let whitespace = [' ' '\t']

(* todo: *)
let unicode_digit = ['0'-'9']
let unicode_letter = ['a'-'z' 'A'-'Z']
let unicode_char = [^ '\n' '\r']
let unicode_char_no_quote = [^ '\n' '\r' '\'' '\\']
let unicode_char_no_double_quote = [^ '\n' '\r' '"' '\\']
let unicode_char_no_backquote = [^ '\n' '\r' '`' ]

let letter = unicode_letter | '_'

let identifier = letter (letter | unicode_digit)*


let decimal_digit = ['0'-'9']
let binary_digit = ['0'-'1']
let octal_digit = ['0'-'7']
let hex_digit = ['0'-'9' 'a'-'f' 'A'-'F']

let decimal_digits = decimal_digit ('_'? decimal_digit)*
let binary_digits = binary_digit ('_'? binary_digit)*
let octal_digits = octal_digit ('_'? octal_digit)*
let hex_digits = hex_digit ('_'? hex_digit)*

let decimal_lit = "0" | ['1'-'9'] ( '_'? decimal_digits)?
let binary_lit = "0" ['b' 'B'] '_'? binary_digits
let octal_lit = "0" ['o' 'O']? '_'? octal_digits
let hex_lit = "0" ['x' 'X'] '_'? hex_digits

let int_lit = 
   decimal_lit 
 | binary_lit 
 | octal_lit 
 | hex_lit

let decimal_exponent = ['e' 'E'] ['+' '-']? decimal_digits
let decimal_float_lit =
   decimal_digits '.' decimal_digits? decimal_exponent?
 | decimal_digits decimal_exponent
 | '.' decimal_digits decimal_exponent?

let hex_mantissa = 
   '_'? hex_digits '.' hex_digits?
 | '_'? hex_digits
 | '.' hex_digits
let hex_exponent = ['p' 'P'] ['+' '-']? decimal_digits
let hex_float_lit = '0' ['x' 'X'] hex_mantissa hex_exponent

let float_lit = decimal_float_lit | hex_float_lit

let imaginary_lit = (decimal_digits | int_lit | float_lit) 'i'

let escaped_char = '\\' ['a' 'b' 'f' 'n' 'r' 't' 't' 'v' '\\' '\'' '"']

let little_u_value = '\\' 'u' hex_digit hex_digit hex_digit hex_digit
let big_u_value =    '\\' 'U' hex_digit hex_digit hex_digit hex_digit
                              hex_digit hex_digit hex_digit hex_digit

(* the Go ref says just unicode_char, but this can not work, hence the
 * use of various xxx_no_yyy below
 *)
let unicode_value_no_quote = 
  unicode_char_no_quote
| little_u_value 
| big_u_value 
| escaped_char

let unicode_value_no_double_quote = 
  unicode_char_no_double_quote
| little_u_value 
| big_u_value 
| escaped_char

let octal_byte_value = '\\' octal_digit octal_digit octal_digit
let hex_byte_value = '\\' 'x' hex_digit hex_digit
let byte_value = octal_byte_value | hex_byte_value

let escapeseq = '\\' _

(*****************************************************************************)
(* Rule initial *)
(*****************************************************************************)

rule token = parse

  (* ----------------------------------------------------------------------- *)
  (* spacing/comments *)
  (* ----------------------------------------------------------------------- *)
  | "/*" {
      let info = tokinfo lexbuf in
      let buf = Buffer.create 127 in
      Buffer.add_string buf "/*";
      comment buf lexbuf;
      TComment(info |> PI.rewrap_str (Buffer.contents buf))
    }

  (* don't keep the trailing \n; it will be in another token *)
  | "//" [^ '\r' '\n']* { TComment (tokinfo lexbuf) }
  | newline     { TCommentNewline (tokinfo lexbuf) }
  | whitespace+ { TCommentSpace (tokinfo lexbuf) }

  (* ----------------------------------------------------------------------- *)
  (* symbols *)
  (* ----------------------------------------------------------------------- *)

  | '+'     { LPLUS (tokinfo lexbuf) }  | '-'     { LMINUS (tokinfo lexbuf) }
  | '*'     { LMULT (tokinfo lexbuf) }  | '/'     { LDIV (tokinfo lexbuf) }
  | '%'     { LPERCENT (tokinfo lexbuf) }

  | "+="    { LASOP (Plus, tokinfo lexbuf) }
  | "-="    { LASOP (Minus, tokinfo lexbuf) }
  | "*="    { LASOP (Mult, tokinfo lexbuf) }
  | "/="    { LASOP (Div, tokinfo lexbuf) }
  | "%="    { LASOP (Mod, tokinfo lexbuf) }
  | "&="    { LASOP (BitAnd, tokinfo lexbuf) }
  | "|="    { LASOP (BitOr, tokinfo lexbuf) }
  | "^="    { LASOP (BitXor, tokinfo lexbuf) }
  | "<<="   { LASOP (LSL, tokinfo lexbuf) }
  | ">>="   { LASOP (LSR, tokinfo lexbuf) }
  (* Go specific operator *)
  | "&^="   { LASOP (BitClear, tokinfo lexbuf) }

  | "=="    { LEQEQ (tokinfo lexbuf) }
  | "!="    { LNE (tokinfo lexbuf) }

  | "<="    { LLE (tokinfo lexbuf) }  | ">="    { LGE (tokinfo lexbuf) }
  | '<'     { LLT (tokinfo lexbuf) }  | '>'     { LGT (tokinfo lexbuf) }

  | '='     { LEQ (tokinfo lexbuf) }

  | '|'     { LPIPE (tokinfo lexbuf) }
  | '&'     { LAND (tokinfo lexbuf) }
  | '^'     { LHAT (tokinfo lexbuf) }
  | '~'     { LTILDE (tokinfo lexbuf) }
  | '!'     { LBANG (tokinfo lexbuf) }

  | "<<"    { LLSH (tokinfo lexbuf) }  | ">>"    { LRSH (tokinfo lexbuf) }

  | "++"    { LINC (tokinfo lexbuf) }  | "--"    { LDEC (tokinfo lexbuf) }

  | "&&"    { LANDAND (tokinfo lexbuf) } | "||"    { LOROR (tokinfo lexbuf) }

  | "<-"    { LCOMM (tokinfo lexbuf) }
  | ":="     { LCOLAS (tokinfo lexbuf) }

  | '(' { LPAREN (tokinfo lexbuf) }   | ')' { RPAREN (tokinfo lexbuf) }
  | '[' { LBRACKET (tokinfo lexbuf) } | ']' { RBRACKET (tokinfo lexbuf) }
  (* can be transformed in an LBODY by parsing hack later *)
  | '{' { LBRACE (tokinfo lexbuf) }   | '}' { RBRACE (tokinfo lexbuf) }

  | ':'     { LCOLON (tokinfo lexbuf) }
  | ';'     { LSEMICOLON (tokinfo lexbuf) }
  | '.'     { LDOT (tokinfo lexbuf) }
  | ','     { LCOMMA (tokinfo lexbuf) }

  (* part of go and also sgrep-ext: *)
  | "..."   { LDDD (tokinfo lexbuf) }
  (* sgrep-ext: *)
  | "<..."  { Flag_parsing.sgrep_guard (LDots (tokinfo lexbuf)) }
  | "...>"  { Flag_parsing.sgrep_guard (RDots (tokinfo lexbuf)) }

  (* ----------------------------------------------------------------------- *)
  (* Keywords and ident *)
  (* ----------------------------------------------------------------------- *)

  (* keywords *)
  | identifier as id
      { match id with

        | "if"       -> LIF (tokinfo lexbuf)
        | "else"     -> LELSE (tokinfo lexbuf)

        | "for"      -> LFOR (tokinfo lexbuf)

        | "switch"      -> LSWITCH (tokinfo lexbuf)
        | "case"      -> LCASE (tokinfo lexbuf)
        | "default"      -> LDEFAULT (tokinfo lexbuf)

        | "return"   -> LRETURN (tokinfo lexbuf)
        | "break"    -> LBREAK (tokinfo lexbuf)
        | "continue" -> LCONTINUE (tokinfo lexbuf)
        | "fallthrough"      -> LFALL (tokinfo lexbuf)
        | "goto"      -> LGOTO (tokinfo lexbuf)

        | "func"      -> LFUNC (tokinfo lexbuf)
        | "const"    -> LCONST (tokinfo lexbuf)
        | "var"    -> LVAR (tokinfo lexbuf)
        | "type"    -> LTYPE (tokinfo lexbuf)
        | "struct"    -> LSTRUCT (tokinfo lexbuf)
        | "interface"   -> LINTERFACE (tokinfo lexbuf)


        | "package"     -> LPACKAGE (tokinfo lexbuf)
        | "import"   -> LIMPORT (tokinfo lexbuf)

        | "go"       -> LGO (tokinfo lexbuf)
        | "chan"       -> LCHAN (tokinfo lexbuf)
        | "select"       -> LSELECT (tokinfo lexbuf)
        | "defer"   -> LDEFER (tokinfo lexbuf)
        | "map"      -> LMAP (tokinfo lexbuf)
        | "range"   -> LRANGE (tokinfo lexbuf)

        (* declared in the "universe block"
         *  - true, false
         *  - iota
         *  - new, make, 
         *    panic (CFG effect, like goto), recover,
         *    print, println
         *    complex, imag, real
         *    append, cap, 
         *    close, delete, copy, 
         *    len,
         *  - nil
         *  - _ (blank identifier)
         *)
      
        | _          -> LNAME (id, (tokinfo lexbuf)) 
    }

  (* sgrep-ext: *)
  | '$' identifier 
    { let s = tok lexbuf in
      if not !Flag_parsing.sgrep_mode
      then error ("identifier with dollar: "  ^ s) lexbuf;
      LNAME (s, tokinfo lexbuf)
    }

  (* ----------------------------------------------------------------------- *)
  (* Constant *)
  (* ----------------------------------------------------------------------- *)

  (* literals *)
  | int_lit as n
      { LINT (n, tokinfo lexbuf) }

  | float_lit as n
      { LFLOAT (n, tokinfo lexbuf) }

  | imaginary_lit as n
      { LIMAG (n, tokinfo lexbuf) }

  (* ----------------------------------------------------------------------- *)
  (* Chars/Strings *)
  (* ----------------------------------------------------------------------- *)
  | '\'' ((unicode_value_no_quote | byte_value) as s) '\''
      { LRUNE (s, tokinfo lexbuf) }
  | '`' ((unicode_char_no_backquote | newline)* as s) '`'
      { LSTR (s, tokinfo lexbuf) }
  | '"' ((unicode_value_no_double_quote | byte_value)* as s) '"'
      { LSTR (s, tokinfo lexbuf) }

  (* ----------------------------------------------------------------------- *)
  (* eof *)
  (* ----------------------------------------------------------------------- *)
  | eof { EOF (tokinfo lexbuf) }

  | _ { error (spf "unrecognized symbol: %s" (tok lexbuf)) lexbuf;
        TUnknown (tokinfo lexbuf)
      }

(*****************************************************************************)
(* Rule comment *)
(*****************************************************************************)

and comment buf = parse
  | "*/"    { Buffer.add_string buf (tok lexbuf) }
  (* noteopti: *)
  | [^'*']+ { Buffer.add_string buf (tok lexbuf); comment buf lexbuf }
  | "*"     { Buffer.add_string buf (tok lexbuf); comment buf lexbuf }
  | eof     { error "end of file in comment" lexbuf }
  | _  {
      let s = tok lexbuf in
      error ("unrecognised symbol in comment:"^s) lexbuf;
      Buffer.add_string buf s;
      comment buf lexbuf
    }
