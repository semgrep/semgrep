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
let tokinfo = Tok.tok_of_lexbuf
let error = Parsing_error.lexical_error

}

(*****************************************************************************)
(* UTF-8 boilerplate *)
(*****************************************************************************)
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

(*****************************************************************************)
(* Regexp aliases *)
(*****************************************************************************)

let newline = ('\n' | "\r\n")
let whitespace = [' ' '\t']

(* TODO: unicode digits *)
let unicode_digit = ['0'-'9']

(* TODO: unicode letters *)
let unicode_letter = ['a'-'z' 'A'-'Z']

let unicode_char =
  ascii # ['\n' '\r']
| utf8_nonascii

let unicode_char_no_quote =
  ascii # ['\n' '\r' '\'' '\\']
| utf8_nonascii

let unicode_char_no_double_quote =
  ascii # ['\n' '\r' '"' '\\']
| utf8_nonascii

let unicode_char_no_backquote =
  ascii # ['\n' '\r' '`' ]
| utf8_nonascii

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

(* semgrep: we can use regexp in semgrep in strings and we want to
 * support any escape characters there, e.g. eval("=~/.*dev\.corp/")
 *)
let semgrep_escapeseq = '\\' _

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
      TComment(info |> Tok.rewrap_str (Buffer.contents buf))
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
  (* sgrep-ext: *)
  | '$' "..." ['A'-'Z''_']['A'-'Z''_''0'-'9']*
     { Flag.sgrep_guard (LNAME (tok lexbuf, tokinfo lexbuf)) }


  (* ----------------------------------------------------------------------- *)
  (* Constant *)
  (* ----------------------------------------------------------------------- *)

  (* literals *)
  (* this is also part of int_lit, but we specialize it here to use the
   * right int_of_string *)
  | "0" (octal_digits as n) { LINT (Parsed_int.parse ( "0o" ^ n, tokinfo lexbuf)) }

  | int_lit as n
      { LINT (Parsed_int.parse (n, tokinfo lexbuf)) }

  | float_lit as n
      { LFLOAT (float_of_string_opt n, tokinfo lexbuf) }

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
  | '"' ((unicode_value_no_double_quote | byte_value | semgrep_escapeseq)* as s) '"'
      { Flag.sgrep_guard (LSTR (s, tokinfo lexbuf)) }

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
