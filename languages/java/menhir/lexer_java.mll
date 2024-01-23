{
(* Joust: a Java lexer, parser, and pretty-printer written in OCaml
 *  Copyright (C) 2001  Eric C. Cooper <ecc@cmu.edu>
 *  Copyright (C) 2022  Eric C. Cooper <ecc@cmu.edu>
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * license.txt for more details.
 *
 * ocamllex lexer for Java
 *
 * Attempts to conform to:
 * The Java Language Specification Second Edition
 * - James Gosling, Bill Joy, Guy Steele, Gilad Bracha
 *
 * Extended by Yoann Padioleau to support more recent versions of Java.
 * Copyright (C) 2011 Facebook
 * Copyright (C) 2020 r2c
 *)

open AST_generic (* for arithmetic operators *)
open Parser_java
module Flag = Flag_parsing

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* shortcuts *)
let tok = Lexing.lexeme
let tokinfo = Tok.tok_of_lexbuf
let error = Parsing_error.lexical_error

(* ---------------------------------------------------------------------- *)
(* Keywords *)
(* ---------------------------------------------------------------------- *)
let primitive_type t = (t, (fun ii -> PRIMITIVE_TYPE (t, ii)))

let keyword_table = Hashtbl_.hash_of_list [
  "if", (fun ii -> IF ii);
  "else", (fun ii -> ELSE ii);

  "while", (fun ii -> WHILE ii);
  "do", (fun ii -> DO ii);
  "for", (fun ii -> FOR ii);

  "return", (fun ii -> RETURN ii);
  "break", (fun ii -> BREAK ii);
  "continue", (fun ii -> CONTINUE ii);

  "switch", (fun ii -> SWITCH ii);
  "case", (fun ii -> CASE ii);
  (* javaext: now also use for interface default implementation in 1.? *)
  "default", (fun ii -> DEFAULT ii);

  "goto", (fun ii -> GOTO ii);

  "try", (fun ii -> TRY ii);
  "catch", (fun ii -> CATCH ii);
  "finally", (fun ii -> FINALLY ii);
  "throw", (fun ii -> THROW ii);

  "synchronized", (fun ii -> SYNCHRONIZED ii);


  "true", (fun ii -> TRUE ii);
  "false", (fun ii -> FALSE ii);
  "null", (fun ii -> NULL ii);

  "void", (fun ii -> VOID ii);

  primitive_type "boolean";
  primitive_type "byte";
  primitive_type "char";
  primitive_type "short";
  primitive_type "int";
  primitive_type "long";
  primitive_type "float";
  primitive_type "double";

  "class", (fun ii -> CLASS ii);
  "interface", (fun ii -> INTERFACE ii);
  "extends", (fun ii -> EXTENDS ii);
  "implements", (fun ii -> IMPLEMENTS ii);

  "this", (fun ii -> THIS ii);
  "super", (fun ii -> SUPER ii);
  "new", (fun ii -> NEW ii);
  "instanceof", (fun ii -> INSTANCEOF ii);

  "abstract", (fun ii -> ABSTRACT ii);
  "final", (fun ii -> FINAL ii);

  "private", (fun ii -> PRIVATE ii);
  "protected", (fun ii -> PROTECTED ii);
  "public", (fun ii -> PUBLIC ii);

  "const", (fun ii -> CONST ii);

  "native", (fun ii -> NATIVE ii);
  "static", (fun ii -> STATIC ii);
  "strictfp", (fun ii -> STRICTFP ii);
  "transient", (fun ii -> TRANSIENT ii);
  "volatile", (fun ii -> VOLATILE ii);

  "throws", (fun ii -> THROWS ii);

  "package", (fun ii -> PACKAGE ii);
  "import", (fun ii -> IMPORT ii);

  (* javaext: 1.4 *)
  "assert", (fun ii -> ASSERT ii);
  (* javaext: 1.? *)
  "enum", (fun ii -> ENUM ii);
  (* javaext: 1.? *)
  (*  "var", (fun ii -> VAR ii); REGRESSIONS *)
  (* javaext: 15 *)
  "record", (fun ii -> RECORD ii);
]

}
(*****************************************************************************)
(* Regexps aliases *)
(*****************************************************************************)
let LF = '\n'  (* newline *)
let CR = '\r'  (* return *)

let LineTerminator = LF | CR | CR LF
let InputCharacter = [^ '\r' '\n']

let SUB = '\026' (* control-Z *) (* decimal *)

let SP = ' '     (* space *)
let HT = '\t'    (* horizontal tab *)
let FF = '\012'  (* form feed *) (* decimal *)

let _WhiteSpace = SP | HT | FF (* | LineTerminator -- handled separately *)

(* let TraditionalComment = "/*" ([^ '*'] | '*' [^ '/'])* "*/" *)
let EndOfLineComment = "//" InputCharacter* LineTerminator
(* let Comment = TraditionalComment | EndOfLineComment *)

(* sgrep-ext: $ is actually a valid letter in Java *)
let Letter = ['A'-'Z' 'a'-'z' '_' '$']
let Digit = ['0'-'9']

let Identifier = Letter (Letter | Digit)*

let NonZeroDigit = ['1'-'9']
let HexDigit = ['0'-'9' 'a'-'f' 'A'-'F']
let OctalDigit = ['0'-'7']
let BinaryDigit = ['0'-'1']

(* javaext: underscore in numbers *)
let IntegerTypeSuffix = ['l' 'L']
let Underscores = '_' '_'*

let DigitOrUnderscore = Digit | '_'
let DigitsAndUnderscores = DigitOrUnderscore DigitOrUnderscore*
let Digits = Digit | Digit DigitsAndUnderscores? Digit

let DecimalNumeral =
  "0"
| NonZeroDigit Digits?
| NonZeroDigit Underscores Digits

let DecimalIntegerLiteral = DecimalNumeral IntegerTypeSuffix?

let HexDigitOrUndercore = HexDigit | '_'
let HexDigitsAndUnderscores = HexDigitOrUndercore HexDigitOrUndercore*
let HexDigits = HexDigit | HexDigit HexDigitsAndUnderscores? HexDigit
let HexNumeral = ("0x" | "0X") HexDigits
let HexIntegerLiteral = HexNumeral IntegerTypeSuffix?

let OctalDigitOrUnderscore = OctalDigit | '_'
let OctalDigitsAndUnderscores = OctalDigitOrUnderscore OctalDigitOrUnderscore*
let OctalDigits = OctalDigit | OctalDigit OctalDigitsAndUnderscores? OctalDigit
let OctalNumeral = "0" (OctalDigits | Underscores OctalDigits)
let OctalIntegerLiteral = OctalNumeral IntegerTypeSuffix?

let BinaryDigitOrUnderscore = BinaryDigit | '_'
let BinaryDigitsAndUnderscores = BinaryDigitOrUnderscore BinaryDigitOrUnderscore*
let BinaryDigits = BinaryDigit | BinaryDigit BinaryDigitsAndUnderscores? BinaryDigit
let BinaryNumeral = ("0b" | "0B") BinaryDigits
let BinaryIntegerLiteral = BinaryNumeral IntegerTypeSuffix?

let IntegerLiteral =
  DecimalIntegerLiteral
| HexIntegerLiteral
| OctalIntegerLiteral
(* javaext: ? *)
| BinaryIntegerLiteral

let ExponentPart = ['e' 'E'] ['+' '-']? Digit+

let FloatTypeSuffix = ['f' 'F' 'd' 'D']

let FloatingPointLiteral =
  (Digit+ '.' Digit* | '.' Digit+) ExponentPart? FloatTypeSuffix?
| Digit+ (ExponentPart FloatTypeSuffix? | ExponentPart? FloatTypeSuffix)

let BooleanLiteral = "true" | "false"

let OctalEscape = '\\' ['0'-'3']? OctalDigit? OctalDigit

(* Not in spec -- added because we don't handle Unicode elsewhere. *)

let UnicodeEscape = "\\u" HexDigit HexDigit HexDigit HexDigit

let EscapeSequence =
  '\\' ['b' 't' 'n' 'f' 'r' '"' '\'' '\\']
| OctalEscape
| UnicodeEscape

(* semgrep: we can use regexp in semgrep in strings and we want to
 * support any escape characters there, e.g. eval("=~/.*dev\.corp/")
 *)
let EscapeSequence_semgrep =
  '\\' _

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

(************************ end of UTF-8 boilerplate ************************)

let SingleCharacter = [^ '\'' '\\' '\n' '\r']
let CharacterLiteral = '\'' (SingleCharacter | EscapeSequence | utf8 ) '\''


let StringCharacter = [^ '"' '\\' '\n' '\r']
(* used inline later *)
let StringLiteral = '"' (StringCharacter | EscapeSequence)* '"'

let NullLiteral = "null"

let _Literal =
  IntegerLiteral
| FloatingPointLiteral
| CharacterLiteral
| StringLiteral
| BooleanLiteral
| NullLiteral

(* Assignment operators, except '=', from section 3.12 *)

let _AssignmentOperator =
  ('+' | '-' | '*' | '/' | '&' | '|' | '^' | '%' | "<<" | ">>" | ">>>") '='


let newline = '\n'

(*****************************************************************************)
(* Token rule *)
(*****************************************************************************)
rule token = parse

  (* ----------------------------------------------------------------------- *)
  (* spacing/comments *)
  (* ----------------------------------------------------------------------- *)
  | [' ' '\t' '\r' '\011' '\012' ]+  { TCommentSpace (tokinfo lexbuf) }

  | newline { TCommentNewline (tokinfo lexbuf) }

  | "/*"
    {
      let info = tokinfo lexbuf in
      let com = comment lexbuf in
      TComment(info |> Tok.tok_add_s com)
    }
  (* don't keep the trailing \n; it will be in another token *)
  | "//" InputCharacter*
   { TComment(tokinfo lexbuf) }


  (* ----------------------------------------------------------------------- *)
  (* Constant *)
  (* ----------------------------------------------------------------------- *)

  (* this is also part of IntegerLiteral, but we specialize it here to use the
   * right int_of_string *)
  | "0" (OctalDigits | Underscores OctalDigits) as n
     { TInt (Parsed_int.parse ( "0o" ^ n, tokinfo lexbuf)) }

  | IntegerLiteral as n       { TInt (Parsed_int.parse (n, tokinfo lexbuf)) }
  | FloatingPointLiteral as n { TFloat (float_of_string_opt n, tokinfo lexbuf) }
  | CharacterLiteral     { TChar (tok lexbuf, tokinfo lexbuf) }
  | '"' ( (StringCharacter | EscapeSequence)* as s) '"'
   { TString (s, tokinfo lexbuf) }
  (* semgrep: *)
  | '"' ( (StringCharacter | EscapeSequence | EscapeSequence_semgrep)* as s)'"'
   { Flag.sgrep_guard (TString (s, tokinfo lexbuf)) }
  (* bool and null literals are keywords, see below *)

  (* ----------------------------------------------------------------------- *)
  (* Keywords and ident (must be after "true"|"false" above) *)
  (* ----------------------------------------------------------------------- *)

  (* semgrep: Note that Identifier accepts dollars in it. According to
   * https://docs.oracle.com/javase/specs/jls/se8/html/jls-3.html
   * identifiers can contain and start with a dollar (especially in
   * generated code)
   * old: if not !Flag_parsing.sgrep_mode
   * then error ("identifier with dollar: "  ^ s) lexbuf;
   *)
  | Identifier
    {
      let info = tokinfo lexbuf in
      let s = tok lexbuf in

      match Common2.optionise (fun () -> Hashtbl.find keyword_table s) with
      | Some f -> f info
      | None -> IDENTIFIER (s, info)
    }
  (* sgrep-ext: *)
  | '$' "..." ['A'-'Z''_']['A'-'Z''_''0'-'9']*
     { Flag.sgrep_guard (METAVAR_ELLIPSIS (tok lexbuf, tokinfo lexbuf)) }

  (* ----------------------------------------------------------------------- *)
  (* Symbols *)
  (* ----------------------------------------------------------------------- *)

  | '('  { LP(tokinfo lexbuf) } | ')'  { RP(tokinfo lexbuf) }
  | '{'  { LC(tokinfo lexbuf) } | '}'  { RC(tokinfo lexbuf) }
  | '['  { LB(tokinfo lexbuf) } | ']'  { RB(tokinfo lexbuf) }
  | ';'  { SM(tokinfo lexbuf) }
  | ','  { CM(tokinfo lexbuf) }
  | '.'  { DOT(tokinfo lexbuf) }

  (* pad: to avoid some conflicts *)
  | "[]"  { LB_RB(tokinfo lexbuf) }

  | "="  { EQ(tokinfo lexbuf) }
  (* relational operator also now used for generics, can be transformed in LT2 *)
  | "<"  { LT(tokinfo lexbuf) } | ">"  { GT(tokinfo lexbuf) }
  | "!"  { NOT(tokinfo lexbuf) }
  | "~"  { COMPL(tokinfo lexbuf) }
  | "?"  { COND(tokinfo lexbuf) }
  | ":"  { COLON(tokinfo lexbuf) }
  | "=="  { EQ_EQ(tokinfo lexbuf) }
  | "<="  { LE(tokinfo lexbuf) } | ">="  { GE(tokinfo lexbuf) }
  | "!="  { NOT_EQ(tokinfo lexbuf) }
  | "&&"  { AND_AND(tokinfo lexbuf) } | "||"  { OR_OR(tokinfo lexbuf) }
  | "++"  { INCR(tokinfo lexbuf) } | "--"  { DECR(tokinfo lexbuf) }
  | "+"  { PLUS(tokinfo lexbuf) } | "-"  { MINUS(tokinfo lexbuf) }
  | "*"  { TIMES(tokinfo lexbuf) } | "/"  { DIV(tokinfo lexbuf) }
  | "&"  { AND(tokinfo lexbuf) }
  (* javaext: also used inside catch for list of possible exn *)
  | "|"  { OR(tokinfo lexbuf) }
  | "^"  { XOR(tokinfo lexbuf) }
  | "%"  { MOD(tokinfo lexbuf) }
  | "<<"  { LS(tokinfo lexbuf) }
  (* this may be split in two tokens in fix_tokens_java.ml *)
  | ">>"  { SRS(tokinfo lexbuf) }
  | ">>>"  { URS(tokinfo lexbuf) }
  (* javaext: lambdas *)
  | "->" { ARROW (tokinfo lexbuf) }
  (* javaext: qualified method *)
  | "::" { COLONCOLON (tokinfo lexbuf) }

  (* ext: annotations *)
  | "@" { AT(tokinfo lexbuf) }
  (* regular feature of Java for params and sgrep-ext: *)
  | "..."  { DOTS(tokinfo lexbuf) }
  (* sgrep-ext: *)
  | "<..."  { Flag_parsing.sgrep_guard (LDots (tokinfo lexbuf)) }
  | "...>"  { Flag_parsing.sgrep_guard (RDots (tokinfo lexbuf)) }

  | "+="  { OPERATOR_EQ (Plus, tokinfo lexbuf) }
  | "-="  { OPERATOR_EQ (Minus, tokinfo lexbuf) }
  | "*="  { OPERATOR_EQ (Mult, tokinfo lexbuf) }
  | "/="  { OPERATOR_EQ (Div, tokinfo lexbuf) }
  | "%="  { OPERATOR_EQ (Mod, tokinfo lexbuf) }
  | "&="  { OPERATOR_EQ (BitAnd, tokinfo lexbuf) }
  | "|="  { OPERATOR_EQ (BitOr, tokinfo lexbuf) }
  | "^="  { OPERATOR_EQ (BitXor, tokinfo lexbuf) }
  | "<<=" { OPERATOR_EQ (LSL, tokinfo lexbuf) }
  | ">>=" { OPERATOR_EQ (LSR, tokinfo lexbuf) }
  | ">>>="{ OPERATOR_EQ (ASR, tokinfo lexbuf) }

  | SUB? eof { EOF (tokinfo lexbuf |> Tok.rewrap_str "") }

  | _ {
  error ("unrecognised symbol, in token rule:"^tok lexbuf) lexbuf;
  TUnknown (tokinfo lexbuf)
  }

(*****************************************************************************)
(* Comments *)
(*****************************************************************************)

(* less: allow only char-'*' ? *)
and comment = parse
  | "*/"     { tok lexbuf }
  (* noteopti: *)
  | [^ '*']+ { let s = tok lexbuf in s ^ comment lexbuf }
  | [ '*']   { let s = tok lexbuf in s ^ comment lexbuf }
  | eof  { error ("Unterminated_comment") lexbuf;  ""  }
  | _  {
    let s = tok lexbuf in
    error ("unrecognised symbol in comment:"^s) lexbuf;
    s ^ comment lexbuf
  }
