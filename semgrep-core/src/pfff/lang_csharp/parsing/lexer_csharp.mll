{
(* Yoann Padioleau
 *
 * Copyright (C) 2010 Facebook
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

module Ast = Ast_csharp
module Flag = Flag_parsing

open Parser_csharp

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * http://www.jaggersoft.com/csharp_grammar.html
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
(* shortcuts *)
let tok = Lexing.lexeme
let tokinfo = Parse_info.tokinfo
let error = Parse_info.lexical_error

(* ---------------------------------------------------------------------- *)
let keyword_table = Common.hash_of_list [
  "if", (fun ii -> Tif ii);
  "abstract", (fun ii -> Tabstract ii);
  "as", (fun ii -> Tas ii);
  "base", (fun ii -> Tbase ii);
  "bool", (fun ii -> Tbool ii);
  "break", (fun ii -> Tbreak ii);
  "byte", (fun ii -> Tbyte ii);
  "case", (fun ii -> Tcase ii);
  "catch", (fun ii -> Tcatch ii);
  "char", (fun ii -> Tchar ii);
  "checked", (fun ii -> Tchecked ii);
  "class", (fun ii -> Tclass ii);
  "const", (fun ii -> Tconst ii);
  "continue", (fun ii -> Tcontinue ii);
  "decimal", (fun ii -> Tdecimal ii);
  "default", (fun ii -> Tdefault ii);
  "delegate", (fun ii -> Tdelegate ii);
  "do", (fun ii -> Tdo ii);
  "double", (fun ii -> Tdouble ii);
  "else", (fun ii -> Telse ii);
  "enum", (fun ii -> Tenum ii);
  "event", (fun ii -> Tevent ii);
  "explicit", (fun ii -> Texplicit ii);
  "extern", (fun ii -> Textern ii);
  "finally", (fun ii -> Tfinally ii);
  "fixed", (fun ii -> Tfixed ii);
  "float", (fun ii -> Tfloat ii);
  "for", (fun ii -> Tfor ii);
  "foreach", (fun ii -> Tforeach ii);
  "goto", (fun ii -> Tgoto ii);
  "if", (fun ii -> Tif ii);
  "implicit", (fun ii -> Timplicit ii);
  "in", (fun ii -> Tin ii);
  "int", (fun ii -> Tint ii);
  "interface", (fun ii -> Tinterface ii);
  "internal", (fun ii -> Tinternal ii);
  "is", (fun ii -> Tis ii);
  "lock", (fun ii -> Tlock ii);
  "long", (fun ii -> Tlong ii);
  "namespace", (fun ii -> Tnamespace ii);
  "new", (fun ii -> Tnew ii);
  "null", (fun ii -> Tnull ii);
  "object", (fun ii -> Tobject ii);
  "operator", (fun ii -> Toperator ii);
  "out", (fun ii -> Tout ii);
  "override", (fun ii -> Toverride ii);
  "params", (fun ii -> Tparams ii);
  "private", (fun ii -> Tprivate ii);
  "protected", (fun ii -> Tprotected ii);
  "public", (fun ii -> Tpublic ii);
  "readonly", (fun ii -> Treadonly ii);
  "ref", (fun ii -> Tref ii);
  "return", (fun ii -> Treturn ii);
  "sbyte", (fun ii -> Tsbyte ii);
  "sealed", (fun ii -> Tsealed ii);
  "short", (fun ii -> Tshort ii);
  "sizeof", (fun ii -> Tsizeof ii);
  "stackalloc", (fun ii -> Tstackalloc ii);
  "static", (fun ii -> Tstatic ii);
  "string", (fun ii -> Tstring ii);
  "struct", (fun ii -> Tstruct ii);
  "switch", (fun ii -> Tswitch ii);
  "this", (fun ii -> Tthis ii);
  "throw", (fun ii -> Tthrow ii);
  "try", (fun ii -> Ttry ii);
  "typeof", (fun ii -> Ttypeof ii);
  "uint", (fun ii -> Tuint ii);
  "ulong", (fun ii -> Tulong ii);
  "unchecked", (fun ii -> Tunchecked ii);
  "unsafe", (fun ii -> Tunsafe ii);
  "ushort", (fun ii -> Tushort ii);
  "using", (fun ii -> Tusing ii);
  "virtual", (fun ii -> Tvirtual ii);
  "void", (fun ii -> Tvoid ii);
  "volatile", (fun ii -> Tvolatile ii);
  "while" , (fun ii -> Twhile ii);

  "true", (fun ii -> Ttrue ii);
  "false", (fun ii -> Tfalse ii);
]

}
(*****************************************************************************)

let letter = ['A'-'Z' 'a'-'z']
let digit  = ['0'-'9']

let newline = '\n'
let space = [' ' '\t']

let nonzerodigit = ['1'-'9']
let octdigit = ['0'-'7']
let hexdigit = digit | ['a'-'f'] | ['A'-'F']


let ident = (letter | '_') (letter | digit)*
(* TODO connect-char combine-char formating-char *)


let escapeseq =
   ( '\\' 'x' hexdigit hexdigit? hexdigit? hexdigit?
    '\\' ['\'' '"' '\\' '0' 'a' 'b' 'f' 'n' 'r' 't' 'v']
   )

(* TODO, was copied from python *)
let decimalinteger = nonzerodigit digit* | '0'
let octinteger = '0' octdigit+
let hexinteger = '0' ('x' | 'X') hexdigit+

let integer = (decimalinteger | octinteger | hexinteger)

(*****************************************************************************)

rule token = parse

  (* ----------------------------------------------------------------------- *)
  (* spacing/comments *)
  (* ----------------------------------------------------------------------- *)

  | "//" [^ '\n']* { TComment (tokinfo lexbuf) }

  | "/*"
      { let info = tokinfo lexbuf in
        let com = comment lexbuf in
        TComment(info |> Parse_info.tok_add_s com)
      }

  | newline { TCommentNewline (tokinfo lexbuf) }
  | space+ { TCommentSpace (tokinfo lexbuf) }

  (* ----------------------------------------------------------------------- *)
  (* cpp *)
  (* ----------------------------------------------------------------------- *)
  | "#" [' ' '\t']* "line" { TCppLine (tokinfo lexbuf) }
  | "#" [' ' '\t']* "error" { TCppError (tokinfo lexbuf) }
  | "#" [' ' '\t']* "warning" { TCppWarning (tokinfo lexbuf) }
  | "#" [' ' '\t']* "region" { TCppRegion (tokinfo lexbuf) }
  | "#" [' ' '\t']* "endregion" { TCppEndRegion (tokinfo lexbuf) }

  | "#" [' ' '\t']* "define" { TDefine (tokinfo lexbuf) }
  | "#" [' ' '\t']* "undef" { TUndef (tokinfo lexbuf) }

  | "#" [' ' '\t']* "if"  { TIfdefIf (tokinfo lexbuf) }
  | "#" [' ' '\t']* "elif"  { TIfdefElif (tokinfo lexbuf) }
  | "#" [' ' '\t']* "else"  { TIfdefElse (tokinfo lexbuf) }
  | "#" [' ' '\t']* "endif"  { TIfdefEndif (tokinfo lexbuf) }


  (* ----------------------------------------------------------------------- *)
  (* symbols *)
  (* ----------------------------------------------------------------------- *)
  (*
   * { } [ ] ( )
   * . , : ;
   * + - * / % & | ^ ! ~
   * = < > ? ++ -- && || << >>
   *
   * == != <= >=
   * += -= *= /= %= &=
   * |= ^= <<= >>= ->
   *)

  | "(" { TOParen(tokinfo lexbuf) }  | ")" { TCParen(tokinfo lexbuf) }
  | "{" { TOBrace(tokinfo lexbuf) }  | "}" { TCBrace(tokinfo lexbuf) }
  | "[" { TOBracket(tokinfo lexbuf) }  | "]" { TCBracket(tokinfo lexbuf) }

  | "<<" { TOAngle(tokinfo lexbuf) }  | ">>" { TCAngle(tokinfo lexbuf) }


  | "+" { TPlus(tokinfo lexbuf) }  | "-" { TMinus(tokinfo lexbuf) }
  | "*" { TStar(tokinfo lexbuf) }  | "/" { TDiv(tokinfo lexbuf) }
  | "%" { TPercent(tokinfo lexbuf) }

  | "="  { TEq (tokinfo lexbuf) }
  | "=="  { TEqEq (tokinfo lexbuf) }
  | "!="  { TNotEq (tokinfo lexbuf) }
  | "<=" { TLessEq(tokinfo lexbuf) }  | ">=" { TMoreEq(tokinfo lexbuf) }
  | "<" { TLess(tokinfo lexbuf) }  | ">" { TMore(tokinfo lexbuf) }

  | "?" { TQuestion(tokinfo lexbuf) }

  | "++" { TInc(tokinfo lexbuf) }
  | "--" { TDec(tokinfo lexbuf) }

  | "!" { TBang(tokinfo lexbuf) }
  | "~" { TTilde(tokinfo lexbuf) }

  | "&" { TAnd(tokinfo lexbuf) }
  | "|" { TOr(tokinfo lexbuf) }
  | "^" { TXor(tokinfo lexbuf) }

  | "&&" { TAndAnd(tokinfo lexbuf) }
  | "||" { TOrOr(tokinfo lexbuf) }

  | "." { TDot(tokinfo lexbuf) }
  | "," { TComma(tokinfo lexbuf) }
  | ":" { TColon(tokinfo lexbuf) }
  | ";" { TSemiColon(tokinfo lexbuf) }

  | "->" { TArrow(tokinfo lexbuf) }

  | ("+=" | "-=" | "*=" | "/=" | "%=" | "&="
     "|=" | "^=" | "<<=" | ">>=")
   { TAssignOp (tok lexbuf, tokinfo lexbuf) }

  (* ----------------------------------------------------------------------- *)
  (* Keywords and ident *)
  (* ----------------------------------------------------------------------- *)
  | ident {
      let info = tokinfo lexbuf in
      let s = tok lexbuf in
      match Common2.optionise (fun () -> Hashtbl.find keyword_table s) with
      | Some f -> f info
      | None -> TIdent (s, info)
    }

  (* ----------------------------------------------------------------------- *)
  (* Constant *)
  (* ----------------------------------------------------------------------- *)
  | integer { TInt (tok lexbuf, tokinfo lexbuf) }

  (* ----------------------------------------------------------------------- *)
  (* Strings *)
  (* ----------------------------------------------------------------------- *)

  | '"' {
      let info = tokinfo lexbuf in
      let s = string_double_quote lexbuf in
      TString (s, info |> Parse_info.tok_add_s (s ^ "\""))
    }

  (* ----------------------------------------------------------------------- *)
  (* Misc *)
  (* ----------------------------------------------------------------------- *)

  (* ----------------------------------------------------------------------- *)
  (* eof *)
  (* ----------------------------------------------------------------------- *)

  | eof { EOF (tokinfo lexbuf) }

  | _ {
      if !Flag.verbose_lexing
      then pr2_once ("LEXER:unrecognised symbol, in token rule:"^tok lexbuf);
      TUnknown (tokinfo lexbuf)
    }

(*****************************************************************************)

and comment = parse
  | "*/"     { tok lexbuf }
  (* noteopti: *)
  | [^ '*']+ { let s = tok lexbuf in s ^ comment lexbuf }
  | [ '*']   { let s = tok lexbuf in s ^ comment lexbuf }
  | _
      { let s = tok lexbuf in
        pr2 ("LEXER: unrecognised symbol in comment:"^s);
        s ^ comment lexbuf
      }
  | eof { pr2 "LEXER: WIERD end of file in comment"; ""}


and string_double_quote = parse
  | '"' { "" }

  | [^ '\"' '\n']* { let s = tok lexbuf in s ^ string_double_quote lexbuf }
  | escapeseq { let s = tok lexbuf in s ^ string_double_quote lexbuf }


  | eof { pr2 "LEXER: end of file in string_double_quote"; "'"}
  | _  { let s = tok lexbuf in
         pr2 ("LEXER: unrecognised symbol in string_double_quote:"^s);
         s ^ string_double_quote lexbuf
    }
