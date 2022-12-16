{
(* Yoann Padioleau
 *
 * Copyright (C) 2014 Facebook
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

module Ast = Ast_rust
module Flag = Flag_parsing

open Parser_rust

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * A Lexer for Rust.
 *
 * Inspiration:
 *  - lang_csharp/ (itself inspired from lang_python/, lang_cpp/, ...)
 *  - http://doc.rust-lang.org/rust.html#lexical-structure
 *  - TODO: rust/boot/fe/lexer.mll in https://github.com/rust-lang/rust before they
 *    removed it from the repo in 6997adf76342b7a6fe03c4bc370ce5fc5082a869
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
(* shortcuts *)
let tok = Lexing.lexeme
let tokinfo = Parse_info.tokinfo
let error = Parse_info.lexical_error

(* ---------------------------------------------------------------------- *)
(* http://doc.rust-lang.org/rust.html#keywords *)
let keyword_table = Common.hash_of_list [
  "as", (fun ii -> Tas ii);
  "box", (fun ii -> Tbox ii);
  "break", (fun ii -> Tbreak ii);
  "continue", (fun ii -> Tcontinue ii);
  "crate", (fun ii -> Tcrate ii);
  "else", (fun ii -> Telse ii);
  "enum", (fun ii -> Tenum ii);
  "extern", (fun ii -> Textern ii);
  "false", (fun ii -> Tfalse ii);
  "fn", (fun ii -> Tfn ii);
  "for", (fun ii -> Tfor ii);
  "if", (fun ii -> Tif ii);
  "impl", (fun ii -> Timpl ii);
  "in", (fun ii -> Tin ii);
  "let", (fun ii -> Tlet ii);
  "loop", (fun ii -> Tloop ii);
  "match", (fun ii -> Tmatch ii);
  "mod", (fun ii -> Tmod ii);
  "mut", (fun ii -> Tmut ii);
  "priv", (fun ii -> Tpriv ii);
  "proc", (fun ii -> Tproc ii);
  "pub", (fun ii -> Tpub ii);
  "ref", (fun ii -> Tref ii);
  "return", (fun ii -> Treturn ii);
  "self", (fun ii -> Tself ii);
  "static", (fun ii -> Tstatic ii);
  "struct", (fun ii -> Tstruct ii);
  "super", (fun ii -> Tsuper ii);
  "true", (fun ii -> Ttrue ii);
  "trait", (fun ii -> Ttrait ii);
  "type", (fun ii -> Ttype ii);
  "unsafe", (fun ii -> Tunsafe ii);
  "use", (fun ii -> Tuse ii);
  "while", (fun ii -> Twhile ii);
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


let ident = (letter | '_') (letter | digit | '_')*

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

  (* ----------------------------------------------------------------------- *)
  (* symbols *)
  (* ----------------------------------------------------------------------- *)
  (*
   * http://doc.rust-lang.org/rust.html#symbols
   *)

  | "(" { TOParen(tokinfo lexbuf) }  | ")" { TCParen(tokinfo lexbuf) }
  | "{" { TOBrace(tokinfo lexbuf) }  | "}" { TCBrace(tokinfo lexbuf) }
  | "[" { TOBracket(tokinfo lexbuf) }  | "]" { TCBracket(tokinfo lexbuf) }


  | "+" { TPlus(tokinfo lexbuf) }  | "-" { TMinus(tokinfo lexbuf) }
  | "*" { TStar(tokinfo lexbuf) }  | "/" { TDiv(tokinfo lexbuf) }
  | "%" { TPercent(tokinfo lexbuf) }

  | "&" { TAnd(tokinfo lexbuf) } | "|" { TOr(tokinfo lexbuf) }
  | "^" { TXor(tokinfo lexbuf) }
  | "<<" { TOAngle(tokinfo lexbuf) }  | ">>" { TCAngle(tokinfo lexbuf) }

  | "&&" { TAndAnd(tokinfo lexbuf) } | "||" { TOrOr(tokinfo lexbuf) }

  | "=="  { TEqEq (tokinfo lexbuf) }
  | "!="  { TNotEq (tokinfo lexbuf) }
  | "<" { TLess(tokinfo lexbuf) }  | ">" { TMore(tokinfo lexbuf) }
  | "<=" { TLessEq(tokinfo lexbuf) }  | ">=" { TMoreEq(tokinfo lexbuf) }

  | "="  { TEq (tokinfo lexbuf) }


  | "#" { TPound(tokinfo lexbuf) }
  | "->" { TArrow(tokinfo lexbuf) }
  | "." { TDot(tokinfo lexbuf) }
  | "::" { TColonColon(tokinfo lexbuf) }
  | ":" { TColon(tokinfo lexbuf) }
  | "," { TComma(tokinfo lexbuf) }
  | ";" { TSemiColon(tokinfo lexbuf) }


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
