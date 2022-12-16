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

module Ast = Ast_erlang
module Flag = Flag_parsing

open Parser_erlang

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * http://www.erlang.org/download/erl_spec47.ps.gz appendix E
 * and erlang-otp/lib/compiler/src/core_scan.erl
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
  (* real keywords *)
  "if",      (fun ii -> Tif ii);
  "cond",    (fun ii -> Tcond ii);
  "when",    (fun ii -> Twhen ii);
  "case",    (fun ii -> Tcase ii);
  "begin",   (fun ii -> Tbegin ii);
  "end",     (fun ii -> Tend ii);
  "let",     (fun ii -> Tlet ii);
  "of",      (fun ii -> Tof  ii);
  "fun",     (fun ii -> Tfun ii);
  "after",   (fun ii -> Tafter ii);
  "query",   (fun ii -> Tquery ii);
  "catch",   (fun ii -> Tcatch ii);
  "receive", (fun ii -> Treceive ii);

  (* operators *)
  "div",  (fun ii -> Tdiv ii);
  "rem",  (fun ii -> Trem ii);
  "or",   (fun ii -> Tor ii);
  "xor",  (fun ii -> Txor ii);
  "bor",  (fun ii -> Tbor ii);
  "bxor", (fun ii -> Tbxor ii);
  "bsl",  (fun ii -> Tbsl ii);
  "bsr",  (fun ii -> Tbsr ii);
  "and",  (fun ii -> Tand ii);
  "band", (fun ii -> Tband ii);
  "not",  (fun ii -> Tnot ii);
  "bnot", (fun ii -> Tbnot ii);
]

}
(*****************************************************************************)

let letter = ['A'-'Z' 'a'-'z']
let digit  = ['0'-'9']

let lowercase = ['a'-'z']
let uppercase = ['A'-'Z']

let newline = '\n'
let space = [' ' '\t']

let nonzerodigit = ['1'-'9']
let octdigit = ['0'-'7']
let hexdigit = digit | ['a'-'f'] | ['A'-'F']

(* TODO, was copied from python *)
let decimalinteger = nonzerodigit digit* | '0'
let octinteger = '0' octdigit+
let hexinteger = '0' ('x' | 'X') hexdigit+

let integer = (decimalinteger | octinteger | hexinteger)

(* TODO, was in csharp *)
let escapeseq =
   ( '\\' '^' _ |
    '\\' ['\'' '"' '\\' 'b' 'd' 'e' 'f' 'n' 'r' 's' 't' 'v'] |
    '\\' octdigit |
    '\\' octdigit octdigit |
    '\\' octdigit octdigit octdigit
   )

let namechars = (letter | digit | '@' | '_')+

let atom = lowercase namechars*
let variable = (uppercase namechars* | '_' namechars+)

(*****************************************************************************)

rule token = parse

  (* ----------------------------------------------------------------------- *)
  (* spacing/comments *)
  (* ----------------------------------------------------------------------- *)

  | "%" [^ '\n']* { TComment (tokinfo lexbuf) }

  | newline { TCommentNewline (tokinfo lexbuf) }
  | space+ { TCommentSpace (tokinfo lexbuf) }

  (* ----------------------------------------------------------------------- *)
  (* symbols *)
  (* ----------------------------------------------------------------------- *)

  | "(" { TOParen(tokinfo lexbuf) }  | ")" { TCParen(tokinfo lexbuf) }
  | "{" { TOBrace(tokinfo lexbuf) }  | "}" { TCBrace(tokinfo lexbuf) }
  | "[" { TOBracket(tokinfo lexbuf) }  | "]" { TCBracket(tokinfo lexbuf) }

  | "." { TDot(tokinfo lexbuf) }
  | ":" { TColon(tokinfo lexbuf) }
  | ";" { TSemiColon(tokinfo lexbuf) }
  | "," { TComma(tokinfo lexbuf) }
  | "?" { TQuestion(tokinfo lexbuf) }

  | "|" { TPipe(tokinfo lexbuf) }
  | "||" { TPipe(tokinfo lexbuf) }
  | "->" { TArrow(tokinfo lexbuf) }
  | "#" { TSharp(tokinfo lexbuf) }

  | "+" { TPlus(tokinfo lexbuf) }  | "-" { TMinus(tokinfo lexbuf) }
  | "*" { TStar(tokinfo lexbuf) }  | "/" { TDiv(tokinfo lexbuf) }

  | "="  { TEq (tokinfo lexbuf) } | "==" { TEqEq(tokinfo lexbuf) }
  | "/=" { TSlashEq(tokinfo lexbuf) }
  | "=:="  { TEqColonEq (tokinfo lexbuf) }
  | "=/=" { TEqSlashEq(tokinfo lexbuf) }
  | "<" { TLess(tokinfo lexbuf) }  | ">" { TMore(tokinfo lexbuf) }
  | "=<" { TLessEq(tokinfo lexbuf) }  | ">=" { TMoreEq(tokinfo lexbuf) }

  | "++" { TInc(tokinfo lexbuf) }
  | "--" { TDec(tokinfo lexbuf) }

  | "!" { TBang(tokinfo lexbuf) }
  | "<-" { TAssign(tokinfo lexbuf) }

  (* ----------------------------------------------------------------------- *)
  (* Keywords and ident *)
  (* ----------------------------------------------------------------------- *)
  | atom {
      let info = tokinfo lexbuf in
      let s = tok lexbuf in
      match Common2.optionise (fun () -> Hashtbl.find keyword_table s) with
      | Some f -> f info
      | None -> TIdent (s, info)
    }
  | variable { TVariable (tok lexbuf, tokinfo lexbuf) }

  | '_' { TUnderscore (tokinfo lexbuf) }

  (* ----------------------------------------------------------------------- *)
  (* Constant *)
  (* ----------------------------------------------------------------------- *)
  | integer { TInt (tok lexbuf, tokinfo lexbuf) }

  (* TODO: TChar ? TFloat ? *)

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

and string_double_quote = parse
  | '"' { "" }

  | [^ '\\' '\"' '\n']* {
      let s = tok lexbuf in s ^ string_double_quote lexbuf
    }
  | escapeseq { let s = tok lexbuf in s ^ string_double_quote lexbuf }


  | eof { pr2 "LEXER: end of file in string_double_quote"; "'"}
  | _  { let s = tok lexbuf in
         pr2 ("LEXER: unrecognised symbol in string_double_quote:"^s);
         s ^ string_double_quote lexbuf
    }
