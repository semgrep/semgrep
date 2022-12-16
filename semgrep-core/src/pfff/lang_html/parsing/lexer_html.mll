{
(* Patrick Doane and Gerd Stolpmann
 *
 * Copyright (C) 2001-2006 Patrick Doane and Gerd Stolpmann
 * Copyright (C) 2011 Facebook
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

open Parser_html
module Ast = Ast_html

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * src: many of the code in this file comes from ocamlnet/netstring/.
 * The original CVS ID is:
 * $Id: nethtml_scanner.mll 1219 2009-04-14 13:28:56Z ChriS $
 *
 * I've extended it to add position information. I've also simplified the code
 * by leveraging the fact that we can call the lexing rule recursively
 * so one does not need the LeftComment, RightComment, MiddleComment
 * and so on extra tokens.
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* shortcuts *)
let tok = Lexing.lexeme
let tokinfo = Parse_info.tokinfo
let error = Parse_info.lexical_error
let tok_add_s = Parse_info.tok_add_s

}

(*****************************************************************************)

(* Simplified rules: Only ASCII is recognized as character set *)

let letter = ['A'-'Z' 'a'-'z' ]
let digit = ['0'-'9']
let hexdigit = ['0'-'9' 'A'-'F' 'a'-'f']
let ws = [ ' ' '\t' '\r' '\n' ]

let namechar = letter | digit | '.' | ':' | '-' | '_'
let name = ( letter | '_' | ':' ) namechar*
let nmtoken = namechar+

let string_literal1 = '"' [^ '"' ]* '"'
let string_literal2 = "'" [^ '\'' ]* "'"
let string_literal3 = [^ '"' '\'' '>' '=' ' ' '\t' '\n' '\r' ]+
let string_literal4 = [^ '"' '\'' '>' ' ' '\t' '\n' '\r' ]+

(*****************************************************************************)

(* This following rules reflect HTML as it is used, not the SGML rules. *)

(* starting point *)
rule scan_document = parse
  | "<!--" {
      let info = tokinfo lexbuf in
      let com = scan_comment lexbuf in
      TComment(info |> tok_add_s com)
    }
  | "<!"  {
      let info = tokinfo lexbuf in
      let com = scan_doctype lexbuf in
      TDoctype(info |> tok_add_s com)
    }
  (* pi stands for processing elements, it is obsolete in HTML 5
   * see http://www.w3.org/TR/2011/WD-html5-diff-20110113/#syntax
   *)
  | "<?" {
      let info = tokinfo lexbuf in
      let com = scan_pi lexbuf in
      TPi(info |> tok_add_s com)
    }

  | "<" (name as s)  { Lelement (tokinfo lexbuf, s) }
  | "</" (name as s) { Lelementend (tokinfo lexbuf, s) }
  | "<" (* misplaced "<" *) {
      error ("wrong < token ") lexbuf;
      Cdata (tokinfo lexbuf, "<")
    }
  | [^ '<' ]+               { Cdata (tokinfo lexbuf, tok lexbuf) }

  | eof { EOF (tokinfo lexbuf) }

(*****************************************************************************)
and scan_comment = parse
  (* FIXME: There may be any number of ws between -- and > *)
  | "-->"  { tok lexbuf }
  | "-"       { let s = tok lexbuf in s ^ scan_comment lexbuf }
  | [^ '-']+  { let s = tok lexbuf in s ^ scan_comment lexbuf }

  | eof { pr2 "LEXER: end of file in comment"; "-->" }

and scan_doctype = parse
  (* Occurence in strings, and [ ] brackets ignored *)
  | ">"         { tok lexbuf }
  | [^ '>' ]+   { let s = tok lexbuf in s ^ scan_doctype lexbuf }

  | eof { pr2 "LEXER: end of file in comment"; ">" }

and scan_pi = parse
  | "?>"   { tok lexbuf }
  | ">"    { tok lexbuf }
  | '?'           { let s = tok lexbuf in s ^ scan_pi lexbuf }
  | [^ '>' '?' ]+ {  let s = tok lexbuf in s ^ scan_pi lexbuf }
  | eof   { pr2 "LEXER: end of file in comment"; ">" }

(*****************************************************************************)
and scan_element = parse
  | ">"     { Relement (tokinfo lexbuf) }
  | "/>"    { Relement_empty (tokinfo lexbuf) }
  | ws+     { Space (tokinfo lexbuf, tok lexbuf) }
  | name    { Name (tokinfo lexbuf, tok lexbuf) }
  | "="     { Eq (tokinfo lexbuf) }
  | '"'     { Other (tokinfo lexbuf) }
  | "'"     { Other (tokinfo lexbuf) }
  | string_literal3 { Literal (tokinfo lexbuf, tok lexbuf) }
  | _       { Other (tokinfo lexbuf) }
  | eof     { EOF (tokinfo lexbuf) }

(*****************************************************************************)
(* ??? *)
and scan_element_after_Eq = parse
  | ">"     { Relement (tokinfo lexbuf) }
  | "/>"    { Relement_empty (tokinfo lexbuf) }
  | ws+     { Space (tokinfo lexbuf, tok lexbuf) }
  | '"'    ( [^ '"' ]* as s) '"'
      {  Literal (tokinfo lexbuf, s) }
  | '"' { Other (tokinfo lexbuf) }
  | "'" ( [^ '\'' ]* as s) '\''
      { Literal (tokinfo lexbuf, s) }
  | "'" { Other (tokinfo lexbuf) }

  | string_literal4 { Literal ((tokinfo lexbuf), tok lexbuf) }
  | _               { Other (tokinfo lexbuf) }
  | eof             { EOF (tokinfo lexbuf) }

(* for <script> and <style> tags, see Parse_html.parse_special,
 * look for a </script> or </style>.
 * todo: what if the js code contains the string "</script>" ??
 *)
and scan_special = parse
  | "</" (name as s) { Lelementend (tokinfo lexbuf, s) }
  | "<"              { CdataSpecial (tokinfo lexbuf, "<") }
  | [^ '<' ]+        { CdataSpecial (tokinfo lexbuf, tok lexbuf) }

  | eof { EOF (tokinfo lexbuf) }
