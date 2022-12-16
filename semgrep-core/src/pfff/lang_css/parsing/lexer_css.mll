{
(*
 * Copyright (c) 2010 Dario Teixeira (dario.teixeira@yahoo.com)
 * Copyright (C) 2011 Facebook
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in the license.txt file.
 *)
open Common

open Parser_css

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*
 * spec: http://www.w3.org/TR/CSS2/grammar.html
 *
 * Most of the code in this file is copy pasted from Dario Teixera
 * css parser and preprocessor: http://forge.ocamlcore.org/projects/ccss/.
 * I've mostly converted it from ulex to ocamllex and removed the line
 * number management which can be done in a better way outside
 * (like in the other pfff parsers).
 *
 * Note that this lexer handles spacing in an unusual way. This is
 * because the CSS grammar is space sensitive ...
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* shortcuts *)
let tok = Lexing.lexeme
let tokinfo = Parse_info.tokinfo
let error = Parse_info.lexical_error

(* ---------------------------------------------------------------------- *)

(*
 *let parse_quantity =
 * let rex =
 * Pcre.regexp "(?<number>(\\+|-)?[0-9]+(\\.[0-9]+)?)(?<units>%|[A-Za-z]+)?"
 * in fun lexbuf ->
 * let subs = Pcre.exec ~rex (Ulexing.utf8_lexeme lexbuf) in
 * let number = Pcre.get_named_substring rex "number" subs
 * and units = try Some (Pcre.get_named_substring rex "units" subs)
 *             with Not_found -> None
 * in (float_of_string number, units)
 *)
}

(*****************************************************************************)

let alpha = ['a'-'z']
let digit = ['0'-'9']
let hexa = ['0'-'9' 'a'-'f']
let space = [' ' '\t' '\n']

let ident = ['a'-'z' '-'] ['A'-'Z' 'a'-'z' '0'-'9' '-' '_']*
let variable = ['A'-'Z'] ['A'-'Z' 'a'-'z' '0'-'9' '-' '_']*
let hashed = '#' ['A'-'Z' 'a'-'z' '0'-'9' '-' '_']+

let number = ('-' | '+')? digit+ ('.' digit+)?
let units = alpha+ | '%'
let nth = ('-' | '+')? digit+ 'n' ('-' | '+') digit+

(*****************************************************************************)

rule token = parse

  (* ----------------------------------------------------------------------- *)
  (* spacing/comments *)
  (* ----------------------------------------------------------------------- *)

  (* note: this lexer generate tokens for comments, so you can not
   * give this lexer as-is to the parsing function. You must have an
   * intermediate layer that filter those tokens.
   *)
  | space* "/*" {
      let info = tokinfo lexbuf in
      let com = comment lexbuf in
      TComment(info |> Parse_info.tok_add_s com)
    }
  | space* "//" [^ '\n']+ space* { TComment (tokinfo lexbuf) }
  | space          { S (tokinfo lexbuf) }

  (* ----------------------------------------------------------------------- *)
  (* Keywords and ident *)
  (* ----------------------------------------------------------------------- *)

  (* pad: ?? why not separate tokens ? and resolve in grammar ? *)
  | "url("               { URI (tokinfo lexbuf) }
  | (ident as s) '('     { TERM_FUNC (s, tokinfo lexbuf) }
  | ':' (ident as s) '(' { SEL_FUNC  (s, tokinfo lexbuf) }

  | nth           { NTH (tok lexbuf, tokinfo lexbuf) }
  | number units? { QUANTITY ((*parse_quantity*) tok lexbuf, tokinfo lexbuf) }

  | ident        { IDENT (tok lexbuf, tokinfo lexbuf) }
  | variable     { VAR   (tok lexbuf, tokinfo lexbuf) }
  | hashed       { HASH  (tok lexbuf, tokinfo lexbuf) }

  | "@charset" space+   { CHARSET  (tokinfo lexbuf) }
  | "@import" space+    { IMPORT   (tokinfo lexbuf) }
  | "@media" space+     { MEDIA    (tokinfo lexbuf) }
  | "@page" space+      { PAGE     (tokinfo lexbuf) }
  | "@font-face" space+ { FONTFACE (tokinfo lexbuf) }
  | space* "!important" space* { IMPORTANT (tokinfo lexbuf) }

  (* ----------------------------------------------------------------------- *)
  (* symbols *)
  (* ----------------------------------------------------------------------- *)

  | "="  { ATTR_EQUALS    (tokinfo lexbuf) }
  | "~=" { ATTR_INCLUDES  (tokinfo lexbuf) }
  | "|=" { ATTR_DASHMATCH (tokinfo lexbuf) }
  | "^=" { ATTR_PREFIX    (tokinfo lexbuf) }
  | "$=" { ATTR_SUFFIX    (tokinfo lexbuf) }
  | "*=" { ATTR_SUBSTRING (tokinfo lexbuf) }

  (* 247 is the decimal Unicode codepoint for the division sign
   * | 247      { QUOTIENT }
   *)
  | space* "::" space* { DOUBLE_COLON (tokinfo lexbuf) }
  | space* '*' space* { ASTERISK      (tokinfo lexbuf) }
  | space* '/' space* { SLASH         (tokinfo lexbuf) }
  | space* '+' space* { PLUS          (tokinfo lexbuf) }
  | space* '-' space* { MINUS         (tokinfo lexbuf) }
  | space* '~' space* { TILDE         (tokinfo lexbuf) }
  | space* '>' space* { GT            (tokinfo lexbuf) }
  | space* '{' space* { OPEN_CURLY    (tokinfo lexbuf) }
  | space* '}' space* { CLOSE_CURLY   (tokinfo lexbuf) }
  | space* ';' space* { SEMICOLON     (tokinfo lexbuf) }
  | space* ':' space* { COLON         (tokinfo lexbuf) }
  | space* ',' space* { COMMA         (tokinfo lexbuf) }
  | space* '(' space* { OPEN_ROUND    (tokinfo lexbuf) }
  | space* ')' space* { CLOSE_ROUND   (tokinfo lexbuf) }

  | '.' { PERIOD (tokinfo lexbuf) }
  | '[' { OPEN_SQUARE (tokinfo lexbuf) }
  | ']' { CLOSE_SQUARE (tokinfo lexbuf) }

  (* ----------------------------------------------------------------------- *)
  (* Constant *)
  (* ----------------------------------------------------------------------- *)

  (* ----------------------------------------------------------------------- *)
  (* Strings *)
  (* ----------------------------------------------------------------------- *)

  | '\''  {
      let info = tokinfo lexbuf in
      let s = single_string lexbuf in
      TString (s, info |> Parse_info.tok_add_s (s ^ "\'"))
    }

  | '"' {
      let info = tokinfo lexbuf in
      let s = double_string lexbuf in
      TString (s, info |> Parse_info.tok_add_s (s ^ "\""))
    }

  (* ----------------------------------------------------------------------- *)
  (* Misc *)
  (* ----------------------------------------------------------------------- *)

  (* ----------------------------------------------------------------------- *)
  (* eof *)
  (* ----------------------------------------------------------------------- *)

  | eof { EOF (tokinfo lexbuf) }
  | _   { TUnknown (tokinfo lexbuf) }

(*****************************************************************************)
and single_string = parse
  | '\''  { "" }
  (* escaping ?? was not in original ... *)
  (* opti ? *)
  | _   { let s = tok lexbuf in s ^ single_string lexbuf }
  | eof {
      pr2 "LEXER: WEIRD end of file in single quoted string";
      ""
    }


and double_string = parse
  | '"'  { "" }
  (* escaping ?? was not in original ... *)
  (* opti ? *)
  | _   { let s = tok lexbuf in s ^ double_string lexbuf }
  | eof {
      pr2 "LEXER: WEIRD end of file in double quoted string";
      ""
    }

(*****************************************************************************)
and comment = parse
  | "*/" space* { tok lexbuf }
  (* was buggy in original I think *)
  | [^'*''/']+ { let s = tok lexbuf in s ^ comment lexbuf }
  | "*"     { let s = tok lexbuf in s ^ comment lexbuf }
  | "/"     { let s = tok lexbuf in s ^ comment lexbuf }
  | eof { pr2 "LEXER: end of file in comment"; "*)"}
  | _  {
      let s = tok lexbuf in
      pr2 ("LEXER: unrecognised symbol in comment:"^s);
      s ^ comment lexbuf
    }
