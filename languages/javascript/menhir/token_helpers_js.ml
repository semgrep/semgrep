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
open Parser_js
module PI = Lib_ast_fuzzy

(*****************************************************************************)
(* Token Helpers *)
(*****************************************************************************)

let is_eof = function
  | EOF _ -> true
  | _ -> false

let is_comment = function
  | TComment _
  | TCommentSpace _
  | TCommentNewline _ ->
      true
  | _ -> false

let token_kind_of_tok t =
  match t with
  | T_LCURLY _
  | T_DOLLARCURLY _ ->
      PI.LBrace
  | T_RCURLY _ -> PI.RBrace
  | T_LPAREN _ -> PI.LPar
  | T_RPAREN _ -> PI.RPar
  | T_LBRACKET _ -> PI.LBracket
  | T_RBRACKET _ -> PI.RBracket
  (* note: if at some point you want to add < > do not forget to also
   * handle T_XHP_OPEN_TAG
   *)
  | TComment _ -> PI.Esthet PI.Comment
  | TCommentSpace _ -> PI.Esthet PI.Space
  | TCommentNewline _ -> PI.Esthet PI.Newline
  | _ -> PI.Other

(*****************************************************************************)
(* Visitors *)
(*****************************************************************************)

(* generated via an Emacs macro from the type definition in parser_js.mli *)
let visitor_info_of_tok f = function
  | T_AT ii -> T_AT (f ii)
  | TUnknown ii -> TUnknown (f ii)
  | TCommentSpace ii -> TCommentSpace (f ii)
  | TCommentNewline ii -> TCommentNewline (f ii)
  | TComment ii -> TComment (f ii)
  | EOF ii -> EOF (f ii)
  | T_INT (s, ii) -> T_INT (s, f ii)
  | T_FLOAT (s, ii) -> T_FLOAT (s, f ii)
  | T_ID (s, ii) -> T_ID (s, f ii)
  | T_STRING (s, ii) -> T_STRING (s, f ii)
  | T_REGEX ((li, (s1, ii1), ri), opt) ->
      let li' = f li in
      let ii1' = f ii1 in
      let ri' = f ri in
      let opt =
        match opt with
        | None -> None
        | Some (s, ii) -> Some (s, f ii)
      in
      T_REGEX ((li', (s1, ii1'), ri'), opt)
  | T_FUNCTION ii -> T_FUNCTION (f ii)
  | T_IF ii -> T_IF (f ii)
  | T_IN ii -> T_IN (f ii)
  | T_INSTANCEOF ii -> T_INSTANCEOF (f ii)
  | T_RETURN ii -> T_RETURN (f ii)
  | T_SWITCH ii -> T_SWITCH (f ii)
  | T_THIS ii -> T_THIS (f ii)
  | T_THROW ii -> T_THROW (f ii)
  | T_TRY ii -> T_TRY (f ii)
  | T_VAR ii -> T_VAR (f ii)
  | T_WHILE ii -> T_WHILE (f ii)
  | T_WITH ii -> T_WITH (f ii)
  | T_CONST ii -> T_CONST (f ii)
  | T_NULL ii -> T_NULL (f ii)
  | T_FALSE ii -> T_FALSE (f ii)
  | T_TRUE ii -> T_TRUE (f ii)
  | T_BREAK ii -> T_BREAK (f ii)
  | T_CASE ii -> T_CASE (f ii)
  | T_CATCH ii -> T_CATCH (f ii)
  | T_CONTINUE ii -> T_CONTINUE (f ii)
  | T_DEFAULT ii -> T_DEFAULT (f ii)
  | T_DO ii -> T_DO (f ii)
  | T_FINALLY ii -> T_FINALLY (f ii)
  | T_FOR ii -> T_FOR (f ii)
  | T_ELSE ii -> T_ELSE (f ii)
  | T_NEW ii -> T_NEW (f ii)
  | T_LCURLY ii -> T_LCURLY (f ii)
  | T_RCURLY ii -> T_RCURLY (f ii)
  | T_LPAREN ii -> T_LPAREN (f ii)
  | T_LPAREN_ARROW ii -> T_LPAREN_ARROW (f ii)
  | T_LPAREN_METHOD_SEMGREP ii -> T_LPAREN_METHOD_SEMGREP (f ii)
  | T_LCURLY_SEMGREP ii -> T_LCURLY_SEMGREP (f ii)
  | T_RPAREN ii -> T_RPAREN (f ii)
  | T_LBRACKET ii -> T_LBRACKET (f ii)
  | T_RBRACKET ii -> T_RBRACKET (f ii)
  | T_SEMICOLON ii -> T_SEMICOLON (f ii)
  | T_COMMA ii -> T_COMMA (f ii)
  | T_PERIOD ii -> T_PERIOD (f ii)
  | T_RSHIFT3_ASSIGN ii -> T_RSHIFT3_ASSIGN (f ii)
  | T_RSHIFT_ASSIGN ii -> T_RSHIFT_ASSIGN (f ii)
  | T_LSHIFT_ASSIGN ii -> T_LSHIFT_ASSIGN (f ii)
  | T_BIT_XOR_ASSIGN ii -> T_BIT_XOR_ASSIGN (f ii)
  | T_BIT_OR_ASSIGN ii -> T_BIT_OR_ASSIGN (f ii)
  | T_BIT_AND_ASSIGN ii -> T_BIT_AND_ASSIGN (f ii)
  | T_MOD_ASSIGN ii -> T_MOD_ASSIGN (f ii)
  | T_DIV_ASSIGN ii -> T_DIV_ASSIGN (f ii)
  | T_MULT_ASSIGN ii -> T_MULT_ASSIGN (f ii)
  | T_MINUS_ASSIGN ii -> T_MINUS_ASSIGN (f ii)
  | T_PLUS_ASSIGN ii -> T_PLUS_ASSIGN (f ii)
  | T_ASSIGN ii -> T_ASSIGN (f ii)
  | T_PLING ii -> T_PLING (f ii)
  | T_QUESTDOT ii -> T_QUESTDOT (f ii)
  | T_COLON ii -> T_COLON (f ii)
  | T_OR ii -> T_OR (f ii)
  | T_AND ii -> T_AND (f ii)
  | T_BIT_OR ii -> T_BIT_OR (f ii)
  | T_BIT_XOR ii -> T_BIT_XOR (f ii)
  | T_BIT_AND ii -> T_BIT_AND (f ii)
  | T_EQUAL ii -> T_EQUAL (f ii)
  | T_NOT_EQUAL ii -> T_NOT_EQUAL (f ii)
  | T_STRICT_EQUAL ii -> T_STRICT_EQUAL (f ii)
  | T_STRICT_NOT_EQUAL ii -> T_STRICT_NOT_EQUAL (f ii)
  | T_LESS_THAN_EQUAL ii -> T_LESS_THAN_EQUAL (f ii)
  | T_GREATER_THAN_EQUAL ii -> T_GREATER_THAN_EQUAL (f ii)
  | T_LESS_THAN ii -> T_LESS_THAN (f ii)
  | T_GREATER_THAN ii -> T_GREATER_THAN (f ii)
  | T_LSHIFT ii -> T_LSHIFT (f ii)
  | T_RSHIFT ii -> T_RSHIFT (f ii)
  | T_RSHIFT3 ii -> T_RSHIFT3 (f ii)
  | T_PLUS ii -> T_PLUS (f ii)
  | T_MINUS ii -> T_MINUS (f ii)
  | T_DIV ii -> T_DIV (f ii)
  | T_MULT ii -> T_MULT (f ii)
  | T_MOD ii -> T_MOD (f ii)
  | T_NOT ii -> T_NOT (f ii)
  | T_BIT_NOT ii -> T_BIT_NOT (f ii)
  | T_INCR ii -> T_INCR (f ii)
  | T_DECR ii -> T_DECR (f ii)
  | T_DELETE ii -> T_DELETE (f ii)
  | T_TYPEOF ii -> T_TYPEOF (f ii)
  | T_VOID ii -> T_VOID (f ii)
  | T_VIRTUAL_SEMICOLON ii -> T_VIRTUAL_SEMICOLON (f ii)
  | T_CLASS ii -> T_CLASS (f ii)
  | T_EXTENDS ii -> T_EXTENDS (f ii)
  | T_STATIC ii -> T_STATIC (f ii)
  | T_INTERFACE ii -> T_INTERFACE (f ii)
  | T_XHP_OPEN_TAG (s, ii) -> T_XHP_OPEN_TAG (s, f ii)
  | T_XHP_CLOSE_TAG (s, ii) -> T_XHP_CLOSE_TAG (s, f ii)
  | T_XHP_GT ii -> T_XHP_GT (f ii)
  | T_XHP_SLASH_GT ii -> T_XHP_SLASH_GT (f ii)
  | T_XHP_ATTR (s, ii) -> T_XHP_ATTR (s, f ii)
  | T_XHP_TEXT (s, ii) -> T_XHP_TEXT (s, f ii)
  | T_ARROW ii -> T_ARROW (f ii)
  | T_XHP_SHORT_FRAGMENT ii -> T_XHP_SHORT_FRAGMENT (f ii)
  | T_DOTS ii -> T_DOTS (f ii)
  | LDots ii -> LDots (f ii)
  | RDots ii -> RDots (f ii)
  | T_DOLLARCURLY ii -> T_DOLLARCURLY (f ii)
  | T_BACKQUOTE ii -> T_BACKQUOTE (f ii)
  | T_ENCAPSED_STRING (s, ii) -> T_ENCAPSED_STRING (s, f ii)
  | T_LET ii -> T_LET (f ii)
  | T_YIELD ii -> T_YIELD (f ii)
  | T_ASYNC ii -> T_ASYNC (f ii)
  | T_AWAIT ii -> T_AWAIT (f ii)
  | T_SUPER ii -> T_SUPER (f ii)
  | T_IMPORT ii -> T_IMPORT (f ii)
  | T_EXPORT ii -> T_EXPORT (f ii)
  | T_FROM ii -> T_FROM (f ii)
  | T_AS ii -> T_AS (f ii)
  | T_OF ii -> T_OF (f ii)
  | T_GET ii -> T_GET (f ii)
  | T_SET ii -> T_SET (f ii)
  | T_EXPONENT ii -> T_EXPONENT (f ii)
  | T_IMPLEMENTS ii -> T_IMPLEMENTS (f ii)
  | T_CONSTRUCTOR ii -> T_CONSTRUCTOR (f ii)
  | T_TYPE ii -> T_TYPE (f ii)
  | T_ANY_TYPE ii -> T_ANY_TYPE (f ii)
  | T_NUMBER_TYPE ii -> T_NUMBER_TYPE (f ii)
  | T_BOOLEAN_TYPE ii -> T_BOOLEAN_TYPE (f ii)
  | T_STRING_TYPE ii -> T_STRING_TYPE (f ii)
  | T_ENUM ii -> T_ENUM (f ii)
  | T_DECLARE ii -> T_DECLARE (f ii)
  | T_MODULE ii -> T_MODULE (f ii)
  | T_PUBLIC ii -> T_PUBLIC (f ii)
  | T_PRIVATE ii -> T_PRIVATE (f ii)
  | T_PROTECTED ii -> T_PROTECTED (f ii)
  | T_READONLY ii -> T_READONLY (f ii)

let info_of_tok tok =
  let res = ref None in
  visitor_info_of_tok
    (fun ii ->
      res := Some ii;
      ii)
    tok
  |> ignore;
  Option.get !res

(*****************************************************************************)
(* Accessors *)
(*****************************************************************************)

let line_of_tok tok =
  let info = info_of_tok tok in
  Tok.line_of_tok info

let col_of_tok tok =
  let info = info_of_tok tok in
  Tok.col_of_tok info
