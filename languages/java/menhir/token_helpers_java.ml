(* Copyright (C) 2008 Yoann Padioleau
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
 *)
open Parser_java
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

let is_just_comment = function
  | TComment _ -> true
  | _ -> false

let token_kind_of_tok t =
  match t with
  | LC _ -> PI.LBrace
  | RC _ -> PI.RBrace
  | LP _
  | LP_LAMBDA _ ->
      PI.LPar
  | RP _ -> PI.RPar
  | LB _ -> PI.LBracket
  | RB _ -> PI.RBracket
  | TComment _ -> PI.Esthet PI.Comment
  | TCommentSpace _ -> PI.Esthet PI.Space
  | TCommentNewline _ -> PI.Esthet PI.Newline
  | _ -> PI.Other

(*****************************************************************************)
(* Visitors *)
(*****************************************************************************)

(* Because ocamlyacc force us to do it that way. The ocamlyacc token
 * cant be a pair of a sum type, it must be directly a sum type.
 *)

(* used by tokens to complete the parse_info with filename, line, col infos *)
let visitor_info_of_tok f = function
  | TUnknown ii -> TUnknown (f ii)
  | TComment ii -> TComment (f ii)
  | TCommentSpace ii -> TCommentSpace (f ii)
  | TCommentNewline ii -> TCommentNewline (f ii)
  | TInt pi -> TInt (Parsed_int.map_tok f pi)
  | TFloat (s, ii) -> TFloat (s, f ii)
  | TChar (s, ii) -> TChar (s, f ii)
  | TString (s, ii) -> TString (s, f ii)
  | TRUE ii -> TRUE (f ii)
  | FALSE ii -> FALSE (f ii)
  | NULL ii -> NULL (f ii)
  | IDENTIFIER (id, ii) -> IDENTIFIER (id, f ii)
  | PRIMITIVE_TYPE (s, ii) -> PRIMITIVE_TYPE (s, f ii)
  | OPERATOR_EQ (op, ii) -> OPERATOR_EQ (op, f ii)
  | COLONCOLON ii -> COLONCOLON (f ii)
  (* 3.11 Separators *)
  | LP ii -> LP (f ii)
  | RP ii -> RP (f ii)
  | LC ii -> LC (f ii)
  | RC ii -> RC (f ii)
  | LB ii -> LB (f ii)
  | LB_RB ii -> LB_RB (f ii)
  | RB ii -> RB (f ii)
  | SM ii -> SM (f ii)
  | CM ii -> CM (f ii)
  | DOT ii -> DOT (f ii)
  (* 3.12 Operators *)
  | EQ ii -> EQ (f ii)
  | GT ii -> GT (f ii)
  | LT ii -> LT (f ii)
  | LT_GENERIC ii -> LT_GENERIC (f ii)
  | LP_LAMBDA ii -> LP_LAMBDA (f ii)
  | LP_PARAM ii -> LP_PARAM (f ii)
  | NOT ii -> NOT (f ii)
  | COMPL ii -> COMPL (f ii)
  | COND ii -> COND (f ii)
  | COLON ii -> COLON (f ii)
  | EQ_EQ ii -> EQ_EQ (f ii)
  | LE ii -> LE (f ii)
  | GE ii -> GE (f ii)
  | NOT_EQ ii -> NOT_EQ (f ii)
  | AND_AND ii -> AND_AND (f ii)
  | OR_OR ii -> OR_OR (f ii)
  | INCR ii -> INCR (f ii)
  | DECR ii -> DECR (f ii)
  | PLUS ii -> PLUS (f ii)
  | MINUS ii -> MINUS (f ii)
  | TIMES ii -> TIMES (f ii)
  | DIV ii -> DIV (f ii)
  | AND ii -> AND (f ii)
  | OR ii -> OR (f ii)
  | XOR ii -> XOR (f ii)
  | MOD ii -> MOD (f ii)
  | LS ii -> LS (f ii)
  | SRS ii -> SRS (f ii)
  | URS ii -> URS (f ii)
  | AT ii -> AT (f ii)
  | DOTS ii -> DOTS (f ii)
  | LDots ii -> LDots (f ii)
  | RDots ii -> RDots (f ii)
  | ARROW ii -> ARROW (f ii)
  | VAR ii -> VAR (f ii)
  | ABSTRACT ii -> ABSTRACT (f ii)
  | BREAK ii -> BREAK (f ii)
  | CASE ii -> CASE (f ii)
  | CATCH ii -> CATCH (f ii)
  | CLASS ii -> CLASS (f ii)
  | CONST ii -> CONST (f ii)
  | CONTINUE ii -> CONTINUE (f ii)
  | DEFAULT ii -> DEFAULT (f ii)
  | DEFAULT_COLON ii -> DEFAULT_COLON (f ii)
  | DO ii -> DO (f ii)
  | ELSE ii -> ELSE (f ii)
  | EXTENDS ii -> EXTENDS (f ii)
  | FINAL ii -> FINAL (f ii)
  | FINALLY ii -> FINALLY (f ii)
  | FOR ii -> FOR (f ii)
  | GOTO ii -> GOTO (f ii)
  | IF ii -> IF (f ii)
  | IMPLEMENTS ii -> IMPLEMENTS (f ii)
  | IMPORT ii -> IMPORT (f ii)
  | INSTANCEOF ii -> INSTANCEOF (f ii)
  | INTERFACE ii -> INTERFACE (f ii)
  | NATIVE ii -> NATIVE (f ii)
  | NEW ii -> NEW (f ii)
  | PACKAGE ii -> PACKAGE (f ii)
  | PRIVATE ii -> PRIVATE (f ii)
  | PROTECTED ii -> PROTECTED (f ii)
  | PUBLIC ii -> PUBLIC (f ii)
  | RETURN ii -> RETURN (f ii)
  | STATIC ii -> STATIC (f ii)
  | STRICTFP ii -> STRICTFP (f ii)
  | SUPER ii -> SUPER (f ii)
  | SWITCH ii -> SWITCH (f ii)
  | SYNCHRONIZED ii -> SYNCHRONIZED (f ii)
  | THIS ii -> THIS (f ii)
  | THROW ii -> THROW (f ii)
  | THROWS ii -> THROWS (f ii)
  | TRANSIENT ii -> TRANSIENT (f ii)
  | TRY ii -> TRY (f ii)
  | VOID ii -> VOID (f ii)
  | VOLATILE ii -> VOLATILE (f ii)
  | WHILE ii -> WHILE (f ii)
  | ASSERT ii -> ASSERT (f ii)
  | ENUM ii -> ENUM (f ii)
  | RECORD ii -> RECORD (f ii)
  | EOF ii -> EOF (f ii)
  (* sgrep-ext: *)
  | METAVAR_ELLIPSIS (id, ii) -> METAVAR_ELLIPSIS (id, f ii)

let info_of_tok tok =
  let res = ref None in
  visitor_info_of_tok
    (fun ii ->
      res := Some ii;
      ii)
    tok
  |> ignore;
  match !res with
  | Some x -> x
  | None -> Tok.unsafe_fake_tok "NOTOK"

(*****************************************************************************)
(* Accessors *)
(*****************************************************************************)

(* todo: remove, just use by parse_java.ml checkpoint mechanism that
 * we actually don't use
 *)
let line_of_tok tok =
  let info = info_of_tok tok in
  Tok.line_of_tok info
