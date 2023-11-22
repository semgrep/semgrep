(* Yoann Padioleau
 *
 * Copyright (C) 2010 Facebook
 * Copyright (C) 2019 Yoann Padioleau
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
open Parser_go
module PI = Lib_ast_fuzzy

(*****************************************************************************)
(* Token Helpers *)
(*****************************************************************************)
let is_eof = function
  | EOF _ -> true
  | _ -> false

let is_irrelevant = function
  | TComment _
  | TCommentSpace _
  | TCommentNewline _ ->
      true
  | _ -> false

let is_comment_or_space = function
  | TComment _
  | TCommentSpace _ ->
      true
  | _ -> false

let token_kind_of_tok t =
  match t with
  | LBRACE _
  | LBODY _ ->
      PI.LBrace
  | RBRACE _ -> PI.RBrace
  | LPAREN _ -> PI.LPar
  | RPAREN _ -> PI.RPar
  | LBRACKET _ -> PI.LBracket
  | RBRACKET _ -> PI.RBracket
  | TComment _ -> PI.Esthet PI.Comment
  | TCommentSpace _ -> PI.Esthet PI.Space
  | TCommentNewline _ -> PI.Esthet PI.Newline
  | _ -> PI.Other

(*****************************************************************************)
(* Visitors *)
(*****************************************************************************)

let visitor_info_of_tok f = function
  | LBRACE_SEMGREP ii -> LBRACE_SEMGREP (f ii)
  | LCOLON_SEMGREP ii -> LCOLON_SEMGREP (f ii)
  | LPAREN_SEMGREP ii -> LPAREN_SEMGREP (f ii)
  | TUnknown ii -> TUnknown (f ii)
  | EOF ii -> EOF (f ii)
  | TCommentSpace ii -> TCommentSpace (f ii)
  | TComment ii -> TComment (f ii)
  | TCommentNewline ii -> TCommentNewline (f ii)
  | LINT pi -> LINT (Parsed_int.map_tok f pi)
  | LFLOAT (s, ii) -> LFLOAT (s, f ii)
  | LIMAG (s, ii) -> LIMAG (s, f ii)
  | LRUNE (s, ii) -> LRUNE (s, f ii)
  | LSTR (s, ii) -> LSTR (s, f ii)
  | LASOP (s, ii) -> LASOP (s, f ii)
  | LNAME (s, ii) -> LNAME (s, f ii)
  | LIF ii -> LIF (f ii)
  | LELSE ii -> LELSE (f ii)
  | LFOR ii -> LFOR (f ii)
  | LRETURN ii -> LRETURN (f ii)
  | LBREAK ii -> LBREAK (f ii)
  | LCONTINUE ii -> LCONTINUE (f ii)
  | LFALL ii -> LFALL (f ii)
  | LSWITCH ii -> LSWITCH (f ii)
  | LCASE ii -> LCASE (f ii)
  | LDEFAULT ii -> LDEFAULT (f ii)
  | LGOTO ii -> LGOTO (f ii)
  | LFUNC ii -> LFUNC (f ii)
  | LCONST ii -> LCONST (f ii)
  | LVAR ii -> LVAR (f ii)
  | LTYPE ii -> LTYPE (f ii)
  | LSTRUCT ii -> LSTRUCT (f ii)
  | LINTERFACE ii -> LINTERFACE (f ii)
  | LGO ii -> LGO (f ii)
  | LCHAN ii -> LCHAN (f ii)
  | LSELECT ii -> LSELECT (f ii)
  | LDEFER ii -> LDEFER (f ii)
  | LPACKAGE ii -> LPACKAGE (f ii)
  | LIMPORT ii -> LIMPORT (f ii)
  | LMAP ii -> LMAP (f ii)
  | LRANGE ii -> LRANGE (f ii)
  | LPAREN ii -> LPAREN (f ii)
  | RPAREN ii -> RPAREN (f ii)
  | LBRACE ii -> LBRACE (f ii)
  | RBRACE ii -> RBRACE (f ii)
  | LBRACKET ii -> LBRACKET (f ii)
  | RBRACKET ii -> RBRACKET (f ii)
  | LCOLON ii -> LCOLON (f ii)
  | LSEMICOLON ii -> LSEMICOLON (f ii)
  | LEQ ii -> LEQ (f ii)
  | LDOT ii -> LDOT (f ii)
  | LCOMMA ii -> LCOMMA (f ii)
  | LCOLAS ii -> LCOLAS (f ii)
  | LDDD ii -> LDDD (f ii)
  | LDots ii -> LDots (f ii)
  | RDots ii -> RDots (f ii)
  | LPLUS ii -> LPLUS (f ii)
  | LMINUS ii -> LMINUS (f ii)
  | LMULT ii -> LMULT (f ii)
  | LDIV ii -> LDIV (f ii)
  | LPERCENT ii -> LPERCENT (f ii)
  | LPIPE ii -> LPIPE (f ii)
  | LAND ii -> LAND (f ii)
  | LHAT ii -> LHAT (f ii)
  | LANDAND ii -> LANDAND (f ii)
  | LOROR ii -> LOROR (f ii)
  | LANDNOT ii -> LANDNOT (f ii)
  | LBODY ii -> LBODY (f ii)
  | LCOMM ii -> LCOMM (f ii)
  | LINC ii -> LINC (f ii)
  | LDEC ii -> LDEC (f ii)
  | LEQEQ ii -> LEQEQ (f ii)
  | LGE ii -> LGE (f ii)
  | LGT ii -> LGT (f ii)
  | LLE ii -> LLE (f ii)
  | LLT ii -> LLT (f ii)
  | LNE ii -> LNE (f ii)
  | LLSH ii -> LLSH (f ii)
  | LRSH ii -> LRSH (f ii)
  | LBANG ii -> LBANG (f ii)

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
