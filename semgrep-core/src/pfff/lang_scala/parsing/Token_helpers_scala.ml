(* Yoann Padioleau
 *
 * Copyright (C) 2021 R2C
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

open Token_scala
module PI = Parse_info

let logger = Logging.get_logger [__MODULE__]

(*****************************************************************************)
(* Token Helpers *)
(*****************************************************************************)

let is_eof = function
  | EOF _ -> true
  | _ -> false

let is_comment = function
  | Comment _ | Space _ -> true
  (* newline has a meaning in the parser, so should not skip *)
  (* old: | Nl _ -> true *)
  | _ -> false

let token_kind_of_tok t =
  match t with
  | LBRACE _ -> PI.LBrace
  | RBRACE _ -> PI.RBrace
  | LPAREN _ -> PI.LPar
  | RPAREN _ -> PI.RPar
  | LBRACKET _ -> PI.LBracket
  | RBRACKET _ -> PI.RBracket

  | Comment _ -> PI.Esthet PI.Comment
  | Space _ -> PI.Esthet PI.Space
  | Nl _ -> PI.Esthet PI.Newline

  | _ -> PI.Other

(*****************************************************************************)
(* Visitors *)
(*****************************************************************************)

let visitor_info_of_tok f = function
  (* tokens with values *)
  (* old:
     | InterpolatedString(ii) -> InterpolatedString(f ii)
     | InterpStart(ii) -> InterpStart(f ii)
  *)
  | T_INTERPOLATED_START(s, ii) -> T_INTERPOLATED_START(s, f ii)
  | T_INTERPOLATED_STRING(s, ii) -> T_INTERPOLATED_STRING(s, f ii)

  | ID_DOLLAR(s, ii) -> ID_DOLLAR(s, f ii)
  | ID_LOWER(s, ii) -> ID_LOWER(s, f ii)
  | ID_UPPER(s, ii) -> ID_UPPER(s, f ii)
  | ID_BACKQUOTED(s, ii) -> ID_BACKQUOTED(s, f ii)
  | OP(s, ii) -> OP(s, f ii)

  | IntegerLiteral(x, ii) -> IntegerLiteral(x, f ii)
  | FloatingPointLiteral(x, ii) -> FloatingPointLiteral(x, f ii)
  | CharacterLiteral(x, ii) -> CharacterLiteral(x, f ii)
  | BooleanLiteral(x, ii) -> BooleanLiteral(x, f ii)
  | SymbolLiteral(ii1, (s, ii2)) ->
      let ii1' = f ii1 in
      let ii2' = f ii2 in
      SymbolLiteral(ii1', (s, ii2'))
  | StringLiteral(x, ii) -> StringLiteral(x, f ii)

  (* tokens without values *)
  | T_INTERPOLATED_END(ii) -> T_INTERPOLATED_END(f ii)
  | T_DOLLAR_LBRACE(ii) -> T_DOLLAR_LBRACE(f ii)

  | Unknown(ii) -> Unknown(f ii)
  | EOF(ii) -> EOF(f ii)

  | Space(ii) -> Space(f ii)
  | Nl(ii) -> Nl(f ii)
  | NEWLINE (ii) -> NEWLINE (f ii)
  | NEWLINES (ii) -> NEWLINES (f ii)
  | Comment(ii) -> Comment(f ii)

  | USCORE(ii) -> USCORE(f ii)
  | TILDE(ii) -> TILDE(f ii)
  | STAR(ii) -> STAR(f ii)
  | HASH(ii) -> HASH(f ii)
  | SEMI(ii) -> SEMI(f ii)

  | LPAREN(ii) -> LPAREN(f ii)
  | LBRACKET(ii) -> LBRACKET(f ii)
  | LBRACE(ii) -> LBRACE(f ii)
  | RPAREN(ii) -> RPAREN(f ii)
  | RBRACKET(ii) -> RBRACKET(f ii)
  | RBRACE(ii) -> RBRACE(f ii)
  | RDots(ii) -> RDots(f ii)
  | LDots(ii) -> LDots(f ii)

  | PLUS(ii) -> PLUS(f ii)
  | PIPE(ii) -> PIPE(f ii)
  | SUPERTYPE(ii) -> SUPERTYPE(f ii)
  | MINUS(ii) -> MINUS(f ii)
  | VIEWBOUND(ii) -> VIEWBOUND(f ii)
  | LARROW(ii) -> LARROW(f ii)
  | SUBTYPE(ii) -> SUBTYPE(f ii)
  | ARROW(ii) -> ARROW(f ii)
  | EQUALS(ii) -> EQUALS(f ii)
  | BANG(ii) -> BANG(f ii)
  | AT(ii) -> AT(f ii)
  | DOT(ii) -> DOT(f ii)
  | COMMA(ii) -> COMMA(f ii)
  | COLON(ii) -> COLON(f ii)

  | Kyield(ii) -> Kyield(f ii)
  | Kwith(ii) -> Kwith(f ii)
  | Kwhile(ii) -> Kwhile(f ii)
  | Kvar(ii) -> Kvar(f ii)
  | Kval(ii) -> Kval(f ii)
  | Ktype(ii) -> Ktype(f ii)
  | Ktry(ii) -> Ktry(f ii)
  | Ktrait(ii) -> Ktrait(f ii)
  | Kthrow(ii) -> Kthrow(f ii)
  | Kthis(ii) -> Kthis(f ii)
  | Ksuper(ii) -> Ksuper(f ii)
  | Ksealed(ii) -> Ksealed(f ii)
  | Kreturn(ii) -> Kreturn(f ii)
  | Kprotected(ii) -> Kprotected(f ii)
  | Kprivate(ii) -> Kprivate(f ii)
  | Kpackage(ii) -> Kpackage(f ii)
  | Koverride(ii) -> Koverride(f ii)
  | Kobject(ii) -> Kobject(f ii)
  | Knull(ii) -> Knull(f ii)
  | Knew(ii) -> Knew(f ii)
  | Kmatch(ii) -> Kmatch(f ii)
  | Klazy(ii) -> Klazy(f ii)
  | Kimport(ii) -> Kimport(f ii)
  | Kimplicit(ii) -> Kimplicit(f ii)
  | Kif(ii) -> Kif(f ii)
  | KforSome(ii) -> KforSome(f ii)
  | Kfor(ii) -> Kfor(f ii)
  | Kfinally(ii) -> Kfinally(f ii)
  | Kfinal(ii) -> Kfinal(f ii)
  | Kextends(ii) -> Kextends(f ii)
  | Kelse(ii) -> Kelse(f ii)
  | Kdo(ii) -> Kdo(f ii)
  | Kdef(ii) -> Kdef(f ii)
  | Kclass(ii) -> Kclass(f ii)
  | Kcatch(ii) -> Kcatch(f ii)
  | Kcase(ii) -> Kcase(f ii)
  | Kabstract(ii) -> Kabstract(f ii)

  | Ellipsis(ii) -> Ellipsis(f ii)



let info_of_tok tok =
  let res = ref None in
  visitor_info_of_tok (fun ii -> res := Some ii; ii) tok |> ignore;
  Common2.some !res

let abstract_info_tok tok =
  visitor_info_of_tok (fun _ -> PI.abstract_info) tok

(*****************************************************************************)
(* More token Helpers for Parse_scala_recursive_descent.ml *)
(*****************************************************************************)

(* ------------------------------------------------------------------------- *)
(* Just used in the tokenizer (for lexing tricks for newline) *)
(* ------------------------------------------------------------------------- *)

(** Can token start a statement? *)
let inFirstOfStat x =
  match x with
  | EOF _
  | Kcatch _ | Kelse _ | Kextends _ | Kfinally _
  | KforSome _ | Kmatch _ | Kwith _ | Kyield _
  | COMMA _ | SEMI _
  | NEWLINE _ | NEWLINES _
  | DOT _ | COLON _ | EQUALS _ | ARROW _
  | LARROW _
  | SUBTYPE _  | VIEWBOUND _ | SUPERTYPE _
  | HASH _
  | RPAREN _ | RBRACKET _ | RBRACE _
  | LBRACKET _ ->
      false
  | _ ->
      logger#info "inFirstOfStat: true for %s" (Common.dump x);
      true

(** Can token end a statement? *)
let inLastOfStat x =
  match x with
  | CharacterLiteral _ | IntegerLiteral _ | FloatingPointLiteral _
  | StringLiteral _ | T_INTERPOLATED_END _
  | SymbolLiteral _

  (* coupling: less: use isIdent? *)
  | ID_LOWER _ | ID_UPPER _ | ID_BACKQUOTED _
  | OP _
  | STAR _ | PLUS _ | MINUS _ | BANG _ | TILDE _ | PIPE _

  | Kthis _
  | Knull _
  | BooleanLiteral _
  | Kreturn _
  | USCORE _
  | Ktype _
  (* less: | XMLSTART  *)
  | RPAREN _ | RBRACKET _ | RBRACE _
  (* semgrep-ext: *)
  | Ellipsis _ | RDots _
    ->
      logger#info "inLastOfStat: true for %s" (Common.dump x);
      true
  | _ -> false

(* ------------------------------------------------------------------------- *)
(* Used in the parser *)
(* ------------------------------------------------------------------------- *)

let isIdent = function
  | ID_LOWER (s, info) | ID_UPPER(s, info)
  | ID_BACKQUOTED (s, info)
  | OP (s, info)
  (* when in interpolated string *)
  | ID_DOLLAR (s, info)
    ->
      Some (s, info)
  (* need that?? *)
  | STAR info -> Some ("*", info)
  | PLUS info -> Some ("+", info)
  | MINUS info -> Some ("-", info)
  | BANG info -> Some ("!", info)
  | TILDE info -> Some ("~", info)
  | PIPE info -> Some ("|", info)
  (* TODO? HASH *)

  | _ -> None

let isIdentBool x =
  isIdent x <> None

let isRawIdent = function
  | ID_LOWER (s, info) | ID_UPPER (s, info) | OP (s, info)
    (* less: and STAR | ... ? maybe not actually *)
    -> Some (s, info)
  | _ -> None

let isRawStar x =
  (* pad: in original code
     match isRawIdent x with
     | Some (s, _) -> s = "*" (* AST: raw.STAR *)
     | _ -> false
  *)
  match x with STAR _ -> true | _ -> false

let isRawBar x =
  (* pad: in original code
     match isRawIdent x with
     | Some (s, _) -> s = "|" (* AST: raw.STAR *)
     | _ -> false
  *)
  match x with PIPE _ -> true | _ -> false

(* ------------------------------------------------------------------------- *)
(* Literals *)
(* ------------------------------------------------------------------------- *)

let isLiteral = function
  | IntegerLiteral(_)
  | FloatingPointLiteral(_)
  | CharacterLiteral(_)
  | BooleanLiteral(_)
  | SymbolLiteral(_)
  | StringLiteral(_)
  (* ?? *)
  | T_INTERPOLATED_START _
  | Knull _
    -> true
  | _ -> false

let isNull = function
  | Knull _ -> true
  | _ -> false

let isNumericLit = function
  | IntegerLiteral(_)
  | FloatingPointLiteral(_)
    -> true
  | _ -> false

(* ------------------------------------------------------------------------- *)
(* Statement separators *)
(* ------------------------------------------------------------------------- *)

let isStatSep = function
  | NEWLINE _ | NEWLINES _ | SEMI _ -> true
  | _ -> false

let isStatSeqEnd = function
  | RBRACE _ | EOF _ -> true
  | _ -> false

(* ------------------------------------------------------------------------- *)
(* modifiers *)
(* ------------------------------------------------------------------------- *)

let isAnnotation = function
  | AT _ -> true
  | _ -> false

let isModifier = function
  | Kabstract _ | Kfinal _ | Ksealed _
  | Kprivate _  | Kprotected _ | Koverride _
  | Kimplicit _ | Klazy _ -> true
  | _ -> false

(* coupling: modifier_of_isLocalModifier_opt *)
let isLocalModifier = function
  | Kabstract _ | Kfinal _ | Ksealed _
  | Kimplicit _ | Klazy _ -> true
  | _ -> false

(* ------------------------------------------------------------------------- *)
(* Construct Intro *)
(* ------------------------------------------------------------------------- *)

let isTemplateIntro = function
  | Kobject _ | Kclass _  | Ktrait _  -> true
  (*TODO | Kcaseobject | | Kcaseclass *) | Kcase _ -> true
  | _ -> false

let isDclIntro = function
  | Kval _ | Kvar _ | Kdef _ | Ktype _ -> true
  | _ -> false

let isExprIntro x =
  isIdentBool x || isLiteral x ||
  (match x with
   | Kthis _ | Ksuper _ | Kif _ | Kfor _ | Knew _
   | Ktry _ | Kwhile _
   | Kdo _ | Kreturn _ | Kthrow _
   | USCORE _
   | LPAREN _ | LBRACE  _
   (* | XMLSTART  *)
   (* semgrep-ext: *)
   | Ellipsis _ | LDots _
     -> true
   | _ -> false
  )

let isDefIntro x =
  isTemplateIntro x || isDclIntro x

let isTypeIntroToken x =
  (isLiteral x && not (isNull x)) ||
  (* pad: was IDENTIFIER | BACKQUOTED_IDENT *)
  isIdentBool x ||
  (match x with
   | Kthis _ | Ksuper _ | USCORE _ | LPAREN _ | AT _ -> true
   | _ -> false
  )


(* ------------------------------------------------------------------------- *)
(* Misc *)
(* ------------------------------------------------------------------------- *)

let isCaseDefEnd = function
  | RBRACE _ | Kcase _ | EOF _ -> true
  | _ -> false

(*
let raw_isUnary _s =
  Common.pr2_once "TODO: raw_isUnary";
  false

let isUnaryOp x =
  match isIdent x with
  | None -> false
  | Some (s, _) -> raw_isUnary s
*)
let isUnaryOp = function
  | MINUS _ | PLUS _ | TILDE _ | BANG _ -> true
  | _ -> false

(* TODO? correct? *)
let nme_MACROkw = "macro"

let isMacro x =
  match isIdent x with
  | None -> false
  | Some (s, _) -> s = nme_MACROkw

let isWildcardType = function
  (* TODO: scala3 also accept '?' *)
  | USCORE _ -> true
  | _ -> false

(* pad: not in original code, hence the _ instead of camlCase *)
let is_stringpart = function
  | StringLiteral _
  | ID_DOLLAR _
  | LBRACE _
    -> true
  | _ -> false
