(* Yoann Padioleau
 *
 * Copyright (C) 2002-2008 Yoann Padioleau
 * Copyright (C) 2011 Facebook
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
open Common
open Parser_cpp
module Flag = Flag_parsing
module TH = Token_helpers_cpp
module Hack = Parsing_hacks_lib

(*****************************************************************************)
(* Prelude  *)
(*****************************************************************************)
(*
 * To parse macro definitions I need to do some tricks
 * as some information can be computed only at the lexing level. For instance
 * the space after the name of the macro in '#define foo (x)' is meaningful
 * but the grammar does not have this information. So define_ident() below
 * look at such space and generate a special TOpar_Define token.
 *
 * In a similar way macro definitions can contain some antislash and newlines
 * and the grammar need to know where the macro ends which is
 * a line-level and so low token-level information. Hence the
 * function define_line'()below and the TCommentNewline_DefineEndOfMacro.
 *
 * update: TCommentNewline_DefineEndOfMacro is handled in a special way
 * at different places, a little bit like EOF, especially for error recovery,
 * so this is an important token that should not be retagged!
 *
 * We also change the kind of TIdent to TIdent_Define to avoid bad interactions
 * with other parsing_hack tricks. For instant if keep TIdent then
 * the stringication heuristics can believe the TIdent is a string-macro.
 * So simpler to change the kind of the TIdent in a macro too.
 *
 * ugly: maybe a better solution perhaps would be to erase
 * TCommentNewline_DefineEndOfMacro from the Ast and list of tokens in parse_c.
 *
 * note: I do a +1 somewhere, it's for the unparsing to correctly sync.
 *
 * note: can't replace mark_end_define by simply a fakeInfo(). The reason
 * is where is the \n TCommentSpace. Normally there is always a last token
 * to synchronize on, either EOF or the token of the next toplevel.
 * In the case of the #define we got in list of token
 * [TCommentSpace "\n"; TDefEOL] but if TDefEOL is a fakeinfo then we will
 * not synchronize on it and so we will not print the "\n".
 * A solution would be to put the TDefEOL before the "\n".
 *
 * todo?: could put a ExpandedTok for that ?
 *)

(*****************************************************************************)
(* Wrappers *)
(*****************************************************************************)
let pr2, _pr2_once = Common2.mk_pr2_wrappers Flag.verbose_lexing

(*****************************************************************************)
(* Helpers  *)
(*****************************************************************************)

let mark_end_define ii =
  let tok_loc = Tok.unsafe_loc_of_tok ii in
  let ii' =
    Tok.OriginTok
      {
        str = "";
        pos = { tok_loc.pos with charpos = Tok.bytepos_of_tok ii + 1 };
      }
  in
  (* fresh_tok *) TCommentNewline_DefineEndOfMacro ii'

let pos ii = Tok.stringpos_of_tok ii

(*****************************************************************************)
(* Parsing hacks for #define *)
(*****************************************************************************)

(* simple automata:
 * state1 --'#define'--> state2 --change_of_line--> state1
 *)

(* put the TCommentNewline_DefineEndOfMacro at the good place
 * and replace \ with TCommentSpace
 *)
let rec define_line_1 acc xs =
  (* Tail-recursive to prevent stack overflows. *)
  match xs with
  | [] -> List.rev acc
  | (TDefine ii as x) :: xs ->
      let line = Tok.line_of_tok ii in
      define_line_2 (x :: acc) line ii xs
  | TCppEscapedNewline ii :: xs ->
      pr2 (spf "WEIRD: a \\ outside a #define at %s" (pos ii));
      define_line_1 ((* fresh_tok*) TCommentSpace ii :: acc) xs
  | x :: xs -> define_line_1 (x :: acc) xs

and define_line_2 acc line lastinfo xs =
  (* Tail-recursive to prevent stack overflows. *)
  match xs with
  | [] ->
      (* should not happened, should meet EOF before *)
      pr2 "PB: WEIRD in Parsing_hack_define.define_line_2";
      List.rev (mark_end_define lastinfo :: acc)
  | x :: xs -> (
      let line' = TH.line_of_tok x in
      let info = TH.info_of_tok x in

      match x with
      | EOF ii -> define_line_1 (EOF ii :: mark_end_define lastinfo :: acc) xs
      | TCppEscapedNewline ii ->
          if line' <> line then pr2 "PB: WEIRD: not same line number";
          define_line_2
            ((* fresh_tok*) TCommentSpace ii :: acc)
            (line + 1) info xs
      | x ->
          if line' =|= line then define_line_2 (x :: acc) line info xs
          else define_line_1 (mark_end_define lastinfo :: acc) (x :: xs))

let define_line xs = define_line_1 [] xs

(* put the TIdent_Define and TOPar_Define *)
let define_ident xs =
  (* Tail-recursive to prevent stack overflows. *)
  let rec aux acc xs =
    match xs with
    | [] -> List.rev acc
    | (TDefine ii as x) :: xs -> (
        match xs with
        | (TCommentSpace _ as x1)
          :: TIdent (s, i2)
          :: (* no space *) TOPar i3
          :: xs ->
            (* if TOPar_Define is just next to the ident (no space), then
             * it's a macro-function. We change the token to avoid
             * ambiguity between '#define foo(x)'  and   '#define foo   (x)'
             *)
            let acc' =
              Hack.fresh_tok (TOPar_Define i3)
              :: Hack.fresh_tok (TIdent_Define (s, i2))
              :: x1 :: x :: acc
            in
            aux acc' xs
        | (TCommentSpace _ as x1) :: TIdent (s, i2) :: xs ->
            let acc' =
              Hack.fresh_tok (TIdent_Define (s, i2)) :: x1 :: x :: acc
            in
            aux acc' xs
        | _ ->
            pr2 (spf "WEIRD #define body, at %s" (pos ii));
            aux (x :: acc) xs)
    | x :: xs -> aux (x :: acc) xs
  in
  aux [] xs

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let fix_tokens_define xs = define_ident (define_line xs) [@@profiling]
