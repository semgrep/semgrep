(* Yoann Padioleau
 *
 * Copyright (C) 2002-2008 Yoann Padioleau
 * Copyright (C) 2011 Facebook
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License (GPL)
 * version 2 as published by the Free Software Foundation.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * file license.txt for more details.
 *)
open Common

open Parser_cpp

module Flag = Flag_parsing
module PI = Parse_info

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
  let ii' = 
    { Parse_info.
      token = Parse_info.OriginTok { 
        (Parse_info.token_location_of_info ii) with 
          Parse_info.str = ""; 
          Parse_info.charpos = PI.pos_of_info ii + 1
      };
      transfo = Parse_info.NoTransfo;
    } 
  in
  (* fresh_tok *) TCommentNewline_DefineEndOfMacro (ii')

let pos ii = Parse_info.string_of_info ii

(*****************************************************************************)
(* Parsing hacks for #define *)
(*****************************************************************************)

(* simple automata:
 * state1 --'#define'--> state2 --change_of_line--> state1
 *)

(* put the TCommentNewline_DefineEndOfMacro at the good place
 * and replace \ with TCommentSpace
 *)
let rec define_line_1 xs = 
  match xs with
  | [] -> []
  | (TDefine ii as x)::xs -> 
      let line = PI.line_of_info ii in
      x::define_line_2 line ii xs
  | TCppEscapedNewline ii::xs -> 
      pr2 (spf "WEIRD: a \\ outside a #define at %s" (pos ii));
      (* fresh_tok*) TCommentSpace ii::define_line_1 xs
  | x::xs -> 
      x::define_line_1 xs

and define_line_2 line lastinfo xs = 
  match xs with 
  | [] -> 
      (* should not happened, should meet EOF before *)
      pr2 "PB: WEIRD in Parsing_hack_define.define_line_2";
      mark_end_define lastinfo::[]
  | x::xs -> 
      let line' = TH.line_of_tok x in
      let info = TH.info_of_tok x in

      (match x with
      | EOF ii -> 
          mark_end_define lastinfo::EOF ii::define_line_1 xs
      | TCppEscapedNewline ii -> 
          if (line' <> line) 
          then pr2 "PB: WEIRD: not same line number";
          (* fresh_tok*) TCommentSpace ii::define_line_2 (line+1) info xs
      | x ->
          if line' = line
          then x::define_line_2 line info xs 
          else 
            mark_end_define lastinfo::define_line_1 (x::xs)
      )

(* put the TIdent_Define and TOPar_Define *)
let rec define_ident xs = 
  match xs with
  | [] -> []
  | (TDefine ii as x)::xs -> 
      x::
      (match xs with
      | (TCommentSpace _ as x)::TIdent (s,i2)::(* no space *)TOPar (i3)::xs -> 
          (* if TOPar_Define is just next to the ident (no space), then
           * it's a macro-function. We change the token to avoid
           * ambiguity between '#define foo(x)'  and   '#define foo   (x)'
           *)
          x
          ::Hack.fresh_tok (TIdent_Define (s,i2))
          ::Hack.fresh_tok (TOPar_Define i3)
          ::define_ident xs

      | (TCommentSpace _ as x)::TIdent (s,i2)::xs -> 
          x
          ::Hack.fresh_tok (TIdent_Define (s,i2))
          ::define_ident xs
      | _ -> 
          pr2 (spf "WEIRD #define body, at %s" (pos ii)); 
          define_ident xs
      )
  | x::xs -> 
      x::define_ident xs

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let fix_tokens_define2 xs = 
  define_ident (define_line_1 xs)

let fix_tokens_define a = 
  Common.profile_code "Hack.fix_define" (fun () -> fix_tokens_define2 a)
