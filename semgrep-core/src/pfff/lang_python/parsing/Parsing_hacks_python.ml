(* Yoann Padioleau
 *
 * Copyright (C) 2019-2021 r2c
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

module T = Parser_python

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* The goal for this module is to add extra "closing" tokens
 * for the grammar to remain simple. The closing tokens
 * are usually one NEWLINE and a few DEDENTs.
 *
 * This file:
 *  class A:
 *     def f():
 *        bar()
 *
 * is tokenized as (skipping the TCommentSpace):
 *  [class; A; :; NEWLINE; INDENT;
 *   def; f; (; ); :; NEWLINE; IDENT;
 *   bar; (; ); NEWLINE; #1
 *   DEDENT; DEDENT; #2
 *   NEWLINE; EOF
 *
 * Instead if bar() is replaced by '...', many .pyi files do not have
 * the trailing NEWLINE which causes some missing DEDENT which would
 * cause parsing errors. This is why for those files we must
 * insert the NEWLINE at #1, and matching DEDENT at #2.
 *
 * alt:
 *  - could insert those closing tokens during error recovery
 *  - could look at state.offset_stack when encounters EOF in the lexer
 *    and also pop and create the DEDENT.
*)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let rec add_dedent_aux num ii xs =
  if num <= 0
  then xs
  else T.DEDENT ii::add_dedent_aux (num - 1) ii xs

let add_dedent num ii xs =
  if num <= 0
  then xs
  (* this closes the small_stmt from the stmt_list in suite (see grammar)
   * which then can be reduced by the series of DEDENT created by
   * add_dedent_aux.
  *)
  else T.NEWLINE ii::add_dedent_aux num ii xs

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)
let fix_tokens toks =
  let rec aux indent xs =
    match xs with
    | [T.NEWLINE ii; T.EOF _] -> add_dedent indent ii xs
    | [T.EOF ii] -> add_dedent indent ii [T.NEWLINE ii; T.EOF ii]
    | [] -> raise Common.Impossible
    | x::xs ->
        let new_indent =
          match x with
          | T.INDENT _ -> indent + 1
          | T.DEDENT _ -> indent - 1
          | _ -> indent
        in
        x::aux new_indent xs
  in
  aux 0 toks
