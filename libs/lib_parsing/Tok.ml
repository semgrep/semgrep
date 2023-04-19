(* Yoann Padioleau
 *
 * Copyright (C) 2010 Facebook
 * Copyright (C) 2023 r2c
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

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Information about tokens (mostly their location).
 *
 * The main types are:
 * 'token_location' < 'token_origin' < 'token_mutable'
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)
(* ('token_location' < 'token_origin' < 'token_mutable') * token_kind *)

(* to report errors, regular position information *)
type token_location = {
  str : string; (* the content of the "token" *)
  pos : Pos.t;
}
[@@deriving show { with_path = false }, eq]

(* to deal with expanded tokens, e.g. preprocessor like cpp for C *)
type token_origin =
  (* Present both in the AST and list of tokens *)
  | OriginTok of token_location
  (* Present only in the AST and generated after parsing. Can be used
   * when building some extra AST elements. *)
  | FakeTokStr of
      string (* to help the generic pretty printer *)
      * (* Sometimes we generate fake tokens close to existing
         * origin tokens. This can be useful when have to give an error
         * message that involves a fakeToken. The int is a kind of
         * virtual position, an offset. See compare_pos below.
         * Those are called "safe" fake tokens (in contrast to the
         * regular/unsafe one which have no position information at all).
         *)
      (token_location * int) option
  (* In the case of a XHP file, we could preprocess it and incorporate
   * the tokens of the preprocessed code with the tokens from
   * the original file. We want to mark those "expanded" tokens
   * with a special tag so that if someone do some transformation on
   * those expanded tokens they will get a warning (because we may have
   * trouble back-propagating the transformation back to the original file).
   *)
  | ExpandedTok of
      (* refers to the preprocessed file, e.g. /tmp/pp-xxxx.pphp *)
      token_location
      * (* kind of virtual position. This info refers to the last token
         * before a serie of expanded tokens and the int is an offset.
         * The goal is to be able to compare the position of tokens
         * between then, even for expanded tokens. See compare_pos
         * below.
         *)
        token_location
      * int
  (* The Ab constructor is (ab)used to call '=' to compare
   * big AST portions. Indeed as we keep the token information in the AST,
   * if we have an expression in the code like "1+1" and want to test if
   * it's equal to another code like "1+1" located elsewhere, then
   * the Pervasives.'=' of OCaml will not return true because
   * when it recursively goes down to compare the leaf of the AST, that is
   * the token_location, there will be some differences of positions. If instead
   * all leaves use Ab, then there is no position information and we can
   * use '='. See also the 'al_info' function below.
   *
   * Ab means AbstractLineTok. I Use a short name to not
   * polluate in debug mode.
   *)
  | Ab
[@@deriving show { with_path = false }, eq]

(* to allow source to source transformation via token "annotations",
 * see the documentation for spatch.
 *)
type token_mutable = {
  (* contains among other things the position of the token through
   * the token_location embedded inside the token_origin type.
   *)
  token : token_origin;
  (* for spatch *)
  mutable transfo : transformation; (* less: mutable comments: ...; *)
}

(* poor's man refactoring *)
and transformation =
  | NoTransfo
  | Remove
  | AddBefore of add
  | AddAfter of add
  | Replace of add
  | AddArgsBefore of string list

and add = AddStr of string | AddNewlineAndIdent
[@@deriving show { with_path = false }, eq]

(* Shortcut.
 * Technically speaking this is not a token, because we do not have
 * the kind of the token (e.g., PLUS | IDENT | IF | ...).
 * It's just a lexeme, but the word lexeme is not as known as token.
 *)
type t = token_mutable [@@deriving eq]

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*
(* for ppx_deriving *)
val pp_full_token_info : bool ref
val pp : Format.formatter -> t -> unit
*)

(* see -full_token_info in meta_parse_info.ml *)
let pp_full_token_info = ref false

(* for ppx_deriving *)
let pp fmt t =
  if !pp_full_token_info then pp_token_mutable fmt t
  else Format.fprintf fmt "()"
