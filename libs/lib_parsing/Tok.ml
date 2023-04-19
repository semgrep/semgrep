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
(* Information about tokens (mostly their position and origin).
 *
 * Note that the types below are a bit complicated because we want
 * to represent "fake" and "expanded" tokens, as well as annotate tokens
 * with transformation. This is also partly because in many of the ASTs
 * and CSTs in Semgrep, including AST_generic.ml, we store the
 * tokens in the AST at the leaves, and abuse them to compute the range
 * of constructs.
 * alt: an alternative would be to use cleaner ASTs with range
 * (general location) information at every nodes, in which case we
 * would not need at least fake tokens.
 *
 * Technically speaking, 't' is not really a token, because the type does
 * not store the kind of the token (e.g., PLUS | IDENT | IF | ...), just its
 * content. It's really just a lexeme, but the word lexeme is not as
 * known as token.
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* to report errors, regular position information *)
type location = { str : string; (* the content of the "token" *) pos : Pos.t }
[@@deriving show { with_path = false }, eq]

(* to represent fake (e.g., fake semicolons in languages such as Javascript),
 * and expanded tokens (e.g. preprocessed constructs by cpp for C/C++)
 *)
type origin =
  (* Present both in the AST and list of tokens *)
  | OriginTok of location
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
      (location * int) option
  (* In the case of a XHP file, we could preprocess it and incorporate
   * the tokens of the preprocessed code with the tokens from
   * the original file. We want to mark those "expanded" tokens
   * with a special tag so that if someone do some transformation on
   * those expanded tokens they will get a warning (because we may have
   * trouble back-propagating the transformation back to the original file).
   *)
  | ExpandedTok of
      (* refers to the preprocessed file, e.g. /tmp/pp-xxxx.pphp *)
      location
      * (* kind of virtual position. This info refers to the last token
         * before a serie of expanded tokens and the int is an offset.
         * The goal is to be able to compare the position of tokens
         * between then, even for expanded tokens. See compare_pos
         * below.
         *)
        location
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
type t = {
  (* contains among other things the position of the token through
   * the token_location embedded inside the token_origin type.
   *)
  token : origin;
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

(*****************************************************************************)
(* Dumpers *)
(*****************************************************************************)

(* see -full_token_info in meta_parse_info.ml *)
let pp_full_token_info = ref false

(* for ppx_deriving *)
let pp fmt t = if !pp_full_token_info then pp fmt t else Format.fprintf fmt "()"

(*****************************************************************************)
(* Fake stuff *)
(*****************************************************************************)

let fake_location = { str = ""; pos = Pos.fake_pos }

(*****************************************************************************)
(* Other helpers *)
(*****************************************************************************)

let first_loc_of_file file = { str = ""; pos = Pos.first_pos_of_file file }

(* used only in the Scala parser for now *)
let abstract_tok = { token = Ab; transfo = NoTransfo }
