(* Yoann Padioleau
 *
 * Copyright (C) 2010 Facebook
 * Copyright (C) 2020, 2023 r2c
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
open Common
open Tok

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* TODO: remove this file, spread its content in separate files *)

(*****************************************************************************)
(* Misc *)
(*****************************************************************************)

(* TODO: remove at some point *)
type t = Tok.t [@@deriving eq, show]

(* Synthesize a token. *)
let unsafe_fake_info str : Tok.t =
  { token = FakeTokStr (str, None); transfo = NoTransfo }

(* "safe" fake token *)
let fake_info_loc next_to_loc str : Tok.t =
  (* TODO: offset seems to have no use right now (?) *)
  { token = FakeTokStr (str, Some (next_to_loc, -1)); transfo = NoTransfo }

let fake_info next_to_tok str : Tok.t =
  match Tok.loc_of_tok next_to_tok with
  | Ok loc -> fake_info_loc loc str
  | Error _ -> unsafe_fake_info str

let is_fake tok =
  match tok.token with
  | FakeTokStr _ -> true
  | _ -> false

(* TODO: the use of unsafe_fake_xxx is usually because the token
 * does not exist in the original file. It's better then to generate
 * an empty string in the FakeTokStr so that pretty printer will
 * not generate those brackets or semicolons. Moreover
 * we use unsafe_fake_bracket not only for () but also for [], {}, and
 * now even for "", so better again to put an empty string in it.
 *)

(* used to be in AST_generic.ml *)
let unsafe_fake_bracket x = (unsafe_fake_info "(", x, unsafe_fake_info ")")

let fake_bracket_loc next_to_loc x =
  (fake_info_loc next_to_loc "(", x, fake_info_loc next_to_loc ")")

let fake_bracket next_to_tok x =
  (fake_info next_to_tok "(", x, fake_info next_to_tok ")")

let unbracket (_, x, _) = x
let sc_loc next_to_loc = fake_info_loc next_to_loc ";"

(*****************************************************************************)
(* Accessors *)
(*****************************************************************************)

(* for error reporting *)
let string_of_token_location x = Pos.string_of_pos x.pos

let string_of_info x =
  match Tok.loc_of_tok x with
  | Ok loc -> string_of_token_location loc
  | Error msg -> spf "unknown location (%s)" msg

(*****************************************************************************)
(* Misc *)
(*****************************************************************************)

let pinfo_of_info ii = ii.token

let is_origintok ii =
  match ii.token with
  | OriginTok _ -> true
  | _ -> false

(* info about the current location *)

(* used by token_helpers *)

(* not used but used to be useful in coccinelle *)
type posrv =
  | Real of Tok.location
  | Virt of
      Tok.location (* last real info before expanded tok *)
      * int (* virtual offset *)

let compare_pos ii1 ii2 =
  let get_pos = function
    | OriginTok pi -> Real pi
    (* todo? I have this for lang_php/
        | FakeTokStr (s, Some (pi_orig, offset)) ->
            Virt (pi_orig, offset)
    *)
    | FakeTokStr _ -> raise (NoTokenLocation "compare_pos: FakeTokStr")
    | Ab -> raise (NoTokenLocation "compare_pos: Ab")
    | ExpandedTok (_pi_pp, pi_orig, offset) -> Virt (pi_orig, offset)
  in
  let pos1 = get_pos (pinfo_of_info ii1) in
  let pos2 = get_pos (pinfo_of_info ii2) in
  match (pos1, pos2) with
  | Real p1, Real p2 -> compare p1.pos.charpos p2.pos.charpos
  | Virt (p1, _), Real p2 ->
      if compare p1.pos.charpos p2.pos.charpos =|= -1 then -1 else 1
  | Real p1, Virt (p2, _) ->
      if compare p1.pos.charpos p2.pos.charpos =|= 1 then 1 else -1
  | Virt (p1, o1), Virt (p2, o2) -> (
      let poi1 = p1.pos.charpos in
      let poi2 = p2.pos.charpos in
      match compare poi1 poi2 with
      | -1 -> -1
      | 0 -> compare o1 o2
      | 1 -> 1
      | _ -> raise Impossible)

(* TODO: we should filter with is_origintok() first, to avoid having
 * the caller to do it.
 *)
let min_max_ii_by_pos xs =
  match xs with
  | [] ->
      raise
        (NoTokenLocation
           "Match returned an empty list with no token location information; \
            this may be fixed by adding enclosing token information (e.g. \
            bracket or parend tokens) to the list's enclosing node type.")
  | [ x ] -> (x, x)
  | x :: xs ->
      let pos_leq p1 p2 = compare_pos p1 p2 =|= -1 in
      xs
      |> List.fold_left
           (fun (minii, maxii) e ->
             let maxii' = if pos_leq maxii e then e else maxii in
             let minii' = if pos_leq e minii then e else minii in
             (minii', maxii'))
           (x, x)
