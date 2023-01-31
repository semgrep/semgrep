(* Yoann Padioleau
 *
 * Copyright (C) 2010 Facebook
 * Copyright (C) 2020 r2c
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

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Information about tokens (mostly their location).
 *
 * The main types are:
 * ('token_location' < 'token_origin' < 'token_mutable') * token_kind
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type token_location = {
  str : string;
  charpos : int;
  line : int;
  column : int;
  file : string;
}
[@@deriving show { with_path = false }, eq]

let fake_token_location =
  {
    charpos = -1;
    str = "";
    line = -1;
    column = -1;
    file = "FAKE TOKEN LOCATION";
  }

let first_loc_of_file file =
  { charpos = 0; str = ""; line = 1; column = 0; file }

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
(* with tarzan *)

type token_mutable = {
  (* contains among other things the position of the token through
   * the token_location embedded inside the token_origin type.
   *)
  token : token_origin;
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
(* with tarzan *)

exception NoTokenLocation of string

let mk_info_of_loc loc = { token = OriginTok loc; transfo = NoTransfo }

let token_location_of_info ii =
  match ii.token with
  | OriginTok pinfo -> Ok pinfo
  (* TODO ? dangerous ? *)
  | ExpandedTok (pinfo_pp, _pinfo_orig, _offset) -> Ok pinfo_pp
  | FakeTokStr (_, Some (pi, _)) -> Ok pi
  | FakeTokStr (_, None) -> Error "FakeTokStr"
  | Ab -> Error "Ab"

let unsafe_token_location_of_info ii =
  match token_location_of_info ii with
  | Ok pinfo -> pinfo
  | Error msg -> raise (NoTokenLocation msg)

(* Synthesize a token. *)
let unsafe_fake_info str : token_mutable =
  { token = FakeTokStr (str, None); transfo = NoTransfo }

let fake_info_loc next_to_loc str : token_mutable =
  (* TODO: offset seems to have no use right now (?) *)
  { token = FakeTokStr (str, Some (next_to_loc, -1)); transfo = NoTransfo }

let fake_info next_to_tok str : token_mutable =
  match token_location_of_info next_to_tok with
  | Ok loc -> fake_info_loc loc str
  | Error _ -> unsafe_fake_info str

let abstract_info = { token = Ab; transfo = NoTransfo }

let is_fake tok =
  match tok.token with
  | FakeTokStr _ -> true
  | _ -> false

(* used to be in AST_generic.ml *)
let unsafe_fake_bracket x = (unsafe_fake_info "(", x, unsafe_fake_info ")")

let fake_bracket_loc next_to_loc x =
  (fake_info_loc next_to_loc "(", x, fake_info_loc next_to_loc ")")

let fake_bracket next_to_tok x =
  (fake_info next_to_tok "(", x, fake_info next_to_tok ")")

let unbracket (_, x, _) = x
let unsafe_sc = unsafe_fake_info ";"
let sc_loc next_to_loc = fake_info_loc next_to_loc ";"
let sc next_to_tok = fake_info next_to_tok ";"

type token_kind =
  (* for the fuzzy parser and sgrep/spatch fuzzy AST *)
  | LPar
  | RPar
  | LBrace
  | RBrace
  | LBracket
  | RBracket
  | LAngle
  | RAngle
  (* for the unparser helpers in spatch, and to filter
   * irrelevant tokens in the fuzzy parser
   *)
  | Esthet of esthet
  (* mostly for the lexer helpers, and for fuzzy parser *)
  (* less: want to factorize all those TH.is_eof to use that?
   * but extra cost? same for TH.is_comment?
   * todo: could maybe get rid of that now that we don't really use
   * berkeley DB and prefer Prolog, and so we don't need a sentinel
   * ast elements to associate the comments with it
   *)
  | Eof
  | Other

and esthet = Comment | Newline | Space

(* shortcut *)
type t = token_mutable [@@deriving eq]

(* see -full_token_info in meta_parse_info.ml *)
let pp_full_token_info = ref false

(* for ppx_deriving *)
let pp fmt t =
  if !pp_full_token_info then pp_token_mutable fmt t
  else Format.fprintf fmt "()"

(*****************************************************************************)
(* Accessors *)
(*****************************************************************************)

(* for error reporting *)
let string_of_token_location x = spf "%s:%d:%d" x.file x.line x.column

let string_of_info x =
  match token_location_of_info x with
  | Ok loc -> string_of_token_location loc
  | Error msg -> spf "unknown location (%s)" msg

let str_of_info ii =
  match ii.token with
  | OriginTok x -> x.str
  | FakeTokStr (s, _) -> s
  | ExpandedTok _
  | Ab ->
      raise (NoTokenLocation "str_of_info: Expanded or Ab")

let _str_of_info ii = (unsafe_token_location_of_info ii).str
let file_of_info ii = (unsafe_token_location_of_info ii).file
let line_of_info ii = (unsafe_token_location_of_info ii).line
let col_of_info ii = (unsafe_token_location_of_info ii).column

(* todo: return a Real | Virt position ? *)
let pos_of_info ii = (unsafe_token_location_of_info ii).charpos

(*****************************************************************************)
(* Lexer helpers *)
(*****************************************************************************)
(* now in Parsing_helpers.ml *)

let tokinfo_str_pos str pos =
  let loc =
    {
      charpos = pos;
      str;
      (* info filled in a post-lexing phase, see complete_token_location_large*)
      line = -1;
      column = -1;
      file = "NO FILE INFO YET";
    }
  in
  mk_info_of_loc loc

let tokinfo lexbuf =
  tokinfo_str_pos (Lexing.lexeme lexbuf) (Lexing.lexeme_start lexbuf)

let rewrap_str s ii =
  {
    ii with
    token =
      (match ii.token with
      | OriginTok pi -> OriginTok { pi with str = s }
      | FakeTokStr (s, info) -> FakeTokStr (s, info)
      | Ab -> Ab
      | ExpandedTok _ ->
          (* ExpandedTok ({ pi with Common.str = s;},vpi) *)
          failwith "rewrap_str: ExpandedTok not allowed here");
  }

(* less: should use Buffer and not ^ so we should not need that *)
let tok_add_s s ii = rewrap_str (str_of_info ii ^ s) ii

let str_of_info_fake_ok ii =
  match ii.token with
  | OriginTok pinfo -> pinfo.str
  | ExpandedTok (pinfo_pp, _pinfo_orig, _offset) -> pinfo_pp.str
  | FakeTokStr (_, Some (pi, _)) -> pi.str
  | FakeTokStr (s, None) -> s
  | Ab -> raise (NoTokenLocation "Ab")

let combine_infos x xs =
  let str = xs |> List.map str_of_info_fake_ok |> String.concat "" in
  tok_add_s str x

let split_info_at_pos pos ii =
  let loc = unsafe_token_location_of_info ii in
  let str = loc.str in
  let loc1_str = String.sub str 0 pos in
  let loc2_str = String.sub str pos (String.length str - pos) in
  let loc1 = { loc with str = loc1_str } in
  let loc2 =
    {
      loc with
      str = loc2_str;
      charpos = loc.charpos + pos;
      column = loc.column + pos;
    }
  in
  (mk_info_of_loc loc1, mk_info_of_loc loc2)

(*****************************************************************************)
(* Errors *)
(*****************************************************************************)
(* this can be used in the different lexer/parsers in pfff *)
(* coupling: see related error in Error_code and its exn_to_error *)
exception Lexical_error of string * t
exception Parsing_error of t
exception Ast_builder_error of string * t
exception Other_error of string * t

let lexical_error s lexbuf =
  let info = tokinfo lexbuf in
  if !Flag_parsing.exn_when_lexical_error then raise (Lexical_error (s, info))
  else if !Flag_parsing.verbose_lexing then pr2_once ("LEXER: " ^ s)
  else ()

(*****************************************************************************)
(* Misc *)
(*****************************************************************************)

let pinfo_of_info ii = ii.token

let is_origintok ii =
  match ii.token with
  | OriginTok _ -> true
  | _ -> false

let has_origin_loc ii =
  match ii.token with
  | OriginTok _
  | FakeTokStr (_, Some _) ->
      true
  | _ -> false

(* info about the current location *)

(* original info *)
let get_original_token_location = function
  | OriginTok pi -> pi
  | ExpandedTok (pi, _, _) -> pi
  | FakeTokStr (_, _) -> raise (NoTokenLocation "FakeTokStr")
  | Ab -> raise (NoTokenLocation "Ab")

(* used by token_helpers *)

(* not used but used to be useful in coccinelle *)
type posrv =
  | Real of token_location
  | Virt of
      token_location (* last real info before expanded tok *)
      * int (* virtual offset *)

let compare_pos ii1 ii2 =
  let get_pos = function
    | OriginTok pi
    | FakeTokStr (_, Some (pi, _)) ->
        Real pi
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
  | Real p1, Real p2 -> compare p1.charpos p2.charpos
  | Virt (p1, _), Real p2 ->
      if compare p1.charpos p2.charpos =|= -1 then -1 else 1
  | Real p1, Virt (p2, _) ->
      if compare p1.charpos p2.charpos =|= 1 then 1 else -1
  | Virt (p1, o1), Virt (p2, o2) -> (
      let poi1 = p1.charpos in
      let poi2 = p2.charpos in
      match compare poi1 poi2 with
      | -1 -> -1
      | 0 -> compare o1 o2
      | 1 -> 1
      | _ -> raise Impossible)

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

(****************************************************************************)
(* Exception printers for Printexc.to_string *)
(****************************************************************************)

let shorten_string s =
  if String.length s > 200 then String.sub s 0 200 ^ " ... (truncated)" else s

(*
   For error messages.
   - should be useful to a human reader
   - should not raise an exception
*)
let show_token_value (x : token_origin) : string =
  match x with
  | OriginTok loc -> spf "%S" (shorten_string loc.str)
  | FakeTokStr (fake, _opt_loc) -> spf "fake %S" (shorten_string fake)
  | ExpandedTok (first_loc, _, _) ->
      (* not sure about this *)
      spf "%S" (shorten_string first_loc.str)
  | Ab -> "abstract token"

let show_token_value_and_location (x : t) =
  let location = string_of_info x in
  let value = show_token_value x.token in
  spf "%s %s" location value

let string_of_exn e =
  let p = show_token_value_and_location in
  match e with
  | NoTokenLocation msg -> Some (spf "Parse_info.NoTokenLocation (%s)" msg)
  | Lexical_error (msg, tok) ->
      Some (spf "Parse_info.Lexical_error (%s, %s)" msg (p tok))
  | Parsing_error tok -> Some (spf "Parse_info.Parsing_error (%s)" (p tok))
  | Ast_builder_error (msg, tok) ->
      Some (spf "Parse_info.Ast_builder_error (%s, %s)" msg (p tok))
  | Other_error (msg, tok) ->
      Some (spf "Parse_info.Other_error (%s, %s)" msg (p tok))
  | _ -> None

let register_exception_printer () = Printexc.register_printer string_of_exn
