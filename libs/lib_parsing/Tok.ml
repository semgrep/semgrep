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
open Common

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Information about tokens (mostly their location).
 *
 * Note that the types below are a bit complicated because we want
 * to represent "fake" and "expanded" tokens, as well as annotate tokens
 * with transformation. This is also partly because in many of the ASTs
 * and CSTs in Semgrep, including in AST_generic.ml, we store the
 * tokens in the ASTs/CSTs at the leaves, and abuse them to compute the range
 * of constructs (they are also convenient for precise error reporting).
 * alt:
 *   - we could cleaner ASTs with range (general location) information at
 *     every nodes, in which case we would not need at least the fake
 *     tokens (we might still need the ExpandedTok type construct though).
 *
 * Technically speaking, 't' below is not really a token, because the type does
 * not store the kind of the token (e.g., PLUS | IDENT | IF | ...), just its
 * content. It's really just a lexeme, but the word lexeme is not as
 * known as token.
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* to report errors, regular position information.
 * TODO? move in a separate Loc.ml?
 *)
type location = { str : string; (* the content of the "token" *) pos : Pos.t }
[@@deriving show { with_path = false }, eq]

(* to represent fake (e.g., fake semicolons in languages such as Javascript),
 * and expanded tokens (e.g., preprocessed constructs by cpp for C/C++)
 *)
type kind =
  (* Present both in the AST and list of tokens in the pfff-based parsers *)
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

(* poor's man refactoring *)
type transformation =
  | NoTransfo
  | Remove
  | AddBefore of add
  | AddAfter of add
  | Replace of add
  | AddArgsBefore of string list

and add = AddStr of string | AddNewlineAndIdent
[@@deriving show { with_path = false }, eq]

type t = {
  (* contains among other things the position of the token through
   * the location embedded inside the kind type.
   *)
  token : kind;
  (* The transfo field as its name suggests is to allow source to source
   * transformations via token "annotations". See the documentation for Spatch.
   * TODO: remove now that we use AST-based autofix in Semgrep.
   *)
  mutable transfo : transformation; (* less: mutable comments: ...; *)
}
[@@deriving show { with_path = false }, eq]

type t_always_equal = t [@@deriving show]

(* sgrep: we do not care about position when comparing for equality 2 ASTs.
 * related: Lib_AST.abstract_position_info_any and then use OCaml generic '='.
 *)
let equal_t_always_equal _t1 _t2 = true
let hash_t_always_equal _t = 0
let hash_fold_t_always_equal acc _t = acc

(*****************************************************************************)
(* Dumpers *)
(*****************************************************************************)

(* see also -full_token_info in Meta_AST.ml *)
let pp_full_token_info = ref false

(* for ppx_deriving *)
let pp fmt t = if !pp_full_token_info then pp fmt t else Format.fprintf fmt "()"

(*****************************************************************************)
(* Fake tokens (safe and unsafe) *)
(*****************************************************************************)

exception NoTokenLocation of string

let fake_location = { str = ""; pos = Pos.fake_pos }

(*****************************************************************************)
(* Builders *)
(*****************************************************************************)

let tok_of_loc loc = { token = OriginTok loc; transfo = NoTransfo }

let tok_of_str_and_bytepos str pos =
  let loc =
    {
      str;
      pos =
        {
          charpos = pos;
          (* info filled in a post-lexing phase, see complete_location *)
          line = -1;
          column = -1;
          file = "NO FILE INFO YET";
        };
    }
  in
  tok_of_loc loc

let tok_of_lexbuf lexbuf =
  tok_of_str_and_bytepos (Lexing.lexeme lexbuf) (Lexing.lexeme_start lexbuf)

(*****************************************************************************)
(* Accessors *)
(*****************************************************************************)

let loc_of_tok (ii : t) : (location, string) Result.t =
  match ii.token with
  | OriginTok pinfo -> Ok pinfo
  (* TODO ? dangerous ? *)
  | ExpandedTok (pinfo_pp, _pinfo_orig, _offset) -> Ok pinfo_pp
  | FakeTokStr (_, Some (pi, _)) -> Ok pi
  | FakeTokStr (_, None) -> Error "FakeTokStr"
  | Ab -> Error "Ab"

let unsafe_loc_of_tok ii =
  match loc_of_tok ii with
  | Ok pinfo -> pinfo
  | Error msg -> raise (NoTokenLocation msg)

let line_of_tok ii = (unsafe_loc_of_tok ii).pos.line
let col_of_tok ii = (unsafe_loc_of_tok ii).pos.column

(* todo: return a Real | Virt position ? *)
let bytepos_of_tok ii = (unsafe_loc_of_tok ii).pos.charpos
let file_of_tok ii = (unsafe_loc_of_tok ii).pos.file

let content_of_tok ii =
  match ii.token with
  | OriginTok x -> x.str
  | FakeTokStr (s, _) -> s
  | ExpandedTok _
  | Ab ->
      raise (NoTokenLocation "content_of_tok: Expanded or Ab")

(* Token locations are supposed to denote the beginning of a token.
   Suppose we are interested in instead having line, column, and charpos of
   the end of a token instead.
   This is something we can do at relatively low cost by going through and
   inspecting the contents of the token, plus the start information.
*)
let get_token_end_info loc =
  let line, col =
    Stdcompat.String.fold_left
      (fun (line, col) c ->
        match c with
        | '\n' -> (line + 1, 0)
        | _ -> (line, col + 1))
      (loc.pos.line, loc.pos.column)
      loc.str
  in
  (line, col, loc.pos.charpos + String.length loc.str)

(*****************************************************************************)
(* Adjusting location *)
(*****************************************************************************)

(* TODO? move to Pos.ml and use Pos.t instead *)
let adjust_loc_wrt_base base_loc loc =
  (* Note that charpos and columns are 0-based, whereas lines are 1-based. *)
  {
    loc with
    pos =
      {
        charpos = base_loc.pos.charpos + loc.pos.charpos;
        line = base_loc.pos.line + loc.pos.line - 1;
        column =
          (if loc.pos.line =|= 1 then base_loc.pos.column + loc.pos.column
          else loc.pos.column);
        file = base_loc.pos.file;
      };
  }

let fix_location fix ii =
  {
    ii with
    token =
      (match ii.token with
      | OriginTok pi -> OriginTok (fix pi)
      | ExpandedTok (pi, vpi, off) -> ExpandedTok (fix pi, vpi, off)
      | FakeTokStr (s, vpi_opt) -> FakeTokStr (s, vpi_opt)
      | Ab -> Ab);
  }

let adjust_tok_wrt_base base_loc ii =
  fix_location (adjust_loc_wrt_base base_loc) ii

(*****************************************************************************)
(* Adjust line x col *)
(*****************************************************************************)

let complete_location filename table (x : location) =
  { x with pos = Pos.complete_position filename table x.pos }

(*
I used to have:
 type program2 = toplevel2 list
  (* the token list contains also the comment-tokens *)
  and toplevel2 = Ast_php.toplevel * Parser_php.token list
type program_with_comments = program2

and a function below called distribute_info_items_toplevel that
would distribute the list of tokens to each toplevel entity.
This was when I was storing parts of AST in berkeley DB and when
I wanted to get some information about an entity (a function, a class)
I wanted to get the list also of tokens associated with that entity.

Now I just have
 type program_and_tokens = Ast_php.program * Parser_php.token list
because I don't use berkeley DB. I use codegraph and an entity_finder
we just focus on use/def and does not store huge asts on disk.


let rec distribute_info_items_toplevel2 xs toks filename =
  match xs with
  | [] -> raise Impossible
  | [Ast_php.FinalDef e] ->
      (* assert (null toks) ??? no cos can have whitespace tokens *)
      let info_item = toks in
      [Ast_php.FinalDef e, info_item]
  | ast::xs ->

      (match ast with
      | Ast_js.St (Ast_js.Nop None) ->
          distribute_info_items_toplevel2 xs toks filename
      | _ ->


      let ii = Lib_parsing_php.ii_of_any (Ast.Toplevel ast) in
      (* ugly: I use a fakeInfo for lambda f_name, so I have
       * have to filter the abstract info here
       *)
      let ii = List.filter PI.is_origintok ii in
      let (min, max) = PI.min_max_ii_by_pos ii in

      let toks_before_max, toks_after =
(* on very huge file, this function was previously segmentation fault
 * in native mode because span was not tail call
 *)
        Common.profile_code "spanning tokens" (fun () ->
        toks +> Common2.span_tail_call (fun tok ->
          match PI.compare_pos (TH.info_of_tok tok) max with
          | -1 | 0 -> true
          | 1 -> false
          | _ -> raise Impossible
        ))
      in
      let info_item = toks_before_max in
      (ast, info_item)::distribute_info_items_toplevel2 xs toks_after filename

let distribute_info_items_toplevel a b c =
  Common.profile_code "distribute_info_items" (fun () ->
    distribute_info_items_toplevel2 a b c
  )
 *)

(*****************************************************************************)
(* Other helpers *)
(*****************************************************************************)

let first_loc_of_file file = { str = ""; pos = Pos.first_pos_of_file file }

(* used only in the Scala parser for now *)
let abstract_tok = { token = Ab; transfo = NoTransfo }
