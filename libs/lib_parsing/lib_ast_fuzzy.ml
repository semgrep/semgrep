(* Yoann Padioleau
 *
 * Copyright (C) 2013 Facebook
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
open Ast_fuzzy

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* A few helpers function to build Ast_fuzzy tree from a list of tokens.
 * It factorizes the language-independent part of those AST fuzzy builder.
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

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

type 'tok hooks = { kind : 'tok -> token_kind; tokf : 'tok -> Tok.t }

exception Unclosed of string * Tok.t (* starting point *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
let char_of_token_kind = function
  | RAngle -> '>'
  | RBracket -> ']'
  | RBrace -> '}'
  | _ -> raise Impossible

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

(*
 * less: I should also factorize with Parse_cpp.parse_fuzzy.
 * put here also generic parts of  token_views_of_xxx?
 *
 * less: check that it's consistent with the indentation?
 * less: more fault tolerance? if col == 0 and { then reset?
 *)

let mk_trees h xs =
  (* filter comment tokens *)
  let xs =
    xs
    |> Common.exclude (fun t ->
           let kind = h.kind t in
           match kind with
           | Esthet _
           | Eof ->
               true
           | _ -> false)
  in

  let rec consume x xs =
    match x with
    | tok when h.kind tok =*= LBrace ->
        let body, closing, rest = look_close RBrace x [] xs in
        (Ast_fuzzy.Braces (h.tokf x, body, h.tokf closing), rest)
    | tok when h.kind tok =*= LBracket ->
        let body, closing, rest = look_close RBracket x [] xs in
        (Ast_fuzzy.Bracket (h.tokf x, body, h.tokf closing), rest)
    | tok when h.kind tok =*= LAngle ->
        let body, closing, rest = look_close RAngle x [] xs in
        (Ast_fuzzy.Angle (h.tokf x, body, h.tokf closing), rest)
    | tok when h.kind tok =*= LPar ->
        let body, closing, rest = look_close_paren x [] xs in
        let body' = split_comma body in
        (Ast_fuzzy.Parens (h.tokf x, body', h.tokf closing), rest)
    | tok -> (Ast_fuzzy.Tok (Tok.content_of_tok (h.tokf tok), h.tokf x), xs)
    (*
    (match Ast.str_of_info (tokext tok) with
    | "..." -> Ast_fuzzy.Dots (tokext tok)
    | s when Ast_fuzzy.is_metavar s -> Ast_fuzzy.Metavar (s, tokext tok)
    | s -> Ast_fuzzy.Tok (s, tokext tok)
*)
  and aux xs =
    match xs with
    | [] -> []
    | x :: xs ->
        let x', xs' = consume x xs in
        x' :: aux xs'
  and look_close close_kind tok_start accbody xs =
    match xs with
    | [] ->
        raise
          (Unclosed
             ( spf "look_close '%c'" (char_of_token_kind close_kind),
               h.tokf tok_start ))
    | x :: xs -> (
        match x with
        | tok when h.kind tok =*= close_kind -> (List.rev accbody, x, xs)
        | _ ->
            let x', xs' = consume x xs in
            look_close close_kind tok_start (x' :: accbody) xs')
  (* todo? diff with look_close RPar ? *)
  and look_close_paren tok_start accbody xs =
    match xs with
    | [] -> raise (Unclosed ("look_close_paren", h.tokf tok_start))
    | x :: xs -> (
        match x with
        | tok when h.kind tok =*= RPar -> (List.rev accbody, x, xs)
        | _ ->
            let x', xs' = consume x xs in
            look_close_paren tok_start (x' :: accbody) xs')
  and split_comma xs =
    let rec aux acc xs =
      match xs with
      | [] -> if null acc then [] else [ Left (acc |> List.rev) ]
      | x :: xs -> (
          match x with
          | Ast_fuzzy.Tok (",", info) ->
              let before = acc |> List.rev in
              if null before then aux [] xs
              else Left before :: Right info :: aux [] xs
          | _ -> aux (x :: acc) xs)
    in
    aux [] xs
  in
  aux xs

let mk_tokens hooks toks =
  toks |> List.map (fun tok -> (hooks.kind tok, hooks.tokf tok))

(*****************************************************************************)
(* Visitor *)
(*****************************************************************************)

type visitor_out = trees -> unit

type visitor_in = {
  ktree : (tree -> unit) * visitor_out -> tree -> unit;
  ktrees : (trees -> unit) * visitor_out -> trees -> unit;
  ktok : (tok -> unit) * visitor_out -> tok -> unit;
}

let (default_visitor : visitor_in) =
  {
    ktree = (fun (k, _) x -> k x);
    ktok = (fun (k, _) x -> k x);
    ktrees = (fun (k, _) x -> k x);
  }

let (mk_visitor : visitor_in -> visitor_out) =
 fun vin ->
  let rec v_tree x =
    let k x =
      match x with
      | Braces (v1, v2, v3) ->
          let _v1 = v_tok v1 and _v2 = v_trees v2 and _v3 = v_tok v3 in
          ()
      | Parens (v1, v2, v3) ->
          let _v1 = v_tok v1
          and _v2 = OCaml.v_list (OCaml.v_either v_trees v_tok) v2
          and _v3 = v_tok v3 in
          ()
      | Angle (v1, v2, v3) ->
          let _v1 = v_tok v1 and _v2 = v_trees v2 and _v3 = v_tok v3 in
          ()
      | Bracket (v1, v2, v3) ->
          let _v1 = v_tok v1 and _v2 = v_trees v2 and _v3 = v_tok v3 in
          ()
      | Metavar v1 ->
          let _v1 = v_wrap v1 in
          ()
      | Dots v1 ->
          let _v1 = v_tok v1 in
          ()
      | Tok v1 ->
          let _v1 = v_wrap v1 in
          ()
    in
    vin.ktree (k, all_functions) x
  and v_trees a =
    let k xs =
      match xs with
      | [] -> ()
      | x :: xs ->
          v_tree x;
          v_trees xs
    in
    vin.ktrees (k, all_functions) a
  and v_wrap (_s, x) = v_tok x
  and v_tok x =
    let k _x = () in
    vin.ktok (k, all_functions) x
  and all_functions x = v_trees x in
  all_functions

(*****************************************************************************)
(* Map *)
(*****************************************************************************)

type map_visitor = { mtok : (tok -> tok) -> tok -> tok }

let (mk_mapper : map_visitor -> trees -> trees) =
 fun hook ->
  let rec map_tree = function
    | Braces (v1, v2, v3) ->
        let v1 = map_tok v1 and v2 = map_trees v2 and v3 = map_tok v3 in
        Braces (v1, v2, v3)
    | Parens (v1, v2, v3) ->
        let v1 = map_tok v1
        and v2 = List.map (OCaml.map_of_either map_trees map_tok) v2
        and v3 = map_tok v3 in
        Parens (v1, v2, v3)
    | Angle (v1, v2, v3) ->
        let v1 = map_tok v1 and v2 = map_trees v2 and v3 = map_tok v3 in
        Angle (v1, v2, v3)
    | Bracket (v1, v2, v3) ->
        let v1 = map_tok v1 and v2 = map_trees v2 and v3 = map_tok v3 in
        Bracket (v1, v2, v3)
    | Metavar v1 ->
        let v1 = map_wrap v1 in
        Metavar v1
    | Dots v1 ->
        let v1 = map_tok v1 in
        Dots v1
    | Tok v1 ->
        let v1 = map_wrap v1 in
        Tok v1
  and map_trees v = List.map map_tree v
  and map_tok v =
    let k v = v in
    hook.mtok k v
  and map_wrap (s, t) = (s, map_tok t) in
  map_trees

(*****************************************************************************)
(* Extractor *)
(*****************************************************************************)

let (toks_of_trees : trees -> Tok.t list) =
 fun trees ->
  let globals = ref [] in
  let hooks =
    { default_visitor with ktok = (fun (_k, _) i -> Common.push i globals) }
  in
  let vout = mk_visitor hooks in
  vout trees;
  List.rev !globals

(*****************************************************************************)
(* Abstract position *)
(*****************************************************************************)

let abstract_position_trees trees =
  let hooks = { mtok = (fun _k _i -> Tok.Ab) } in
  let mapper = mk_mapper hooks in
  mapper trees
