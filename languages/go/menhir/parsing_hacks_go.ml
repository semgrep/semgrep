(* Yoann Padioleau
 *
 * Copyright (C) 2019 r2c
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
open Parser_go
module Flag = Flag_parsing
module T = Parser_go
module TH = Token_helpers_go
module F = Ast_fuzzy

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* The goal for this module is to retag tokens (e.g, a LBRACE in LBODY),
 * or insert tokens (e.g., implicit semicolons) to help the grammar
 * remains simple and unambiguous.
 *
 * See lang_cpp/parsing/parsing_hacks.ml for more information about
 * this technique.
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)
type env_lbody = InIfHeader | Normal

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* alt: could have instead a better Ast_fuzzy type instead of putting
 * everything in the Tok category?
 *)
let is_identifier horigin (info : Tok.t) =
  match Hashtbl.find_opt horigin info with
  | Some (T.LNAME _) -> true
  | _ -> false

(*****************************************************************************)
(* ASI *)
(*****************************************************************************)

let fix_tokens_asi xs =
  let env = () in
  let rec aux env xs =
    match xs with
    | [] -> []
    (* ASI: automatic semicolon insertion, similar in Javascript *)
    | (( LNAME _ | LINT _ | LFLOAT _ | LIMAG _ | LRUNE _ | LSTR _ | LBREAK _
       | LCONTINUE _ | LFALL _ | LRETURN _ | LINC _ | LDEC _ | RPAREN _
       | RBRACE _ | RBRACKET _
       (* sgrep-ext: *)
       | LDDD _ ) as x)
      :: ((TCommentNewline ii | EOF ii) as y)
      :: xs -> (
        match (x, y, !Flag_parsing.sgrep_mode) with
        (* do NOT ASI *)

        (* sgrep-ext: only in sgrep-mode *)
        | LDDD _, _, false
        (* sgrep-ext: we don't want $X==$X to be transformed
         * in $X==$X; in sgrep mode
         *)
        | _, EOF _, true ->
            x :: y :: aux env xs
        (* otherwise do ASI *)
        | _ ->
            let iifake = Tok.rewrap_str "FAKE ';'" ii in
            (* implicit semicolon insertion *)
            x :: LSEMICOLON iifake :: y :: aux env xs)
    | x :: xs -> x :: aux env xs
  in
  aux env xs

(*****************************************************************************)
(* LBODY *)
(*****************************************************************************)
(* retagging:
 *  - '{' when part of a composite literal
 *  - '{' when composite literal in semgrep at toplevel
 *  - ':' when part of a keyval in semgrep at toplevel
 *
 * This is similar to what we do in parsing_hacks_js.ml to overcome
 * some shift/reduce limitations by cheating and inventing new tokens.
 *)
let fix_tokens_lbody toks =
  try
    let trees =
      Lib_ast_fuzzy.mk_trees
        { Lib_ast_fuzzy.tokf = TH.info_of_tok; kind = TH.token_kind_of_tok }
        toks
    in
    let horigin =
      toks
      |> List_.map (fun t -> (TH.info_of_tok t, t))
      |> Hashtbl_.hash_of_list
    in

    let retag_lbrace = Hashtbl.create 101 in
    let retag_lbrace_semgrep = Hashtbl.create 1 in
    let retag_lcolon_semgrep = Hashtbl.create 1 in
    let retag_lparen_semgrep = Hashtbl.create 1 in

    (match trees with
    (* TODO: check that actually a composite literal in it? *)
    | F.Braces (t1, _body, _) :: _ when !Flag_parsing.sgrep_mode ->
        Hashtbl.add retag_lbrace_semgrep t1 true
    (* no way it's a label *)
    | F.Tok (_s, info) :: F.Tok (":", t2) :: _
      when !Flag_parsing.sgrep_mode && is_identifier horigin info ->
        Hashtbl.add retag_lcolon_semgrep t2 true
    (* TODO: could check that xs looks like a parameter list
     * TODO what comes after Parens could be a symbol part of a type
     * instead of just a single type like 'int'?
     *)
    | F.Tok (_s, info) :: F.Parens (l, _xs, _r) :: F.Tok (_s2, info2) :: _
      when !Flag_parsing.sgrep_mode && is_identifier horigin info
           && is_identifier horigin info2 ->
        Hashtbl.add retag_lparen_semgrep l true
    | _ -> ());

    let rec aux env trees =
      match trees with
      | [] -> ()
      (* if func(...) bool { return ... }(...) { ... } *)
      | F.Braces (_lb1, xs1, _rb1)
        :: F.Parens (_lb2, xs2, _rb2)
        :: F.Braces (lb3, xs3, _rb3)
        :: ys
        when env =*= InIfHeader ->
          Hashtbl.add retag_lbrace lb3 true;
          aux Normal xs1;
          xs2
          |> List.iter (function
               | Either.Left xs -> aux Normal xs
               | Either.Right _ -> ());
          aux Normal xs3;
          aux Normal ys (* for a := struct {...} { ... } { ... } *)
      | F.Tok (("struct" | "interface"), _)
        :: F.Braces (_lb1, xs1, _rb1)
        :: F.Braces (_lb2, xs2, _rb2)
        :: F.Braces (lb3, xs3, _rb3)
        :: ys
        when env =*= InIfHeader ->
          Hashtbl.add retag_lbrace lb3 true;
          aux Normal xs1;
          aux Normal xs2;
          aux Normal xs3;
          aux Normal ys
          (* must be after previous case *)
          (* skipping: if ok := interface{} ... *)
      | F.Tok (("struct" | "interface"), _) :: F.Braces (_lb1, xs1, _rb1) :: ys
        when env =*= InIfHeader ->
          aux Normal xs1;
          aux env ys
      (* for a := range []int{...} { ... } *)
      | F.Braces (_lb1, xs1, _rb1) :: F.Braces (lb2, xs2, _rb2) :: ys
        when env =*= InIfHeader ->
          Hashtbl.add retag_lbrace lb2 true;
          aux Normal xs1;
          aux Normal xs2;
          aux Normal ys (* False Positive (FP): for ... {}[...] *)
      | F.Braces (_lb, xs, _rb) :: F.Bracket (_, ys, _) :: zs
        when env =*= InIfHeader ->
          aux Normal xs;
          aux Normal ys;
          aux env zs
      (* False Positive (FP): if ... {}; ... { *)
      | F.Braces (_lb, xs, _rb) :: F.Tok (";", _) :: zs when env =*= InIfHeader
        ->
          aux Normal xs;
          aux env zs
      | F.Braces (lb, xs, _rb) :: ys ->
          (* for ... { ... } *)
          if env =*= InIfHeader then Hashtbl.add retag_lbrace lb true;
          aux Normal xs;
          aux Normal ys
      | F.Tok (("if" | "for" | "switch" | "select"), _) :: xs ->
          aux InIfHeader xs
      | x :: xs ->
          (match x with
          | F.Parens (_, xs, _) ->
              xs
              |> List.iter (function
                   | Either.Left trees -> aux Normal trees
                   | Either.Right _comma -> ())
          | _ -> ());
          aux env xs
    in
    aux Normal trees;

    (* use the tagged information and transform tokens *)
    toks
    |> List_.map (function
         | T.LBRACE info when Hashtbl.mem retag_lbrace info -> T.LBODY info
         | T.LBRACE info when Hashtbl.mem retag_lbrace info -> T.LBODY info
         | T.LBRACE info when Hashtbl.mem retag_lbrace_semgrep info ->
             T.LBRACE_SEMGREP info
         | T.LCOLON info when Hashtbl.mem retag_lcolon_semgrep info ->
             T.LCOLON_SEMGREP info
         | T.LPAREN info when Hashtbl.mem retag_lparen_semgrep info ->
             T.LPAREN_SEMGREP info
         | x -> x)
  with
  | Lib_ast_fuzzy.Unclosed (msg, info) ->
      if !Flag.error_recovery then toks
      else raise (Parsing_error.Lexical_error (msg, info))

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let fix_tokens xs =
  let xs = fix_tokens_asi xs in
  fix_tokens_lbody xs
