(* Yoann Padioleau
 *
 * Copyright (C) 2012 Facebook
 * Copyright (C) 2020 Semgrep Inc.
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
open Parser_java
module Flag = Flag_parsing
module TH = Token_helpers_java
module F = Ast_fuzzy
module T = Parser_java
module Log = Log_parser_java.Log

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* This module transforms certain tokens like '<', normally a LT
 * into a LT_GENERIC, which helps solving conflicts in the original
 * Java grammar.
 *
 * This is similar to what we do for C/C++.
 * See cpp/.../parsing_hacks.ml for more information.
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
(* alt: could have instead a better Ast_fuzzy type instead of putting
 * everything in the Tok category?
 * coupling: copy-paste of Parsing_hacks_go.is_identifier
 *)
let is_identifier horigin (info : Tok.t) =
  match Hashtbl.find_opt horigin info with
  | Some (IDENTIFIER _) -> true
  | _ -> false

(*****************************************************************************)
(* Generic inference *)
(*****************************************************************************)

let fix_tokens_generics xs =
  let rec aux env xs =
    let depth_angle = env in
    if depth_angle < 0 then (
      Log.warn (fun m -> m "depth_angle < 0, %d" depth_angle);
      Log.debug (fun m ->
          m "%s"
            (match xs with
            | x :: _ -> Dumper.dump x
            | [] -> "<can't get info from empty list>"));
      (* alt: failwith "depth < 0" *)
      aux 0 xs)
    else
      match xs with
      | [] -> []
      (* dont transform the < of type parameters in LT_GENERIC. Transforms
       * only for type arguments (but increment depth_angle because
       * we may still need to transform some >> into > >).
       *)
      | CLASS ii :: TCommentSpace ii2 :: IDENTIFIER (s3, ii3) :: LT ii4 :: xs ->
          CLASS ii :: TCommentSpace ii2
          :: IDENTIFIER (s3, ii3)
          :: LT ii4
          :: aux (depth_angle + 1) xs
      | CLASS ii
        :: TCommentSpace ii2
        :: IDENTIFIER (s3, ii3)
        :: TCommentSpace iisp
        :: LT ii4
        :: xs ->
          CLASS ii :: TCommentSpace ii2
          :: IDENTIFIER (s3, ii3)
          :: TCommentSpace iisp :: LT ii4
          :: aux (depth_angle + 1) xs
      | INTERFACE ii
        :: TCommentSpace ii2
        :: IDENTIFIER (s3, ii3)
        :: LT ii4
        :: xs ->
          INTERFACE ii :: TCommentSpace ii2
          :: IDENTIFIER (s3, ii3)
          :: LT ii4
          :: aux (depth_angle + 1) xs
      | INTERFACE ii
        :: TCommentSpace ii2
        :: IDENTIFIER (s3, ii3)
        :: TCommentSpace iisp
        :: LT ii4
        :: xs ->
          INTERFACE ii :: TCommentSpace ii2
          :: IDENTIFIER (s3, ii3)
          :: TCommentSpace iisp :: LT ii4
          :: aux (depth_angle + 1) xs
      (* UGLY HARDCODE, proper way is to have a phase where filter all
       * TCommentSpace and Newline
       *)
      | CLASS ii
        :: TCommentNewline iinewline
        :: TCommentSpace ii2
        :: IDENTIFIER (s3, ii3)
        :: LT ii4
        :: xs ->
          CLASS ii :: TCommentNewline iinewline :: TCommentSpace ii2
          :: IDENTIFIER (s3, ii3)
          :: LT ii4
          :: aux (depth_angle + 1) xs
      (* too many FPs
          | IDENTIFIER (s, ii1)::TCommentSpace iispace::LT ii2::xs
             when s =~ "^[A-Z]" ->
              IDENTIFIER (s, ii1)::TCommentSpace iispace::LT_GENERIC ii2::
                aux (depth_angle + 1) xs
      *)
      (* less: allow also a small space, but usually we should fix
       * this code. But pb, see previous comment.
       *)
      | IDENTIFIER (s, ii1) :: LT ii2 :: xs when s =~ "^[A-Z]" ->
          Log.debug (fun m -> m "retagging < at %s" (Tok.stringpos_of_tok ii2));
          IDENTIFIER (s, ii1) :: LT_GENERIC ii2 :: aux (depth_angle + 1) xs
      | IDENTIFIER (s, ii1)
        :: TCommentSpace iispace
        :: LT ii2
        :: IDENTIFIER (s3, ii3)
        :: xs
        when s =~ "^[A-Z]" && s3 =~ "^[A-Z]" ->
          Log.debug (fun m -> m "retagging < at %s" (Tok.stringpos_of_tok ii2));
          IDENTIFIER (s, ii1)
          :: TCommentSpace iispace :: LT_GENERIC ii2
          :: aux (depth_angle + 1) (IDENTIFIER (s3, ii3) :: xs)
      | IDENTIFIER (s, ii1) :: TCommentSpace iispace :: LT ii2 :: COND ii3 :: xs
        when s =~ "^[A-Z]" ->
          Log.debug (fun m -> m "retagging < at %s" (Tok.stringpos_of_tok ii2));
          IDENTIFIER (s, ii1)
          :: TCommentSpace iispace :: LT_GENERIC ii2
          :: aux (depth_angle + 1) (COND ii3 :: xs)
      (* xxx.<type>of(...), actually don't have to transform in a LT_GENERIC
       * but it's a type context so we need to augment depth_angle
       * so at least the >> get transformed into > >.
       *)
      | DOT ii1 :: LT ii2 :: xs ->
          Log.debug (fun m -> m "retagging < at %s" (Tok.stringpos_of_tok ii2));
          DOT ii1 :: LT_GENERIC ii2 :: aux (depth_angle + 1) xs
      (* <T extends ...> bar().
       * could also check for public|static|... just before the <
       * which is also the sign of generic method.
       *)
      | LT ii1
        :: IDENTIFIER (s, ii2)
        :: TCommentSpace iispace
        :: EXTENDS ii3
        :: xs ->
          LT ii1
          :: IDENTIFIER (s, ii2)
          :: TCommentSpace iispace :: EXTENDS ii3
          :: aux (depth_angle + 1) xs
      | GT ii :: xs when depth_angle > 0 -> GT ii :: aux (depth_angle - 1) xs
      (* transform >> into two > > *)
      | SRS ii :: xs when depth_angle > 0 ->
          (* todo: split ii *)
          GT ii :: GT ii :: aux (depth_angle - 2) xs
      (* transform >>> into three > > > *)
      | URS ii :: xs when depth_angle > 0 ->
          (* todo: split ii *)
          GT ii :: GT ii :: GT ii :: aux (depth_angle - 3) xs
      | x :: xs -> x :: aux env xs
  in
  aux 0 xs

(*****************************************************************************)
(* Lambdas *)
(*****************************************************************************)
let fix_tokens_fuzzy toks =
  try
    let trees =
      Lib_ast_fuzzy.mk_trees
        { Lib_ast_fuzzy.tokf = TH.info_of_tok; kind = TH.token_kind_of_tok }
        toks
    in
    let retag_lparen = Hashtbl.create 101 in
    let retag_default = Hashtbl.create 101 in
    let retag_lparen_constructor = Hashtbl.create 101 in

    let horigin =
      toks
      |> List_.map (fun t -> (TH.info_of_tok t, t))
      |> Hashtbl_.hash_of_list
    in

    (match trees with
    (* MyConstructor(...) { ... } *)
    | Tok (_s, info) :: F.Parens (l, _xs, _r) :: F.Braces (_, _, _) :: _
      when !Flag_parsing.sgrep_mode && is_identifier horigin info ->
        Hashtbl.add retag_lparen_constructor l true
    | _ -> ());

    let rec aux env trees =
      match trees with
      | [] -> ()
      (* (...) -> *)
      | F.Parens (lp, xs, _rp) :: F.Tok ("->", _) :: ys ->
          Hashtbl.add retag_lparen lp true;
          iter_parens () xs;
          aux () ys
      | F.Tok ("default", ii) :: F.Tok (":", _) :: ys ->
          Hashtbl.add retag_default ii true;
          aux () ys
      | x :: xs ->
          (match x with
          | F.Parens (_, xs, _) -> iter_parens env xs
          | F.Braces (_, xs, _) -> aux env xs
          | F.Angle _
          | F.Bracket _
          | F.Metavar _
          | F.Dots _
          | F.Tok _ ->
              ());
          aux env xs
    and iter_parens env xs =
      xs
      |> List.iter (function
           | Either.Left trees -> aux env trees
           | Either.Right _comma -> ())
    in
    aux () trees;

    (* use the tagged information and transform tokens *)
    toks
    |> List_.map (function
         | T.LP info when Hashtbl.mem retag_lparen info ->
             Log.debug (fun m ->
                 m "retagging ( for lambda at %s" (Tok.stringpos_of_tok info));
             T.LP_LAMBDA info
         | T.LP info when Hashtbl.mem retag_lparen_constructor info ->
             Log.debug (fun m ->
                 m "retagging ( for constructor at %s"
                   (Tok.stringpos_of_tok info));
             T.LP_PARAM info
         | T.DEFAULT info when Hashtbl.mem retag_default info ->
             Log.debug (fun m ->
                 m "retagging default at %s" (Tok.stringpos_of_tok info));
             T.DEFAULT_COLON info
         | x -> x)
  with
  | Lib_ast_fuzzy.Unclosed (msg, info) ->
      if !Flag.error_recovery then toks
      else raise (Parsing_error.Lexical_error (msg, info))

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let fix_tokens xs =
  let xs = fix_tokens_generics xs in
  let xs = fix_tokens_fuzzy xs in
  xs
