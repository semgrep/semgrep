(* Yoann Padioleau
 *
 * Copyright (C) 2002-2008 Yoann Padioleau
 * Copyright (C) 2014 Facebook
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
open Parser_cpp
open Token_views_cpp
module TH = Token_helpers_cpp
module TV = Token_views_cpp

[@@@warning "-9"]

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Argument vs Parameter *)
(*****************************************************************************)

let look_like_argument _tok_before xs =
  (* normalize for C++ *)
  let xs =
    xs
    |> List_.map (function
         | Tok ({ t = TAnd ii } as record) -> Tok { record with t = TMul ii }
         | x -> x)
  in
  (* split by comma so can easily check if have stuff like '*xx'
   * that takes the full argument
   *)
  let xxs = split_comma xs in

  let aux1 xs =
    match xs with
    | [] -> false
    (* *xx    (note: actually can also be a function pointer decl) *)
    | [ Tok { t = TMul _ }; Tok { t = TIdent _ } ] -> true
    (* *(xx) *)
    | [ Tok { t = TMul _ }; Parens _ ] -> true
    (* TODO: xx * yy  and space = 1 between the 2 :) *)
    | _ -> false
  in

  let rec aux xs =
    match xs with
    | [] -> false
    (* a function call probably *)
    | Tok { t = TIdent _ } :: Parens _ :: _xs ->
        (* todo? look_like_argument recursively in Parens || aux xs ? *)
        true
    (* if have = ... then must stop, could be default parameter of a method *)
    | Tok { t = TEq _ } :: _xs -> false
    (* could be part of a type declaration *)
    | Tok { t = TOCro _ } :: Tok { t = TCCro _ } :: _xs -> false
    | Tok { t = TOCro _ } :: Tok { t = TInt _ } :: Tok { t = TCCro _ } :: _xs ->
        false
    | Tok { t = TOCro _ } :: Tok { t = TIdent _ } :: Tok { t = TCCro _ } :: _xs
      ->
        false
    | x :: xs -> (
        match x with
        | Tok { t = TInt _ | TFloat _ | TChar _ | TString _ } -> true
        | Tok { t = Ttrue _ | Tfalse _ } -> true
        | Tok { t = Tthis _ } -> true
        | Tok { t = Tnew _ } -> true
        | Tok { t = tok } when TH.is_binary_operator_except_star tok -> true
        | Tok { t = TInc _ | TDec _ } -> true
        | Tok { t = TDot _ | TPtrOp _ | TPtrOpStar _ | TDotStar _ } -> true
        | Tok { t = TOCro _ } -> true
        | Tok { t = TWhy _ | TBang _ } -> true
        | _ -> aux xs)
  in
  (* todo? what if they contradict each other? if one say arg and
   * the other a parameter?
   *)
  xxs |> List.exists aux1 || aux xs

let look_like_typedef s =
  s =~ ".*_t$" || s = "ulong" || s = "uchar" || s = "uvlong" || s = "vlong"
  || s = "uintptr"

(* plan9, but actually some fp such as Paddr which is actually a macro *)
(*  || s =~ "[A-Z][a-z].*$" *)
(* with DECLARE_BOOST_TYPE, but have some false positives
 * when people do xx* indexPtr = const_cast<>(indexPtr);
 *)
(* s =~ ".*Ptr$" *)
(* || s = "StringPiece" *)

(* todo: pass1, look for const, etc
 * todo: pass2, look xx_t, xx&, xx*, xx**, see heuristics in typedef
 *
 * Many patterns should mimic some heuristics in parsing_hack_typedef.ml
 *)
let look_like_parameter_bis tok_before xs =
  (* normalize for C++ *)
  let xs =
    xs
    |> List_.map (function
         | Tok ({ t = TAnd ii } as record) -> Tok { record with t = TMul ii }
         | x -> x)
  in
  let xxs = split_comma xs in

  let aux1 xs =
    match xs with
    | [] -> false
    (* xx_t *)
    | [ Tok { t = TIdent (s, _) } ] when look_like_typedef s -> true
    (* xx* *)
    | [ Tok { t = TIdent _ }; Tok { t = TMul _ } ] -> true
    (* xx** *)
    | [ Tok { t = TIdent _ }; Tok { t = TMul _ }; Tok { t = TMul _ } ] -> true
    (* xx * y      could be multiplication (or xx & yy) ..
     * todo: could look if space around :) but because of the
     *  filtering of template and qualifier the no_space_between
     *  may not be completely accurate here. May need lower level access
     *  to the list of TCommentSpace and their position.
     *  hmm but can look at col?
     *
     * C-s for parameter_decl in grammar to see that catch() is
     * a InParameter.
     *)
    | [ Tok { t = TIdent _ }; Tok { t = TMul _ }; Tok { t = TIdent _ } ] -> (
        match tok_before with
        | Tok
            {
              t =
                ( Tcatch _
                (* ugly: TIdent_Constructor interaction between past heuristics *)
                | TIdent_Constructor _ | Toperator _ (* no! | TIdent _ *) );
            } ->
            true
        | _ -> false)
    | _ -> false
  in

  let rec aux xs =
    match xs with
    | [] -> false
    (* xx yy *)
    | Tok { t = TIdent _ } :: Tok { t = TIdent _ } :: _xs -> true
    | x :: xs -> (
        match x with
        | Tok { t = tok } when TH.is_basic_type tok -> true
        | Tok { t = Tconst _ | Tvolatile _ } -> true
        | Tok { t = Tstruct _ | Tunion _ | Tenum _ | Tclass _ } -> true
        | _ -> aux xs)
  in
  xxs |> List.exists aux1 || aux xs

(* TODO: lots of things can mean it's not a parameter, like
 * a ';' in xs
 *)
let look_like_parameter tok_before xs =
  match tok_before with
  | Tok { t = Tfor _ } -> false
  | _ -> look_like_parameter_bis tok_before xs

(*****************************************************************************)
(* Main heuristics *)
(*****************************************************************************)
(*
 * Most of the important contexts are introduced via some '{' '}'. To
 * disambiguate is it often enough to just look at a few tokens before the
 * '{'.
 *
 * Below we assume a view without:
 * - comments
 * - cpp directives
 *
 * todo
 *  - handle more C++ (right now I did it mostly to be able to parse plan9)
 *  - harder now that have c++, can have function inside struct so need
 *    handle all together.
 *  - change token but do not recurse in
 *    nested Braceised. maybe do via accumulator, don't use iter_token_brace?
 *  - need remove the qualifier as they make the sequence pattern matching
 *    more difficult?
 *)
let set_context_tag_multi groups =
  let rec aux xs =
    match xs with
    | [] -> ()
    (* struct Foo {, also valid for class and union *)
    | Tok { t = Tstruct _ | Tunion _ | Tclass _ }
      :: Tok { t = TIdent (s, _) }
      :: (Braces (_t1, _body, _t2) as braces)
      :: xs ->
        [ braces ]
        |> TV.iter_token_multi (fun tok ->
               tok.TV.where <- TV.InClassStruct s :: tok.TV.where);
        aux (braces :: xs)
    | Tok { t = Tstruct _ | Tunion _ }
      :: (Braces (_t1, _body, _t2) as braces)
      :: xs ->
        [ braces ]
        |> TV.iter_token_multi (fun tok ->
               tok.TV.where <- TV.InClassStruct "__anon__" :: tok.TV.where);
        aux (braces :: xs)
    (* = { } *)
    | Tok { t = TEq _; _ } :: (Braces (_t1, _body, _t2) as braces) :: xs ->
        [ braces ]
        |> TV.iter_token_multi (fun tok ->
               tok.TV.where <- InInitializer :: tok.TV.where);
        aux (braces :: xs)
    (* enum xxx { InEnum *)
    | Tok { t = Tenum _ }
      :: Tok { t = TIdent (_, _) }
      :: (Braces (_t1, _body, _t2) as braces)
      :: xs
    | Tok { t = Tenum _ } :: (Braces (_t1, _body, _t2) as braces) :: xs ->
        [ braces ]
        |> TV.iter_token_multi (fun tok ->
               tok.TV.where <- TV.InEnum :: tok.TV.where);
        aux (braces :: xs)
    (* C++: class Foo : ... { *)
    | Tok { t = Tclass _ | Tstruct _ }
      :: Tok { t = TIdent (s, _) }
      :: Tok { t = TCol ii }
      :: xs ->
        let before, braces, after =
          try
            xs
            |> Common2.split_when (function
                 | Braces _ -> true
                 | _ -> false)
          with
          | Not_found ->
              raise
                (UnclosedSymbol
                   (spf "PB with split_when at %s" (Tok.stringpos_of_tok ii)))
        in
        aux before;
        [ braces ]
        |> TV.iter_token_multi (fun tok ->
               tok.TV.where <- TV.InClassStruct s :: tok.TV.where);
        aux [ braces ];
        aux after
    (* need to look what was before to help the look_like_xxx heuristics
     *
     * The order of the 3 rules below is important. We must first try
     * look_like_argument which has less FP than look_like_parameter
     *)
    | x :: (Parens (_t1, body, _t2) as parens) :: xs
      when look_like_argument x body ->
        (*msg_context t1.t (TV.InArgument); *)
        [ parens ]
        |> TV.iter_token_multi (fun tok ->
               tok.TV.where <- TV.InArgument :: tok.TV.where);
        (* todo? recurse on body? *)
        aux [ x ];
        aux (parens :: xs)
    (* C++: special cases *)
    | (Tok { t = Toperator _ } as tok1)
      :: tok2
      :: (Parens (_t1, body, _t2) as parens)
      :: xs
      when look_like_parameter tok1 body ->
        (* msg_context t1.t (TV.InParameter); *)
        [ parens ]
        |> TV.iter_token_multi (fun tok ->
               tok.TV.where <- TV.InParameter :: tok.TV.where);
        (* recurse on body? hmm if InParameter should not have nested
         * stuff except when pass function pointer
         *)
        aux [ tok1; tok2 ];
        aux (parens :: xs)
    | x :: (Parens (_t1, body, _t2) as parens) :: xs
      when look_like_parameter x body ->
        (* msg_context t1.t (TV.InParameter); *)
        [ parens ]
        |> TV.iter_token_multi (fun tok ->
               tok.TV.where <- TV.InParameter :: tok.TV.where);
        (* recurse on body? hmm if InParameter should not have nested
         * stuff except when pass function pointer
         *)
        aux [ x ];
        aux (parens :: xs)
    (* void xx() *)
    | Tok { t = typ }
      :: Tok { t = TIdent _ }
      :: (Parens (_t1, _body, _t2) as parens)
      :: xs
      when TH.is_basic_type typ ->
        (* msg_context t1.t (TV.InParameter); *)
        [ parens ]
        |> TV.iter_token_multi (fun tok ->
               tok.TV.where <- TV.InParameter :: tok.TV.where);
        aux (parens :: xs)
    | x :: xs ->
        (match x with
        | Tok _t -> ()
        | Parens (_t1, xs, _t2)
        | Braces (_t1, xs, _t2)
        | Angle (_t1, xs, _t2) ->
            aux xs);
        aux xs
  in
  (* sane initialization *)
  groups |> TV.iter_token_multi (fun tok -> tok.TV.where <- [ TV.InTopLevel ]);
  aux groups

(*****************************************************************************)
(* Main heuristics C++ *)
(*****************************************************************************)
(*
 * assumes a view without:
 * - template arguments, qualifiers,
 * - comments and cpp directives
 * - TODO public/protected/... ?
 *)
let set_context_tag_cplus groups = set_context_tag_multi groups
