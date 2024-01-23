(* Yoann Padioleau
 *
 * Copyright (C) 2002-2008 Yoann Padioleau
 * Copyright (C) 2011 Facebook
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
module TH = Token_helpers_cpp
module TV = Token_views_cpp
open Parser_cpp
open Token_views_cpp
open Parsing_hacks_lib

[@@@warning "-9"]

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* This file gathers parsing heuristics related to C++.
 *
 * See also Token_views_cpp.set_context_tag and
 * Parsing_hacks_typedef.filter_for_typedef that have also some
 * heuristics specific to C++.
 *
 * TODO: * TIdent_TemplatenameInQualifier
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let no_space_between i1 i2 =
  Tok.line_of_tok i1 =|= Tok.line_of_tok i2
  && Tok.col_of_tok i1 + String.length (Tok.content_of_tok i1)
     =|= Tok.col_of_tok i2

(*****************************************************************************)
(* Template inference *)
(*****************************************************************************)

let templateLOOKAHEAD = 30

(* note: no need to check for TCPar to stop for instance the search,
 * this is will be done automatically because we would be inside a
 * Parenthised expression.
 *)
let rec have_a_tsup_quite_close xs =
  match xs with
  | [] -> false
  | x :: xs -> (
      match x with
      | { t = TSup _ } -> true
      (* false positive *)
      | { t = tok } when TH.is_static_cast_like tok -> false
      (* ugly: *)
      | { t = TOBrace _ | TPtVirg _ | TCol _ | TAssign _ } -> false
      | { t = TInf _ } ->
          (* probably nested template, still try
           * TODO: bug when have i < DEG<...>::foo(...)
           *  we should recurse!
           *)
          have_a_tsup_quite_close xs
      (* bugfix: but want allow some binary operator :) like '*' *)
      | { t = tok } when TH.is_binary_operator_except_star tok -> false
      | _ -> have_a_tsup_quite_close xs)

(* precondition: there is a tsup *)
let rec find_tsup_quite_close tok_open xs =
  let rec aux acc xs =
    match xs with
    | [] ->
        raise
          (UnclosedSymbol
             (spf "PB: find_tsup_quite_close, no > for < at line %d"
                (TH.line_of_tok tok_open.t)))
    | x :: xs -> (
        match x with
        | { t = TSup ii } -> (List.rev acc, (x, ii), xs)
        | { t = TInf _ } ->
            (* recurse *)
            let before, (tsuptok, _), after = find_tsup_quite_close x xs in
            (* we don't care about this one, it will be eventually be
             * transformed by the caller *)
            aux ((tsuptok :: List.rev before) @ (x :: acc)) after
        | x -> aux (x :: acc) xs)
  in
  aux [] xs

(* note: some macros in standard.h may expand to static_cast, so perhaps
 * better to do template detection after macro expansion ?
 *
 * C-s for TInf_Template in the grammar and you will see all cases
 * should be covered by the patterns below.
 *)
let find_template_inf_sup xs =
  let rec aux xs =
    match xs with
    | [] -> ()
    (* template<...> *)
    | { t = Ttemplate _ } :: ({ t = TInf i2 } as tok2) :: xs ->
        change_tok tok2 (TInf_Template i2);
        let before_sup, (toksup, toksupi), rest =
          find_tsup_quite_close tok2 xs
        in
        change_tok toksup (TSup_Template toksupi);

        (* recurse *)
        aux before_sup;
        aux rest
    (* static_cast<...> *)
    | { t = tok1 } :: ({ t = TInf i2 } as tok2) :: xs
      when TH.is_static_cast_like tok1 ->
        change_tok tok2 (TInf_Template i2);
        let before_sup, (toksup, toksupi), rest =
          find_tsup_quite_close tok2 xs
        in
        change_tok toksup (TSup_Template toksupi);

        (* recurse *)
        aux before_sup;
        aux rest
    (*
     * TODO: have_a_tsup_quite_close does not handle a relational < followed
     *  by a regular template.
     *)
    | { t = TIdent (_, i1) } :: ({ t = TInf i2 } as tok2) :: xs
      when no_space_between i1 i2
           && (* safe guard, and good style anyway *)
           have_a_tsup_quite_close (List_.take_safe templateLOOKAHEAD xs) ->
        change_tok tok2 (TInf_Template i2);
        let before_sup, (toksup, toksupi), rest =
          find_tsup_quite_close tok2 xs
        in
        change_tok toksup (TSup_Template toksupi);

        (* old: was changing to TIdent_Templatename but now first need
         * to do the typedef inference and then can transform the
         * TIdent_Typedef into a TIdent_Templatename
         *)

        (* recurse *)
        aux before_sup;
        aux rest
    (* special cases which allow extra space between ident and <
     * but I think it would be better for people to fix their code
     * | {t=TIdent (s,i1)}::({t=TInf i2} as tok2)
     *  ::tok3::({t=TSup i4} as tok4)::xs ->
     *  ...
     *
     *)
    (* recurse *)
    | _ :: xs -> aux xs
  in

  aux xs

(*****************************************************************************)
(* Heuristics *)
(*****************************************************************************)

let reclassify_tokens_before_idents_or_typedefs xs =
  let groups = List.rev xs in

  let rec aux xs =
    match xs with
    | [] -> ()
    (* xx::yy     where yy is ident (funcall, variable, etc)
     * need to do that recursively! if have a::b::c
     *)
    | Tok { t = TIdent _ | TIdent_ClassnameInQualifier _ }
      :: Tok { t = TColCol _ }
      :: Tok ({ t = TIdent (s2, i2) } as tok2)
      :: xs ->
        change_tok tok2 (TIdent_ClassnameInQualifier (s2, i2));
        aux (Tok tok2 :: xs)
    (* xx::t      wher et is a type
     * TODO need to do that recursively! if have a::b::c
     *)
    | Tok { t = TIdent_Typedef _ }
      :: Tok ({ t = TColCol icolcol } as tcolcol)
      :: Tok ({ t = TIdent (s2, i2) } as tok2)
      :: xs ->
        change_tok tok2 (TIdent_ClassnameInQualifier_BeforeTypedef (s2, i2));
        change_tok tcolcol (TColCol_BeforeTypedef icolcol);
        aux xs
    (* xx::t<...> where t is a templatename *)
    | Tok { t = TIdent_Templatename _ }
      :: Tok ({ t = TColCol icolcol } as tcolcol)
      :: Tok ({ t = TIdent (s2, i2) } as tok2)
      :: xs ->
        change_tok tok2 (TIdent_ClassnameInQualifier_BeforeTypedef (s2, i2));
        change_tok tcolcol (TColCol_BeforeTypedef icolcol);
        aux xs
    (* t<...>    where t is a typedef *)
    | Angle (_, xs_angle, _)
      :: Tok ({ t = TIdent_Typedef (s1, i1) } as tok1)
      :: xs ->
        aux xs_angle;
        change_tok tok1 (TIdent_Templatename (s1, i1));
        (* recurse with tok1 too! *)
        aux (Tok tok1 :: xs)
    (* TODO
     * TIdent_TemplatenameInQualifier ?
     *)
    | x :: xs ->
        (match x with
        | Tok _ -> ()
        | Braces (_, xs, _)
        | Parens (_, xs, _)
        | Angle (_, xs, _) ->
            aux (List.rev xs));
        aux xs
  in
  aux groups;
  ()

(* quite similar to filter_for_typedef
 * TODO: at some point need have to remove this and instead
 *  have a correct filter_for_typedef that also returns
 *  nested types in template arguments (and some
 *  typedef heuristics that work on template_arguments too)
 *
 * TODO: once you don't use it, remove certain grammar rules (C-s TODO)
 *)
let find_template_commentize groups =
  (* remove template *)
  let rec aux xs =
    xs
    |> List.iter (function
         | TV.Braces (_, xs, _) -> aux xs
         | TV.Parens (_, xs, _) -> aux xs
         | TV.Angle (_, _xs, _) as angle ->
             (* let's commentize everything *)
             [ angle ]
             |> TV.iter_token_multi (fun tok ->
                    change_tok tok
                      (TComment_Cpp
                         (Token_cpp.CplusplusTemplate, TH.info_of_tok tok.t)))
         | TV.Tok tok -> (
             (* todo? should also pass the static_cast<...> which normally
              * expect some TInf_Template after. Right mow I manage
              * that by having some extra rules in the grammar
              *)
             match tok.t with
             | Ttemplate _ ->
                 change_tok tok
                   (TComment_Cpp
                      (Token_cpp.CplusplusTemplate, TH.info_of_tok tok.t))
             | _ -> ()))
  in
  aux groups

(* assumes a view without:
 * - template arguments
 *
 * TODO: once you don't use it, remove certain grammar rules (C-s TODO)
 *
 * note: passing qualifiers is slightly less important than passing template
 * arguments because they are before the name (as opposed to templates
 * which are after) and most of our heuristics for typedefs
 * look tokens forward, not backward (actually a few now look backward too)
 *)
let find_qualifier_commentize xs =
  let rec aux xs =
    match xs with
    | [] -> ()
    | ({ t = TIdent _ } as t1) :: ({ t = TColCol _ } as t2) :: xs ->
        [ t1; t2 ]
        |> List.iter (fun tok ->
               change_tok tok
                 (TComment_Cpp
                    (Token_cpp.CplusplusQualifier, TH.info_of_tok tok.t)));
        aux xs
    (* need also to pass the top :: *)
    | ({ t = TColCol _ } as t2) :: xs ->
        [ t2 ]
        |> List.iter (fun tok ->
               change_tok tok
                 (TComment_Cpp
                    (Token_cpp.CplusplusQualifier, TH.info_of_tok tok.t)));
        aux xs
    (* recurse *)
    | _ :: xs -> aux xs
  in
  aux xs

(* assumes a view where:
 * - set_context_tag has been called.
 * TODO: filter the 'explicit' keyword? filter the TCppDirectiveOther
 *  have a filter_for_constructor?
 *)
let find_constructor xs =
  let rec aux xs =
    match xs with
    | [] -> ()
    (* { Foo(... *)
    | { t = TOBrace _ | TCBrace _ | TPtVirg _ | Texplicit _; _ }
      :: ({ t = TIdent (s1, i1); where = TV.InClassStruct s2 :: _; _ } as tok1)
      :: { t = TOPar _ }
      :: xs
      when s1 = s2 ->
        change_tok tok1 (TIdent_Constructor (s1, i1));
        aux xs
    (* public: Foo(...   could also filter the privacy directives so
     * need only one rule
     *)
    | { t = Tpublic _ | Tprotected _ | Tprivate _ }
      :: { t = TCol _ }
      :: ({ t = TIdent (s1, i1); where = TV.InClassStruct s2 :: _; _ } as tok1)
      :: { t = TOPar _ }
      :: xs
      when s1 = s2 ->
        change_tok tok1 (TIdent_Constructor (s1, i1));
        aux xs
    (* recurse *)
    | _ :: xs -> aux xs
  in
  aux xs

(* assumes a view where:
 * - template have been filtered but NOT the qualifiers!
 *)
let find_constructor_outside_class xs =
  let rec aux xs =
    match xs with
    | [] -> ()
    | { t = TIdent (s1, _); _ }
      :: { t = TColCol _ }
      :: ({ t = TIdent (s2, i2); _ } as tok)
      :: xs
      when s1 = s2 ->
        change_tok tok (TIdent_Constructor (s2, i2));
        aux (tok :: xs)
    (* recurse *)
    | _ :: xs -> aux xs
  in
  aux xs

(* assumes have:
 * - the typedefs
 * - the right context
 *
 * TODO: filter the TCppDirectiveOther,  have a filter_for_constructed?
 *)
let find_constructed_object_and_more xs =
  let rec aux xs =
    match xs with
    | [] -> ()
    | { t = Tdelete _ | Tnew _; _ }
      :: ({ t = TOCro i1 } as tok1)
      :: ({ t = TCCro i2 } as tok2)
      :: xs ->
        change_tok tok1 (TOCro_new i1);
        change_tok tok2 (TCCro_new i2);
        aux xs
    (* xx yy(1 ... *)
    | { t = TIdent_Typedef _; _ }
      :: { t = TIdent _; _ }
      :: ({ t = TOPar ii; where = InArgument :: _; _ } as tok1)
      :: xs ->
        change_tok tok1 (TOPar_CplusplusInit ii);
        aux xs
    (* int yy(1 ... *)
    | { t = tok; _ }
      :: { t = TIdent _; _ }
      :: ({ t = TOPar ii; where = InArgument :: _; _ } as tok1)
      :: xs
      when TH.is_basic_type tok ->
        change_tok tok1 (TOPar_CplusplusInit ii);
        aux xs
    (* xx& yy(1 ... *)
    | { t = TIdent_Typedef _; _ }
      :: { t = TAnd _ }
      :: { t = TIdent _; _ }
      :: ({ t = TOPar ii; where = InArgument :: _; _ } as tok1)
      :: xs ->
        change_tok tok1 (TOPar_CplusplusInit ii);
        aux xs
    (* xx yy(zz)
     * The InArgument heuristic can't guess anything when just have
     * idents inside the parenthesis. It's probably a constructed
     * object though.
     * TODO? could be a function declaration, especially when at Toplevel.
     * If inside a function, then very probably a constructed object.
     *)
    | { t = TIdent_Typedef _; _ }
      :: { t = TIdent _; _ }
      :: ({ t = TOPar ii } as tok1)
      :: { t = TIdent _; _ }
      :: { t = TCPar _ }
      :: xs ->
        change_tok tok1 (TOPar_CplusplusInit ii);
        aux xs
    (* xx yy(zz, ww) *)
    | { t = TIdent_Typedef _; _ }
      :: { t = TIdent _; _ }
      :: ({ t = TOPar ii } as tok1)
      :: { t = TIdent _; _ }
      :: { t = TComma _ }
      :: { t = TIdent _; _ }
      :: { t = TCPar _ }
      :: xs ->
        change_tok tok1 (TOPar_CplusplusInit ii);
        aux xs
    (* xx yy(&zz) *)
    | { t = TIdent_Typedef _; _ }
      :: { t = TIdent _; _ }
      :: ({ t = TOPar ii } as tok1)
      :: { t = TAnd _ }
      :: { t = TIdent _; _ }
      :: { t = TCPar _ }
      :: xs ->
        change_tok tok1 (TOPar_CplusplusInit ii);
        aux xs
    (* int(), probably part of operator declaration
     * could check that token before is a 'operator'
     *)
    | { t = kind } :: { t = TOPar _ } :: { t = TCPar _ } :: xs
      when TH.is_basic_type kind ->
        aux xs
    (* int(...)  unless it's int( * xxx ) *)
    | { t = _kind } :: { t = TOPar _ } :: { t = TMul _ } :: xs -> aux xs
    | ({ t = kind } as tok1) :: { t = TOPar _ } :: xs when TH.is_basic_type kind
      ->
        let newone =
          match kind with
          | Tchar ii -> Tchar_Constr ii
          | Tshort ii -> Tshort_Constr ii
          | Tint ii -> Tint_Constr ii
          | Tdouble ii -> Tdouble_Constr ii
          | Tfloat ii -> Tfloat_Constr ii
          | Tlong ii -> Tlong_Constr ii
          | Tbool ii -> Tbool_Constr ii
          | Tunsigned ii -> Tunsigned_Constr ii
          | Tsigned ii -> Tsigned_Constr ii
          | _ -> raise Impossible
        in
        change_tok tok1 newone;
        aux xs
    (* recurse *)
    | _ :: xs -> aux xs
  in
  aux xs
