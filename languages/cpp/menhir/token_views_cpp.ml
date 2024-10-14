(* Yoann Padioleau
 *
 * Copyright (C) 2007, 2008 Ecole des Mines de Nantes
 * Copyright (C) 2011, 2014 Facebook
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
module Flag = Flag_parsing
module Flag_cpp = Flag_parsing_cpp
module TH = Token_helpers_cpp
open Parser_cpp

(*****************************************************************************)
(* Prelude  *)
(*****************************************************************************)
(*
 * This module makes it easier to write some fuzzy parsing heuristics
 * by offering different "views" over the same set of tokens.
 *
 * Normally I should not use ref/mutable in the token_extended type below
 * and instead have a set of functions taking a list of tokens and
 * returning a list of tokens. The problem is that to make easier some
 * functions, it is better to work on better representation, on "views"
 * over this list of tokens. But then modifying those views and get
 * back from those views to the original simple list of tokens is
 * tedious. One way is to maintain next to the view a list of "actions"
 * (I was using a hash storing the charpos of the token and associating
 * the action) but it is tedious too. Simpler to use mutable/ref. We
 * use the same idea that we use when working on the Ast.
 *
 * old: when I was using the list of "actions" next to the views, the hash
 * indexed by the charpos, there could have been some problems:
 * how my fake_pos interact with the way I tag and adjust token ?
 * because I base my tagging on the position of the token ! so sometimes
 * could tag another fakeInfo that should not be tagged ?
 * fortunately I don't use anymore this technique.
 *)

(*****************************************************************************)
(* Some debugging functions  *)
(*****************************************************************************)
let pr2, _pr2_once = Common2.mk_pr2_wrappers Flag.verbose_parsing

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type token_extended = {
  (* chose 't' and not 'tok' to have a short name because we will write
   * lots of ocaml patterns around this ... so better to be short
   *)
  mutable t : Parser_cpp.token;
  (* In C++ we have functions inside classes, so need a stack of context *)
  mutable where : context list;
  (* less: need also a after ? *)
  mutable new_tokens_before : Parser_cpp.token list;
  (* line x col  cache (more easily accessible) of the info in the token *)
  line : int;
  col : int;
}

(* The strategy to tag is mostly to look at the token(s) before the '{' *)
and context =
  | InTopLevel
  | InClassStruct of string
  (* can be __anon__ *)
  | InEnum
  | InInitializer
  | InAssign
  | InParameter
  | InArgument
  (* TODO actually commented in token_view_context because of c++ *)
  | InFunction
(*
    | InTemplateParam (* TODO *)
*)

(* InCondition ? InParenExpr ? *)

(* x list list, because x list separated by ',' *)
type paren_grouped =
  | Parenthised of paren_grouped list list * token_extended list
  | PToken of token_extended

type brace_grouped =
  | Braceised of
      brace_grouped list list * token_extended * token_extended option
  | BToken of token_extended

(* Far better data structure than doing hacks in the lexer or parser
 * because in lexer we don't know to which ifdef a endif is related
 * and so when we want to comment a ifdef, we don't know which endif
 * we must also comment. Especially true for the #if 0 which sometimes
 * have a #else part.
 *
 * x list list, because x list separated by #else or #elif
 *)
type ifdef_grouped =
  | Ifdef of ifdef_grouped list list * token_extended list
  | Ifdefbool of bool * ifdef_grouped list list * token_extended list
  | NotIfdefLine of token_extended list

type 'a line_grouped = Line of 'a list

type body_function_grouped =
  | BodyFunction of token_extended list
  | NotBodyLine of token_extended list

(* quite similar to ast_fuzzy.ml but with extended token *)
type multi_grouped =
  | Braces of token_extended * multi_grouped list * token_extended option
  | Parens of token_extended * multi_grouped list * token_extended option
  | Angle of token_extended * multi_grouped list * token_extended option
  | Tok of token_extended

(* with tarzan *)

exception UnclosedSymbol of string

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let mk_token_extended x =
  let info = TH.info_of_tok x in
  let line, col = (Tok.line_of_tok info, Tok.col_of_tok info) in
  {
    t = x;
    line;
    col;
    (* we use Common.hd_exn "unexpected empty list" at a few places, so convenient to have a sentinel *)
    where = [ InTopLevel ];
    new_tokens_before = [];
  }

let mk_token_fake x =
  { t = x; line = -1; col = -1; where = [ InTopLevel ]; new_tokens_before = [] }

let rebuild_tokens_extented toks_ext =
  let _tokens = ref [] in
  toks_ext
  |> List.iter (fun tok ->
         tok.new_tokens_before |> List.iter (fun x -> Stack_.push x _tokens);
         Stack_.push tok.t _tokens);
  let tokens = List.rev !_tokens in
  tokens |> Common2.acc_map mk_token_extended

(*****************************************************************************)
(* View builders  *)
(*****************************************************************************)

(* ------------------------------------------------------------------------- *)
(* Parens *)
(* ------------------------------------------------------------------------- *)

(* return the body of the parenthised expression and the rest of the tokens *)
let rec mk_parameters extras acc_before_sep xs =
  match xs with
  | [] ->
      (* maybe because of #ifdef which "opens" '(' in 2 branches *)
      pr2 "PB: not found closing paren in fuzzy parsing";
      ([ List.rev acc_before_sep ], List.rev extras, [])
  | x :: xs -> (
      match x.t with
      (* synchro *)
      | xx when TH.is_obrace xx && x.col =|= 0 ->
          pr2 "PB: found synchro point } in paren";
          ([ List.rev acc_before_sep ], List.rev extras, x :: xs)
      | xx when TH.is_cpar xx ->
          ([ List.rev acc_before_sep ], List.rev (x :: extras), xs)
      | xx when TH.is_opar xx ->
          let body, extrasnest, xs = mk_parameters [ x ] [] xs in
          mk_parameters extras
            (Parenthised (body, extrasnest) :: acc_before_sep)
            xs
      | TComma _ ->
          let body, extras, xs = mk_parameters (x :: extras) [] xs in
          (List.rev acc_before_sep :: body, extras, xs)
      | _ -> mk_parameters extras (PToken x :: acc_before_sep) xs)

(* todo: synchro ! use more indentation
 * if paren not closed and same indentation level, certainly because
 * part of a mid-ifdef-expression.
 *
 * c++ext: TODO: need to handle templates here.
 * The parenthized view must not consider the ',' in expressions
 * like foo(lexical cast<string,int>, ...) as a separator for the arguments
 * of foo(), otherwise we will get [lexical_cast<string;  ...] which
 * could confuse some heuristics.
 *
 * pre: have done the TInf->TInf_Template translation.
 *)
let mk_parenthised xs =
  (* Tail-recursive to prevent stack overflows. *)
  let rec aux acc xs =
    match xs with
    | [] -> List.rev acc
    | x :: xs -> (
        match x.t with
        | xx when TH.is_opar xx ->
            let body, extras, xs = mk_parameters [ x ] [] xs in
            aux (Parenthised (body, extras) :: acc) xs
        | _ -> aux (PToken x :: acc) xs)
  in
  aux [] xs

(* ------------------------------------------------------------------------- *)
(* Brace *)
(* ------------------------------------------------------------------------- *)

let rec mk_braceised xs =
  match xs with
  | [] -> []
  | x :: xs -> (
      match x.t with
      | xx when TH.is_obrace xx ->
          let body, endbrace, xs = mk_braceised_aux [] xs in
          Braceised (body, x, endbrace) :: mk_braceised xs
      | xx when TH.is_cbrace xx ->
          pr2 "PB: found closing brace alone in fuzzy parsing";
          BToken x :: mk_braceised xs
      | _ -> BToken x :: mk_braceised xs)

(* return the body of the parenthised expression and the rest of the tokens *)
and mk_braceised_aux acc xs =
  match xs with
  | [] ->
      (* maybe because of #ifdef which "opens" '(' in 2 branches *)
      pr2 "PB: not found closing brace in fuzzy parsing";
      ([ List.rev acc ], None, [])
  | x :: xs -> (
      match x.t with
      | xx when TH.is_cbrace xx -> ([ List.rev acc ], Some x, xs)
      | xx when TH.is_obrace xx ->
          let body, endbrace, xs = mk_braceised_aux [] xs in
          mk_braceised_aux (Braceised (body, x, endbrace) :: acc) xs
      | _ -> mk_braceised_aux (BToken x :: acc) xs)

(* ------------------------------------------------------------------------- *)
(* Ifdefs *)
(* ------------------------------------------------------------------------- *)
let rec mk_ifdef xs =
  match xs with
  | [] -> []
  | x :: xs -> (
      match x.t with
      | TIfdef _ ->
          let body, extra, xs = mk_ifdef_parameters [ x ] [] xs in
          Ifdef (body, extra) :: mk_ifdef xs
      | TIfdefBool (b, _) ->
          let body, extra, xs = mk_ifdef_parameters [ x ] [] xs in

          (* if not passing, then consider a #if 0 as an ordinary #ifdef *)
          if !Flag_cpp.if0_passing then
            Ifdefbool (b, body, extra) :: mk_ifdef xs
          else Ifdef (body, extra) :: mk_ifdef xs
      | TIfdefMisc (b, _)
      | TIfdefVersion (b, _) ->
          let body, extra, xs = mk_ifdef_parameters [ x ] [] xs in
          Ifdefbool (b, body, extra) :: mk_ifdef xs
      | _ ->
          (* todo? can have some Ifdef in the line ? *)
          let line, xs = List_.span (fun y -> y.line =|= x.line) (x :: xs) in
          NotIfdefLine line :: mk_ifdef xs)

and mk_ifdef_parameters extras acc_before_sep xs =
  match xs with
  | [] ->
      (* Note that mk_ifdef is assuming that CPP instruction are alone
       * on their line. Because I do a span (fun x -> is_same_line ...)
       * I might take with me a #endif if this one is mixed on a line
       * with some "normal" tokens.
       *)
      pr2 "PB: not found closing ifdef in fuzzy parsing";
      ([ List.rev acc_before_sep ], List.rev extras, [])
  | x :: xs -> (
      match x.t with
      | TEndif _ -> ([ List.rev acc_before_sep ], List.rev (x :: extras), xs)
      | TIfdef _ ->
          let body, extrasnest, xs = mk_ifdef_parameters [ x ] [] xs in
          mk_ifdef_parameters extras
            (Ifdef (body, extrasnest) :: acc_before_sep)
            xs
      | TIfdefBool (b, _) ->
          let body, extrasnest, xs = mk_ifdef_parameters [ x ] [] xs in

          if !Flag_cpp.if0_passing then
            mk_ifdef_parameters extras
              (Ifdefbool (b, body, extrasnest) :: acc_before_sep)
              xs
          else
            mk_ifdef_parameters extras
              (Ifdef (body, extrasnest) :: acc_before_sep)
              xs
      | TIfdefMisc (b, _)
      | TIfdefVersion (b, _) ->
          let body, extrasnest, xs = mk_ifdef_parameters [ x ] [] xs in
          mk_ifdef_parameters extras
            (Ifdefbool (b, body, extrasnest) :: acc_before_sep)
            xs
      | TIfdefelse _
      | TIfdefelif _ ->
          let body, extras, xs = mk_ifdef_parameters (x :: extras) [] xs in
          (List.rev acc_before_sep :: body, extras, xs)
      | _ ->
          let line, xs = List_.span (fun y -> y.line =|= x.line) (x :: xs) in
          mk_ifdef_parameters extras (NotIfdefLine line :: acc_before_sep) xs)

(* ------------------------------------------------------------------------- *)
(* Lines (of parens) *)
(* ------------------------------------------------------------------------- *)

let line_of_paren = function
  | PToken x -> x.line
  | Parenthised (_xxs, info_parens) -> (
      match info_parens with
      | [] -> raise Impossible
      | x :: _xs -> x.line)

(* old
   let rec span_line_paren line = function
   | [] -> [],[]
   | x::xs ->
      (match x with
      | PToken tok when TH.is_eof tok.t ->
          [], x::xs
      | _ ->
        if line_of_paren x = line
        then
          let (l1, l2) = span_line_paren line xs in
          (x::l1, l2)
        else ([], x::xs)
      )

   let rec mk_line_parenthised xs =
   match xs with
   | [] -> []
   | x::xs ->
      let line_no = line_of_paren x in
      let line, xs = span_line_paren line_no xs in
      Line (x::line)::mk_line_parenthised xs
*)

let line_range_of_paren = function
  | PToken x -> (x.line, x.line)
  | Parenthised (_xxs, info_parens) -> (
      match info_parens with
      | [] -> raise Impossible
      | x :: xs ->
          let lines_no = x :: xs |> List_.map (fun x -> x.line) in
          (Common2.minimum lines_no, Common2.maximum lines_no))

let rec span_line_paren_range (imin, imax) = function
  | [] -> ([], [])
  | x :: xs -> (
      match x with
      | PToken tok when TH.is_eof tok.t -> ([], x :: xs)
      | _ ->
          if line_of_paren x >= imin && line_of_paren x <= imax then
            (* may need to extend *)
            let _imin', imax' = line_range_of_paren x in
            let l1, l2 = span_line_paren_range (imin, max imax imax') xs in
            (x :: l1, l2)
          else ([], x :: xs))

let rec mk_line_parenthised xs =
  match xs with
  | [] -> []
  | x :: xs ->
      let line_range = line_range_of_paren x in
      let line, xs = span_line_paren_range line_range xs in
      Line (x :: line) :: mk_line_parenthised xs

(* ------------------------------------------------------------------------- *)
(* Function body *)
(* ------------------------------------------------------------------------- *)
let rec mk_body_function_grouped xs =
  match xs with
  | [] -> []
  | x :: xs -> (
      match x with
      | { t = TOBrace _; col = 0; _ } -> (
          let is_closing_brace = function
            | { t = TCBrace _; col = 0; _ } -> true
            | _ -> false
          in
          let body, xs = List_.span (fun x -> not (is_closing_brace x)) xs in
          match xs with
          | { t = TCBrace _; col = 0; _ } :: xs ->
              BodyFunction body :: mk_body_function_grouped xs
          | [] ->
              pr2 "PB:not found closing brace in fuzzy parsing";
              [ NotBodyLine body ]
          | _ -> raise Impossible)
      | _ ->
          let line, xs = List_.span (fun y -> y.line =|= x.line) (x :: xs) in
          NotBodyLine line :: mk_body_function_grouped xs)

(* ------------------------------------------------------------------------- *)
(* Multi ('{', '(', '<')  (could also do '[' ?) *)
(* ------------------------------------------------------------------------- *)

(* Assumes work on a list of tokens without comments, without ifdefs
 * (todo? and without #define?).
 * Used for typedef inference. Now also used for fuzzy parsing!
 *
 * todo? more fault tolerance, if col == 0 and { the reset!
 * less: could check that it's consistent with the indentation
 *
 *)
let mk_multi xs =
  let rec consume x xs =
    match x with
    | { t = (*TOBrace ii*) tok; _ } when TH.is_obrace tok ->
        let body, closing, rest = look_close_brace x [] xs in
        (Braces (x, body, closing), rest)
    | { t = (*TOPar ii*) tok; _ } when TH.is_opar tok ->
        let body, closing, rest = look_close_paren x [] xs in
        (Parens (x, body, closing), rest)
    | { t = TInf_Template _ii; _ } ->
        let body, closing, rest = look_close_template x [] xs in
        (Angle (x, body, closing), rest)
    | x -> (Tok x, xs)
  and aux xs =
    match xs with
    | [] -> []
    | x :: xs ->
        let x', xs' = consume x xs in
        x' :: aux xs'
  and look_close_brace tok_start accbody xs =
    match xs with
    | [] ->
        raise
          (UnclosedSymbol
             (spf "PB look_close_brace (started at %d)"
                (TH.line_of_tok tok_start.t)))
    | x :: xs -> (
        match x with
        | { t = TCBrace _ii; _ } -> (List.rev accbody, Some x, xs)
        (* Many macros have unclosed '{'. An alternative
         * would be to work on a view where define has been filtered
         *)
        | { t = TCommentNewline_DefineEndOfMacro _ii; _ } ->
            (List.rev accbody, None, x :: xs)
        | _ ->
            let x', xs' = consume x xs in
            look_close_brace tok_start (x' :: accbody) xs')
  and look_close_paren tok_start accbody xs =
    match xs with
    | [] ->
        raise
          (UnclosedSymbol
             (spf "PB look_close_paren (started at %d)"
                (TH.line_of_tok tok_start.t)))
    | x :: xs -> (
        match x with
        | { t = (*TCPar ii*) tok; _ } when TH.is_cpar tok ->
            (List.rev accbody, Some x, xs)
        | _ ->
            let x', xs' = consume x xs in
            look_close_paren tok_start (x' :: accbody) xs')
  and look_close_template tok_start accbody xs =
    match xs with
    | [] ->
        raise
          (UnclosedSymbol
             (spf "PB look_close_template (started at %d)"
                (TH.line_of_tok tok_start.t)))
    | x :: xs -> (
        match x with
        | { t = TSup_Template _ii; _ } -> (List.rev accbody, Some x, xs)
        | _ ->
            let x', xs' = consume x xs in
            look_close_template tok_start (x' :: accbody) xs')
  in
  aux xs

let split_comma xs =
  xs
  |> Common2.split_gen_when (function
       | Tok { t = TComma _; _ } :: xs -> Some xs
       | _ -> None)

(*****************************************************************************)
(* View iterators  *)
(*****************************************************************************)

let rec iter_token_paren f xs =
  xs
  |> List.iter (function
       | PToken tok -> f tok
       | Parenthised (xxs, info_parens) ->
           info_parens |> List.iter f;
           xxs |> List.iter (fun xs -> iter_token_paren f xs))

let rec iter_token_brace f xs =
  xs
  |> List.iter (function
       | BToken tok -> f tok
       | Braceised (xxs, tok1, tok2opt) ->
           f tok1;
           Option.iter f tok2opt;
           xxs |> List.iter (fun xs -> iter_token_brace f xs))

let rec iter_token_ifdef f xs =
  xs
  |> List.iter (function
       | NotIfdefLine xs -> xs |> List.iter f
       | Ifdefbool (_, xxs, info_ifdef)
       | Ifdef (xxs, info_ifdef) ->
           info_ifdef |> List.iter f;
           xxs |> List.iter (iter_token_ifdef f))

let rec iter_token_multi f xs =
  xs
  |> List.iter (function
       | Tok t -> f t
       | Braces (t1, xs, t2)
       | Parens (t1, xs, t2)
       | Angle (t1, xs, t2) ->
           f t1;
           iter_token_multi f xs;
           Option.iter f t2)

let tokens_of_paren xs =
  let g = ref [] in
  xs |> iter_token_paren (fun tok -> Stack_.push tok g);
  List.rev !g

let tokens_of_paren_ordered xs =
  let g = ref [] in

  let rec aux_tokens_ordered = function
    | PToken tok -> Stack_.push tok g
    | Parenthised (xxs, info_parens) ->
        let opar, cpar, commas =
          match info_parens with
          | opar :: xs -> (
              match List.rev xs with
              | cpar :: xs -> (opar, cpar, List.rev xs)
              | _ -> raise Impossible)
          | _ -> raise Impossible
        in
        Stack_.push opar g;
        aux_args (xxs, commas);
        Stack_.push cpar g
  and aux_args (xxs, commas) =
    match (xxs, commas) with
    | [], [] -> ()
    | [ xs ], [] -> xs |> List.iter aux_tokens_ordered
    | xs :: ys :: xxs, comma :: commas ->
        xs |> List.iter aux_tokens_ordered;
        Stack_.push comma g;
        aux_args (ys :: xxs, commas)
    | _ -> raise Impossible
  in

  xs |> List.iter aux_tokens_ordered;
  List.rev !g

let tokens_of_multi_grouped xs =
  let res = ref [] in

  let add x = Stack_.push x res in

  let rec aux xs =
    xs
    |> List.iter (function
         | Tok t1 -> add t1
         | Braces (t1, xs, t2)
         | Parens (t1, xs, t2)
         | Angle (t1, xs, t2) ->
             add t1;
             aux xs;
             Option.iter add t2)
  in
  aux xs;
  List.rev !res

(*****************************************************************************)
(* vof  *)
(*****************************************************************************)

let vof_context = function
  | InTopLevel -> OCaml.VSum ("T", [])
  | InClassStruct _s -> OCaml.VSum ("C", [])
  | InEnum -> OCaml.VSum ("E", [])
  | InInitializer -> OCaml.VSum ("I", [])
  | InAssign -> OCaml.VSum ("=", [])
  | InParameter -> OCaml.VSum ("P", [])
  | InArgument -> OCaml.VSum ("A", [])
  | InFunction -> OCaml.VSum ("F", [])
(*
  | InTemplateParam -> OCaml.VSum ("<>", [])
*)

let vof_token_extended t =
  let info = TH.info_of_tok t.t in
  let str = Tok.content_of_tok info in
  let xs = List_.map vof_context t.where in
  OCaml.VTuple [ OCaml.VString str; OCaml.VList xs ]

let rec vof_multi_grouped = function
  | Braces (v1, v2, v3) ->
      let v1 = vof_token_extended v1
      and v2 = OCaml.vof_list vof_multi_grouped v2
      and v3 = OCaml.vof_option vof_token_extended v3 in
      OCaml.VSum ("Braces", [ v1; v2; v3 ])
  | Parens (v1, v2, v3) ->
      let v1 = vof_token_extended v1
      and v2 = OCaml.vof_list vof_multi_grouped v2
      and v3 = OCaml.vof_option vof_token_extended v3 in
      OCaml.VSum ("Parens", [ v1; v2; v3 ])
  | Angle (v1, v2, v3) ->
      let v1 = vof_token_extended v1
      and v2 = OCaml.vof_list vof_multi_grouped v2
      and v3 = OCaml.vof_option vof_token_extended v3 in
      OCaml.VSum ("Angle", [ v1; v2; v3 ])
  | Tok v1 ->
      let v1 = vof_token_extended v1 in
      OCaml.VSum ("Tok", [ v1 ])

let vof_multi_grouped_list xs =
  let v = OCaml.VList (xs |> List_.map vof_multi_grouped) in
  v
