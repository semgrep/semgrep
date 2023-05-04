(* Yoann Padioleau
 *
 * Copyright (C) 2013 Facebook
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
open Parser_php
module TH = Token_helpers_php

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * This module transforms certain tokens like '>>', normally a T_SR
 * into two TGREATER tokens which helps avoid using ugly tricks in the grammar
 * regarding generics.
 *
 * This is similar to what we do for C/C++.
 * See cpp/.../parsing_hacks.ml for more information.
 *
 * In Hack they maintain those different states (InToplevel, InFunction,
 * InBlock, ...) in the lexer itself, I prefer for now to separate
 * concerns and do that entirely post-lexing (which introduces some performance
 * degradation, from 195s to parse www to 209s).
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)
type env = { stack : ctx list; misc : unit }

and ctx =
  | Toplevel
  | ClassHeader
  | ClassBody
  | FunctionHeader
  | TypeHeader
  | UserAttribute
  | Block

(*****************************************************************************)
(* generics *)
(*****************************************************************************)

(* Split a single (assumed to be 2-chars wide) info and turn it
   into a (1-char) lhs and rhs. Used to convert `>>` into two `>`
*)
let split_two_char pi =
  let lhs = { pi with Tok.str = String.sub pi.Tok.str 0 1 } in
  let rhs =
    {
      Tok.str = String.sub pi.Tok.str 1 1;
      pos =
        { pi.pos with charpos = pi.pos.charpos + 1; column = pi.pos.column + 1 };
    }
  in
  (lhs, rhs)

let split_two_char_info i =
  let tok =
    match i with
    | Tok.OriginTok t -> t
    | _ -> failwith "Parse error..."
  in

  let lhspi, rhspi = split_two_char tok in
  let lhs = Tok.OriginTok lhspi in
  let rhs = Tok.OriginTok rhspi in
  (lhs, rhs)

(*
 * Utilities for lambda parsing
 *)

(*
 * Checks if the given tokens are compatible with a set of lambda params.
 * It must either be empty, as in () ==> ... or contain one variable/variadic.
 *
 * Both of these cases are not compatible with typehints, so we can safely
 * determine if a (...) expression is part of lambda's params or its typehint.
 *)
let is_params toks =
  List.length toks > 0
  && (List.for_all
        (function
          | T_LAMBDA_OPAR _
          | T_LAMBDA_CPAR _
          | TOPAR _
          | TCPAR _ ->
              true
          | x -> TH.is_comment x)
        toks
     || List.exists
          (function
            | T_VARIABLE _
            | T_ELLIPSIS _ ->
                true
            | _ -> false)
          toks)

(* Looks to see if the next token is a variable (ignoring comments) *)
let rec is_variable toks =
  match toks with
  | [] -> false
  | T_VARIABLE _ :: _ -> true
  | x :: xs -> if TH.is_comment x then is_variable xs else false

(*
 * Find the next group of parenthesized tokens, being sure to balance parens.
 * Returns an empty list if the parens were imbalanced or the first non-comment
 * token was anything except a close paren.
 *
 * Replaces the opening/closing parens with lambda parens if `replace` is true.
 *)
let find_paren_tokens toks replace =
  let rec aux toks acc depth =
    match toks with
    | [] -> ([], []) (* failure *)
    | x :: xs -> (
        match x with
        | TCPAR t ->
            let x' = if depth == 0 && replace then T_LAMBDA_CPAR t else x in
            aux xs (x' :: acc) (depth + 1)
        | TOPAR t ->
            if depth == 1 then
              let x' = if replace then T_LAMBDA_OPAR t else x in
              (List.rev (x' :: acc), xs)
            else aux xs (x :: acc) (depth - 1)
        | T_SR t ->
            if depth > 0 then
              if replace then
                (* In the context of lambda parens, >> only makes sense
                 * if we split it into two > tokens *)
                let lhs, rhs = split_two_char_info t in
                aux xs (TGREATER rhs :: TGREATER lhs :: acc) depth
              else aux xs (x :: acc) depth
            else ([], [])
        | _ ->
            if TH.is_comment x || depth > 0 then aux xs (x :: acc) depth
            else (* couldn't find the first closing paren *)
              ([], []))
  in
  aux toks [] 0

(*
 * Try to (roughly) match a lambda typehint - may have false positives.
 * On the other hand, it's guaranteed that any valid typehint will be matched.
 * False positives will most likely lead to an invalid set of lambda
 * parens, though.
 *)
let find_typehint toks =
  let rec aux toks acc depth =
    match toks with
    | [] -> ([], []) (* failure *)
    (* assume parens/brackets are balanced correctly *)
    | x :: xs -> (
        match x with
        | T_LAMBDA_CPAR _
        | TCPAR _
        | TGREATER _ ->
            aux xs (x :: acc) (depth + 1)
        | T_LAMBDA_OPAR _
        | TOPAR _
        | TSMALLER _ ->
            aux xs (x :: acc) (depth - 1)
        | T_SR t ->
            (* >> when we're looking for a typehint is only valid in the context
             * of closing a template, so split it up. *)
            let lhs, rhs = split_two_char_info t in
            aux xs (TGREATER rhs :: TGREATER lhs :: acc) (depth + 2)
        | T_DOUBLE_ARROW _
        | TOBRACE _
        | TCBRACE _ ->
            ([], []) (* absolutely will not be in a typehint *)
        | TCOLON _ ->
            if depth == 0 then (List.rev (x :: acc), xs)
            else aux xs (x :: acc) depth
        | _ -> aux xs (x :: acc) depth)
  in
  aux toks [] 0

(*****************************************************************************)
(* Fix tokens *)
(*****************************************************************************)

let fix_tokens xs =
  let rec aux env acc xs =
    match xs with
    (* need an acc, to be tail recursive, otherwise get some stack overflow *)
    | [] -> List.rev acc
    (* '>>', maybe should be split in two tokens '>' '>' when in generic
     * context
     *)
    | T_SR ii :: xs -> (
        match env.stack with
        (* type context, those are the only places where types allowed for
         * now, which makes the job easier than in parsing_hacks_java.ml
         *)
        | (ClassHeader | ClassBody | TypeHeader | FunctionHeader) :: _ ->
            let lhs, rhs = split_two_char_info ii in
            aux env (TGREATER rhs :: TGREATER lhs :: acc) xs
        | UserAttribute :: rest ->
            aux { env with stack = rest } (T_SR ii :: acc) xs
        | _ -> aux env (T_SR ii :: acc) xs)
    (* This must be part of a lambda expression.
     * The parameters of a lambda expression are extremely difficult to parse
     * due to their similarity to standard expressions.
     * To get around this, we'll try to mark the opening and closing parens
     * of the lambda's parameters with special lambda paren tokens.
     *)
    | T_DOUBLE_ARROW arrow :: xs ->
        let replaced, rest =
          (* Nothing needs to be done for $x ==> ... *)
          if is_variable acc then ([], acc)
          else
            (* The majority of the time, lambdas aren't typehinted so let's just
             * eagerly replace the parens assuming these are the params. *)
            let toks, rest = find_paren_tokens acc true in
            match toks with
            (* Not a set of parens - this is probably a typehint. *)
            | [] -> (
                let typehint, rest = find_typehint acc in
                match typehint with
                | [] -> ([], acc) (* ignore; let the parser deal with it *)
                | _ ->
                    let params, rest2 = find_paren_tokens rest true in
                    if is_params params then (typehint @ params, rest2)
                    else ([], acc)
                (* ignore *))
            (* There are two possibilities now:
             *
             * 1) The typehint is a tuple or function, in which case this
             *    closing paren is part of a typehint.
             * 2) The closing paren is part of the parameters.
             *
             * is_params will be able to distinguish between the two cases.
             *)
            | _ -> (
                if is_params toks then (toks, rest)
                else
                  (* try finding a typehint *)
                  let typehint, rest = find_typehint acc in
                  match typehint with
                  | [] -> ([], acc) (* no match *)
                  | _ ->
                      let params, rest2 = find_paren_tokens rest true in
                      if is_params params then (typehint @ params, rest2)
                      else ([], acc))
          (* ignore *)
        in
        aux env (T_DOUBLE_ARROW arrow :: (replaced @ rest)) xs
    | x :: xs ->
        let stack =
          (* quite similar to hack/lexing_modes.ml *)
          match (x, env.stack) with
          (* ugly: we check we are at toplevel because the keyword 'class'
           * could be used in a different context as part of an XHP attribute
           * name, see ident_xhp_attr_name_atom rule in parser_php.mly
           *)
          | (T_CLASS _ | T_TRAIT _ | T_INTERFACE _), Toplevel :: _rest ->
              ClassHeader :: env.stack
          | T_TYPE _, Toplevel :: _rest -> TypeHeader :: env.stack
          | T_FUNCTION _, (Toplevel | ClassHeader) :: _rest ->
              FunctionHeader :: env.stack
          | T_FUNCTION _, Block :: _rest -> FunctionHeader :: env.stack
          (* also FunctionHeader because we can have attributes on parameters *)
          | T_SL _, (Toplevel | ClassBody | FunctionHeader) :: _rest ->
              UserAttribute :: env.stack
          | TOBRACE _ii, ClassHeader :: rest -> ClassBody :: rest
          (* subtle: do not do Block::env.stack here otherwise we will
           * not pop up enough to get back to a Toplevel context
           *)
          | TOBRACE _ii, FunctionHeader :: rest -> Block :: rest
          | TOBRACE _ii, _ -> Block :: env.stack
          | (T_CURLY_OPEN _ | T_DOLLAR_OPEN_CURLY_BRACES _), _ ->
              Block :: env.stack
          | TCBRACE _ii, _x :: xs -> xs
          | TCBRACE ii, [] ->
              failwith
                (spf "unmatching closing brace at %s" (Tok.stringpos_of_tok ii))
          | TSEMICOLON _ii, (FunctionHeader | TypeHeader) :: rest -> rest
          (* default case *)
          | _, st -> st
        in
        aux { env with stack } (x :: acc) xs
  in
  aux { stack = [ Toplevel ]; misc = () } [] xs
  [@@profiling]
