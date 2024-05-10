(* Yoann Padioleau
 *
 * Copyright (C) 2010 Facebook
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
open Fpath_.Operators
open Parser_lisp
open Ast_lisp
module PS = Parsing_stat
module Log = Log_lib_parsing.Log

(* we don't need a full grammar for lisp code, so we put everything,
 * the token type, the helper in parser_ml. No token_helpers_lisp.ml
 *)
module TH = Parser_lisp

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * alt:
 *  - Could reuse the parser in ocamlsexp ? but they just have Atom | Sexp
 *    and I need to differentiate numbers in the highlighter, and
 *    also handling quoted, anti-quoted and other lisp special things.
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* the token list contains also the comment-tokens *)
type program_and_tokens = Ast_lisp.program option * Parser_lisp.token list

(*****************************************************************************)
(* Lexing only *)
(*****************************************************************************)

(* could factorize and take the tokenf and visitor_of_infof in argument
 * but sometimes copy-paste is ok.
 *)
let tokens input_source =
  let token = Lexer_lisp.token in
  Parsing_helpers.tokenize_all_and_adjust_pos input_source token
    TH.visitor_info_of_tok TH.is_eof
[@@profiling]

(*****************************************************************************)
(* Parser *)
(*****************************************************************************)

(* simple recursive descent parser *)
let rec sexps toks =
  match toks with
  | [] -> ([], [])
  | [ EOF _ ] -> ([], [])
  | (TCParen _ | TCBracket _) :: _ -> ([], toks)
  | xs ->
      let s, rest = sexp xs in
      let xs, rest = sexps rest in
      (s :: xs, rest)

and sexp toks =
  match toks with
  | [] -> raise Todo
  | x :: xs -> (
      match x with
      | TComment _
      | TCommentSpace _
      | TCommentNewline _ ->
          raise Impossible
      | TNumber x -> (Atom (Number x), xs)
      | TString x -> (Atom (String x), xs)
      | TIdent x -> (Atom (Id x), xs)
      | TOParen t1 -> (
          let xs, rest = sexps xs in
          match rest with
          | TCParen t2 :: rest -> (Sexp (t1, xs, t2), rest)
          | _ -> raise (Parsing_error.Other_error ("unclosed parenthesis", t1)))
      | TOBracket t1 -> (
          let xs, rest = sexps xs in
          match rest with
          | TCBracket t2 :: rest -> (Sexp (t1, xs, t2), rest)
          | _ -> raise (Parsing_error.Other_error ("unclosed bracket", t1)))
      | TCParen t
      | TCBracket t ->
          raise
            (Parsing_error.Other_error
               ("closing bracket/paren without opening one", t))
      | TQuote t ->
          let s, rest = sexp xs in
          (Special ((Quote, t), s), rest)
      | TBackQuote t ->
          let s, rest = sexp xs in
          (Special ((BackQuote, t), s), rest)
      | TAt t ->
          let s, rest = sexp xs in
          (Special ((At, t), s), rest)
      | TComma t ->
          let s, rest = sexp xs in
          (Special ((Comma, t), s), rest)
      (* hmmm probably unicode *)
      | TUnknown t -> (Atom (String (Tok.content_of_tok t, t)), xs)
      | EOF t -> raise (Parsing_error.Other_error ("unexpected eof", t)))

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let parse filename =
  let stat = Parsing_stat.default_stat !!filename in
  let toks_orig = tokens (Parsing_helpers.file !!filename) in

  let toks = toks_orig |> List_.exclude TH.is_comment in

  let ast =
    try
      match sexps toks with
      | xs, [] -> Some xs
      | _, x :: _xs ->
          raise
            (Parsing_error.Other_error ("trailing constructs", TH.info_of_tok x))
    with
    | Parsing_error.Other_error (s, info) ->
        Log.err (fun m ->
            m "Parse error: %s, {%s} at %s" s (Tok.content_of_tok info)
              (Tok.stringpos_of_tok info));
        stat.PS.error_line_count <- stat.PS.total_line_count;
        None
    | exn -> Exception.catch_and_reraise exn
  in
  ((ast, toks_orig), stat)
[@@profiling]

let parse_program file =
  let (ast, _toks), _stat = parse file in
  Common2.some ast
