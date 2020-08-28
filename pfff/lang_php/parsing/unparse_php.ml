(*s: unparse_php.ml *)
(*s: Facebook copyright *)
(* Yoann Padioleau
 * 
 * Copyright (C) 2009-2013 Facebook
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
(*e: Facebook copyright *)
open Common 

open Cst_php 
module V = Visitor_php
module TH = Token_helpers_php

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * There are multiple ways to unparse PHP code:
 *  - one can iterate over the AST, and print its leaves, but 
 *    comments and spaces are not in the AST so you need
 *    some extra code that also visits the tokens and try to "sync" the
 *    visit of the AST with the tokens
 *  - one can iterate over the tokens which contain comments and spaces
 *    but this can be too low level
 *  - one can use a real pretty printer with a boxing or backtracking model
 *    working on a AST extended with comments (see julien's ast_pretty_print/)
 * 
 * Right now the preferred method for spatch is the second one. The pretty
 * printer currently is too different from our coding conventions
 * (also because we don't have precise coding conventions).
 * This token-based unparser handles transfo annotations (Add/Remove).
 * 
 * related: the sexp/json "exporters".
 * 
 * note: this module could be in analyze_php/ instead of parsing_php/, 
 * but it's maybe good to have the basic parser/unparser together.
 *)

(*****************************************************************************)
(* Unparsing using AST visitor *)
(*****************************************************************************)

(* This will not preserve spaces and comments but it's useful
 * and good enough for printing small chunk of PHP code for debugging
 * purpose. We also try to preserve the line numbers.
 *)
let string_of_program ast = 
  Common2.with_open_stringbuf (fun (_pr_with_nl, buf) ->
    let pp s = Buffer.add_string buf s in
    let cur_line = ref 1 in

    pp "<?php";
    pp "\n"; 
    incr cur_line;

    let visitor = V.mk_visitor { V.default_visitor with
      V.kinfo = (fun (_k, _) info ->
        match info.Parse_info.token with
        | Parse_info.OriginTok p ->
            let line = p.Parse_info.line in 
            if line > !cur_line
            then begin
              (line - !cur_line) |> Common2.times (fun () -> pp "\n"); 
              cur_line := line;
            end;
            let s =  p.Parse_info.str in
            pp s; pp " ";
        | Parse_info.FakeTokStr (s, _opt) ->
            pp s; pp " ";
            if s = ";" 
            then begin
              pp "\n";
              incr cur_line;
            end
        | Parse_info.Ab ->
            ()
        | Parse_info.ExpandedTok _ ->
            raise Todo
      );
    }
    in
    visitor (Program ast);
  )

(*****************************************************************************)
(* Even simpler unparser using AST visitor *)
(*****************************************************************************)

(* This function is used notably in spatch to pretty print matched code
 * in metavariables.
 *)
   

let string_of_any any = 
  Common2.with_open_stringbuf (fun (_pr_with_nl, buf) ->
    let pp s = Buffer.add_string buf s in
    
    let toks = ref [] in
    
    let hooks = { V.default_visitor with
      V.kinfo = (fun (_k, _) info ->
        match info.Parse_info.token with
        | Parse_info.OriginTok p ->
          let s =  p.Parse_info.str in
          Common.push s toks
        | Parse_info.FakeTokStr (s, _opt) ->
          Common.push s toks
        | Parse_info.Ab -> ()
        | Parse_info.ExpandedTok _ -> 
          failwith "unparse_php: should not have ExpandedTok"
      );
    }
    in
    (V.mk_visitor hooks) any;

    let rec aux xs =
      match xs with
      | [] -> ()
      | [x] -> pp x
      | x::y::xs ->
        (match x =~ ".*[a-zA-Z_0-9]$", y =~ "^[a-zA-Z_0-9]", x with
        (* e.g. when have "$x" and  "instanceof", or when have
         * "<div>" and "a=", we need to add a space 
         *)
        | true, true, _ -> pp x; pp " "
        | _, _, (";" | "{" | "}") -> pp x; pp "\n"
        | _, _, _ -> pp x
        );
        aux (y::xs)
    in
    aux (List.rev !toks)
  )
    
(* convenient shortcut *)
let string_of_expr x = string_of_any (Expr x)

(*****************************************************************************)
(* Transformation-aware unparser (using the tokens) *)
(*****************************************************************************)
(* less:
 * - use a AddedBefore where should use a AddedAfter on bar.spatch
 * - put some newline in the Added of a spatch, add_statement.spatch
 *   too many places where we do ugly hack around newline
 *)

let string_of_program_with_comments_using_transfo (_ast, toks) =
  let toks' = toks |> List.map (fun tok ->
      TH.token_kind_of_tok tok, TH.info_of_tok tok ) in
  Lib_unparser.string_of_toks_using_transfo toks'
(*e: unparse_php.ml *)
