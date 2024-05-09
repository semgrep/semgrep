(* Yoann Padioleau
 *
 * Copyright (C) 2010 Facebook
 * Copyright (C) 2019 Semgrep Inc.
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
open Fpath_.Operators
module Flag = Flag_parsing
module TH = Token_helpers_go
module Lexer = Lexer_go
module Log = Log_lib_parsing.Log

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Parser for Go(lang)
 *
 * TODO:
 *  - error recovery: could skip until next toplevel decl (func/type/const/var)
 *
 *)

(*****************************************************************************)
(* Error diagnostic  *)
(*****************************************************************************)
let error_msg_tok tok = Parsing_helpers.error_message_info (TH.info_of_tok tok)

(*****************************************************************************)
(* Lexing only *)
(*****************************************************************************)

let tokens input_source =
  let token lexbuf = Lexer.token lexbuf in
  Parsing_helpers.tokenize_all_and_adjust_pos input_source token
    TH.visitor_info_of_tok TH.is_eof
[@@profiling]

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)
let parse filename =
  (* this can throw Parse_info.Lexical_error *)
  let toks_orig = tokens (Parsing_helpers.file !!filename) in
  let toks = List_.exclude TH.is_comment_or_space toks_orig in
  (* insert implicit SEMICOLON and replace some LBRACE with LBODY *)
  let toks = Parsing_hacks_go.fix_tokens toks in
  let tr, lexer, lexbuf_fake =
    Parsing_helpers.mk_lexer_for_yacc toks TH.is_irrelevant
  in

  try
    (* -------------------------------------------------- *)
    (* Call parser *)
    (* -------------------------------------------------- *)
    let xs =
      Profiling.profile_code "Parser_go.file" (fun () ->
          Parser_go.file lexer lexbuf_fake)
    in
    {
      Parsing_result.ast = xs;
      tokens = toks_orig;
      stat = Parsing_stat.correct_stat !!filename;
    }
  with
  | Parsing.Parse_error ->
      let cur = tr.Parsing_helpers.current in
      if not !Flag.error_recovery then
        raise (Parsing_error.Syntax_error (TH.info_of_tok cur));

      if !Flag.show_parsing_error then (
        Log.err (fun m -> m "parse error \n = %s" (error_msg_tok cur));
        let filelines = UFile.cat_array filename in
        let checkpoint2 = UFile.cat filename |> List.length in
        let line_error = Tok.line_of_tok (TH.info_of_tok cur) in
        Log.err (fun m ->
            m "%s"
              (Parsing_helpers.show_parse_error_line line_error (0, checkpoint2)
                 filelines)));
      {
        Parsing_result.ast = [];
        tokens = toks_orig;
        stat = Parsing_stat.bad_stat !!filename;
      }
[@@profiling]

let parse_program file =
  let res = parse file in
  res.Parsing_result.ast

(*****************************************************************************)
(* Sub parsers *)
(*****************************************************************************)

let program_of_string (caps : < Cap.tmp >) (s : string) : Ast_go.program =
  CapTmp.with_temp_file caps#tmp ~contents:s ~suffix:".go" parse_program

(* for sgrep/spatch *)
let any_of_string s =
  Common.save_excursion Flag_parsing.sgrep_mode true (fun () ->
      let toks_orig = tokens (Parsing_helpers.Str s) in
      let toks = List_.exclude TH.is_comment_or_space toks_orig in
      (* insert implicit SEMICOLON and replace some LBRACE with LBODY *)
      let toks = Parsing_hacks_go.fix_tokens toks in
      let tr, lexer, lexbuf_fake =
        Parsing_helpers.mk_lexer_for_yacc toks TH.is_irrelevant
      in
      (* -------------------------------------------------- *)
      (* Call parser *)
      (* -------------------------------------------------- *)
      try Parser_go.sgrep_spatch_pattern lexer lexbuf_fake with
      | Parsing.Parse_error ->
          let cur = tr.Parsing_helpers.current in
          Log.err (fun m -> m "parse error \n = %s" (error_msg_tok cur));
          raise (Parsing_error.Syntax_error (TH.info_of_tok cur)))
