(* Yoann Padioleau
 *
 * Copyright (C) 2009-2011 Facebook
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
open Fpath_.Operators
module Ast = Cst_php
module Flag = Flag_parsing
module Flag_php = Flag_parsing_php
module TH = Token_helpers_php
module PS = Parsing_stat
module Log = Log_lib_parsing.Log

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* A PHP parser.
 *
 * related work:
 *  - miamide, also in ocaml, but didn't support all of PHP
 *  - https://github.com/sfindeisen/phphard, also written in ocaml, but
 *    seems pretty rudimentary
 *)

(*****************************************************************************)
(* Error diagnostic  *)
(*****************************************************************************)
let error_msg_tok tok = Parsing_helpers.error_message_info (TH.info_of_tok tok)

(*****************************************************************************)
(* Lexing only *)
(*****************************************************************************)
let tokens ?(init_state = Lexer_php.INITIAL) input_source =
  Lexer_php.reset ();
  Lexer_php._mode_stack := [ init_state ];

  let token lexbuf =
    let tok =
      (* for yyless emulation *)
      match !Lexer_php._pending_tokens with
      | x :: xs ->
          Lexer_php._pending_tokens := xs;
          x
      | [] -> (
          match Lexer_php.current_mode () with
          | Lexer_php.INITIAL -> Lexer_php.initial lexbuf
          | Lexer_php.ST_IN_SCRIPTING -> Lexer_php.st_in_scripting lexbuf
          | Lexer_php.ST_IN_SCRIPTING2 -> Lexer_php.st_in_scripting lexbuf
          | Lexer_php.ST_DOUBLE_QUOTES -> Lexer_php.st_double_quotes lexbuf
          | Lexer_php.ST_BACKQUOTE -> Lexer_php.st_backquote lexbuf
          | Lexer_php.ST_LOOKING_FOR_PROPERTY ->
              Lexer_php.st_looking_for_property lexbuf
          | Lexer_php.ST_LOOKING_FOR_VARNAME ->
              Lexer_php.st_looking_for_varname lexbuf
          | Lexer_php.ST_VAR_OFFSET -> Lexer_php.st_var_offset lexbuf
          | Lexer_php.ST_START_HEREDOC s -> Lexer_php.st_start_heredoc s lexbuf
          | Lexer_php.ST_START_NOWDOC s -> Lexer_php.st_start_nowdoc s lexbuf)
    in
    if not (TH.is_comment tok) then
      Lexer_php._last_non_whitespace_like_token := Some tok;
    tok
  in
  Parsing_helpers.tokenize_all_and_adjust_pos input_source token
    TH.visitor_info_of_tok TH.is_eof
[@@profiling]

let is_comment v =
  TH.is_comment v
  ||
  (* TODO a little bit specific to FB ? *)
  match v with
  | Parser_php.T_OPEN_TAG _ -> true
  | Parser_php.T_CLOSE_TAG _ -> true
  | _ -> false

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let parse filename =
  let stat = Parsing_stat.default_stat !!filename in
  let filelines = UFile.cat_array filename in

  let toks = tokens (Parsing_helpers.file !!filename) in
  let toks = Parsing_hacks_php.fix_tokens toks in

  let tr, lexer, lexbuf_fake =
    Parsing_helpers.mk_lexer_for_yacc toks is_comment
  in

  let checkpoint = TH.line_of_tok tr.Parsing_helpers.current in

  let elems =
    try
      (* -------------------------------------------------- *)
      (* Call parser *)
      (* -------------------------------------------------- *)
      Either.Left
        (Profiling.profile_code "Parser_php.main" (fun () ->
             Parser_php.main lexer lexbuf_fake))
    with
    | Parsing.Parse_error ->
        let line_error = TH.line_of_tok tr.Parsing_helpers.current in

        let _passed_before_error = tr.Parsing_helpers.passed in
        let current = tr.Parsing_helpers.current in

        (* no error recovery, the whole file is discarded *)
        tr.Parsing_helpers.passed <- List.rev toks;

        let info_of_bads =
          Common2.map_eff_rev TH.info_of_tok tr.Parsing_helpers.passed
        in

        Right (info_of_bads, line_error, current)
  in

  match elems with
  | Either.Left xs -> { Parsing_result.ast = xs; tokens = toks; stat }
  | Either.Right (info_of_bads, line_error, cur) ->
      if not !Flag.error_recovery then
        raise (Parsing_error.Syntax_error (TH.info_of_tok cur));

      if !Flag.show_parsing_error then (
        Log.err (fun m -> m "parse error\n = %s" (error_msg_tok cur));
        let checkpoint2 = UFile.cat filename |> List.length in
        Log.err (fun m ->
            m "%s"
              (Parsing_helpers.show_parse_error_line line_error
                 (checkpoint, checkpoint2) filelines)));
      (* TODO: just count the skipped lines; Use Hashtbl.length strategy *)
      stat.PS.error_line_count <- stat.PS.total_line_count;

      let info_item = List.rev tr.Parsing_helpers.passed in
      {
        Parsing_result.ast = [ Ast.NotParsedCorrectly info_of_bads ];
        tokens = info_item;
        stat;
      }
[@@profiling]

let parse_program file =
  let res = parse file in
  res.Parsing_result.ast

(*****************************************************************************)
(* Sub parsers *)
(*****************************************************************************)

let any_of_string s =
  Common.save_excursion Flag_parsing.sgrep_mode true (fun () ->
      let toks =
        tokens ~init_state:Lexer_php.ST_IN_SCRIPTING (Parsing_helpers.Str s)
      in
      let toks = Parsing_hacks_php.fix_tokens toks in
      let _tr, lexer, lexbuf_fake =
        Parsing_helpers.mk_lexer_for_yacc toks is_comment
      in
      Parser_php.semgrep_pattern lexer lexbuf_fake)

let program_of_string s =
  match any_of_string s with
  | Cst_php.Program x -> x
  | _else_ -> failwith ("not a program: " ^ s)
