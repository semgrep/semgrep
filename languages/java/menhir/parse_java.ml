(* Yoann Padioleau
 *
 * Copyright (C) 2010 Facebook
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
module Flag = Flag_parsing
module PS = Parsing_stat
module TH = Token_helpers_java
module Log = Log_lib_parsing.Log

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Lots of copy paste with my other parsers (e.g. C++, PHP, sql) but
 * copy paste is sometimes ok.
 *)

(*****************************************************************************)
(* Error diagnostic *)
(*****************************************************************************)
let error_msg_tok tok = Parsing_helpers.error_message_info (TH.info_of_tok tok)

(*****************************************************************************)
(* Lexing only *)
(*****************************************************************************)

let tokens input_source =
  let token = Lexer_java.token in
  Parsing_helpers.tokenize_all_and_adjust_pos input_source token
    TH.visitor_info_of_tok TH.is_eof
[@@profiling]

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)
let parse filename =
  let stat = Parsing_stat.default_stat !!filename in
  let filelines = UFile.cat_array filename in

  let toks = tokens (Parsing_helpers.file !!filename) in
  let toks = Parsing_hacks_java.fix_tokens toks in

  let tr, lexer, lexbuf_fake =
    Parsing_helpers.mk_lexer_for_yacc toks TH.is_comment
  in

  let checkpoint = TH.line_of_tok tr.Parsing_helpers.current in

  let elems =
    try
      (* -------------------------------------------------- *)
      (* Call parser *)
      (* -------------------------------------------------- *)
      Either.Left
        (Profiling.profile_code "Parser_java.main" (fun () ->
             Parser_java.goal lexer lexbuf_fake))
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
  | Either.Right (_info_of_bads, line_error, cur) ->
      if not !Flag.error_recovery then
        raise (Parsing_error.Syntax_error (TH.info_of_tok cur));

      if !Flag.show_parsing_error then (
        Log.err (fun m -> m "parse error \n = %s" (error_msg_tok cur));
        let checkpoint2 = UFile.cat filename |> List.length in
        Log.err (fun m ->
            m "%s"
              (Parsing_helpers.show_parse_error_line line_error
                 (checkpoint, checkpoint2) filelines)));
      stat.PS.error_line_count <- stat.PS.total_line_count;
      { Parsing_result.ast = []; tokens = toks; stat }
[@@profiling]

let parse_program file =
  let res = parse file in
  res.Parsing_result.ast

let parse_string (caps : < Cap.tmp >) (w : string) :
    (Ast_java.program, Parser_java.token) Parsing_result.t =
  CapTmp.with_temp_file caps#tmp ~contents:w ~suffix:".java" parse

(*****************************************************************************)
(* Sub parsers *)
(*****************************************************************************)

(* for sgrep/spatch *)
let any_of_string s =
  Common.save_excursion Flag_parsing.sgrep_mode true (fun () ->
      let toks = tokens (Parsing_helpers.Str s) in
      let toks = Parsing_hacks_java.fix_tokens toks in
      let _tr, lexer, lexbuf_fake =
        Parsing_helpers.mk_lexer_for_yacc toks TH.is_comment
      in
      Parser_java.semgrep_pattern lexer lexbuf_fake)
