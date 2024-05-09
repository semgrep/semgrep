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
module Flag = Flag_parsing
module TH = Token_helpers_ml
module PS = Parsing_stat
module Log = Log_lib_parsing.Log

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*****************************************************************************)
(* Error diagnostic  *)
(*****************************************************************************)
let error_msg_tok tok = Parsing_helpers.error_message_info (TH.info_of_tok tok)

(*****************************************************************************)
(* Lexing only *)
(*****************************************************************************)
let tokens input_source =
  let token = Lexer_ml.token in
  Parsing_helpers.tokenize_all_and_adjust_pos input_source token
    TH.visitor_info_of_tok TH.is_eof
[@@profiling]

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)
let parse filename =
  let filename = !!filename in
  let stat = Parsing_stat.default_stat filename in
  let toks = tokens (Parsing_helpers.file filename) in

  let tr, lexer, lexbuf_fake =
    Parsing_helpers.mk_lexer_for_yacc toks TH.is_comment
  in

  try
    (* -------------------------------------------------- *)
    (* Call parser *)
    (* -------------------------------------------------- *)
    let xs =
      Profiling.profile_code "Parser_ml.main" (fun () ->
          if filename =~ ".*\\.mli" then Parser_ml.interface lexer lexbuf_fake
          else Parser_ml.implementation lexer lexbuf_fake)
    in
    { Parsing_result.ast = xs; tokens = toks; stat }
  with
  | Parsing.Parse_error ->
      let cur = tr.Parsing_helpers.current in
      if not !Flag.error_recovery then
        raise (Parsing_error.Syntax_error (TH.info_of_tok cur));

      if !Flag.show_parsing_error then (
        Log.err (fun m -> m "parse error \n = %s" (error_msg_tok cur));
        let filelines = UFile.cat_array (Fpath.v filename) in
        let checkpoint2 = UFile.Legacy.cat filename |> List.length in
        let line_error = TH.line_of_tok cur in
        Log.err (fun m ->
            m "%s"
              (Parsing_helpers.show_parse_error_line line_error (0, checkpoint2)
                 filelines)));

      stat.PS.error_line_count <- stat.PS.total_line_count;
      { Parsing_result.ast = []; tokens = toks; stat }
[@@profiling]

let parse_program file =
  let res = parse file in
  res.Parsing_result.ast

(*****************************************************************************)
(* Sub parsers *)
(*****************************************************************************)
let type_of_string s =
  let lexbuf = Lexing.from_string s in
  let rec lexer lexbuf =
    let res = Lexer_ml.token lexbuf in
    if TH.is_comment res then lexer lexbuf else res
  in
  let ty = Parser_ml.type_for_lsp lexer lexbuf in
  ty

(* for sgrep/spatch *)
let any_of_string s =
  Common.save_excursion Flag_parsing.sgrep_mode true (fun () ->
      let toks = tokens (Parsing_helpers.Str s) in
      let _tr, lexer, lexbuf_fake =
        Parsing_helpers.mk_lexer_for_yacc toks TH.is_comment
      in
      (* -------------------------------------------------- *)
      (* Call parser *)
      (* -------------------------------------------------- *)
      Parser_ml.semgrep_pattern lexer lexbuf_fake)
