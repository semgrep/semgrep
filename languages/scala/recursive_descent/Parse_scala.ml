(* Yoann Padioleau
 *
 * Copyright (C) 2021 Semgrep Inc.
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
module TH = Token_helpers_scala
module PS = Parsing_stat
module Log = Log_lib_parsing.Log

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Driver for the Scala parser
*)

(*****************************************************************************)
(* Error diagnostic  *)
(*****************************************************************************)
let _error_msg_tok tok = Parsing_helpers.error_message_info (TH.info_of_tok tok)

(*****************************************************************************)
(* Lexing only *)
(*****************************************************************************)

let tokens input_source =
  Lexer_scala.reset ();
  let token lexbuf =
    let tok =
      match Lexer_scala.current_mode () with
      | Lexer_scala.ST_IN_CODE -> Lexer_scala.token lexbuf
      | Lexer_scala.ST_IN_INTERPOLATED_DOUBLE ->
          Lexer_scala.in_interpolated_double lexbuf
      | Lexer_scala.ST_IN_INTERPOLATED_TRIPLE ->
          Lexer_scala.in_interpolated_triple lexbuf
    in
    tok
  in
  (* set to false to parse correctly arrows *)
  Parsing_helpers.tokenize_all_and_adjust_pos input_source token
    TH.visitor_info_of_tok TH.is_eof
[@@profiling]

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)
let parse (filename : Fpath.t) =
  let stat = Parsing_stat.default_stat !!filename in
  let toks = tokens (Parsing_helpers.file !!filename) in

  (*
  let tr, lexer, lexbuf_fake =
    Parse_info.mk_lexer_for_yacc toks TH.is_comment in
  *)
  try
    (* -------------------------------------------------- *)
    (* Call parser *)
    (* -------------------------------------------------- *)
    (*
    let xs =
      Parser_scala.compilationUnit    lexer lexbuf_fake
    in
    *)
    let xs = Parser_scala_recursive_descent.parse toks in
    { Parsing_result.ast = xs; tokens = toks; stat }
  with
  | Parsing_error.Syntax_error cur
    when !Flag.error_recovery && not !Flag.debug_parser ->
      if !Flag.show_parsing_error then (
        Log.err (fun m ->
            m "parse error \n = %s" (Parsing_helpers.error_message_info cur));
        let filelines = UFile.cat_array filename in
        let checkpoint2 = UFile.cat filename |> List.length in
        let line_error = Tok.line_of_tok cur in
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
(* for semgrep *)
let any_of_string s =
  Common.save_excursion Flag_parsing.sgrep_mode true (fun () ->
      let toks = tokens (Parsing_helpers.Str s) in
      (* -------------------------------------------------- *)
      (* Call parser *)
      (* -------------------------------------------------- *)
      Parser_scala_recursive_descent.semgrep_pattern toks)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let find_source_files_of_dir_or_files xs =
  UFile.files_of_dirs_or_files_no_vcs_nofilter xs
  |> List.filter (fun filename ->
         match File_type.file_type_of_file filename with
         | File_type.PL File_type.Scala -> true
         | _ -> false)
  |> List_.sort
