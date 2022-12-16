(* Yoann Padioleau
 *
 * Copyright (C) 2019 Yoann Padioleau
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
module TH   = Token_helpers_skip
module PI = Parse_info

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Lots of copy paste with my other parsers (e.g. C++, PHP, sql) but
 * copy paste is sometimes ok.
*)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type program_and_tokens =
  Ast_skip.program option * Parser_skip.token list

(*****************************************************************************)
(* Error diagnostic  *)
(*****************************************************************************)
let error_msg_tok tok =
  Parse_info.error_message_info (TH.info_of_tok tok)

(*****************************************************************************)
(* Lexing only *)
(*****************************************************************************)

let tokens2 file =
  let token = Lexer_skip.token in
  Parse_info.tokenize_all_and_adjust_pos
    file token TH.visitor_info_of_tok TH.is_eof

let tokens a =
  Common.profile_code "Parse_skip.tokens" (fun () -> tokens2 a)

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)
let parse2 filename =

  let stat = Parse_info.default_stat filename in
  let toks = tokens filename in

  let tr, lexer, lexbuf_fake =
    Parse_info.mk_lexer_for_yacc toks TH.is_comment in

  try
    (* -------------------------------------------------- *)
    (* Call parser *)
    (* -------------------------------------------------- *)
    let xs =
      Common.profile_code "Parser_skip.main" (fun () ->
        Parser_skip.main    lexer lexbuf_fake
      )
    in
    (Some xs, toks), stat

  with Parsing.Parse_error   ->

    let cur = tr.PI.current in
    if not !Flag.error_recovery
    then raise (PI.Parsing_error (TH.info_of_tok cur));

    if !Flag.show_parsing_error
    then begin
      pr2 ("parse error \n = " ^ error_msg_tok cur);
      let filelines = Common2.cat_array filename in
      let checkpoint2 = Common.cat filename |> List.length in
      let line_error = PI.line_of_info (TH.info_of_tok cur) in
      Parse_info.print_bad line_error (0, checkpoint2) filelines;
    end;
    stat.PI.error_line_count <- stat.PI.total_line_count;
    (None, toks), stat

let parse a =
  Common.profile_code "Parse_skip.parse" (fun () -> parse2 a)

let parse_program file =
  let ((astopt, _toks), _stat) = parse file in
  Common2.some astopt
