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
open Common

module Flag = Flag_parsing
module PI = Parse_info

module TH = Token_helpers_java

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Lots of copy paste with my other parsers (e.g. C++, PHP, sql) but
 * copy paste is sometimes ok.
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* the token list contains also the comment-tokens *)
type program_and_tokens = Ast_java.program option * Parser_java.token list

(*****************************************************************************)
(* Error diagnostic *)
(*****************************************************************************)
let error_msg_tok tok =
  Parse_info.error_message_info (TH.info_of_tok tok)

(*****************************************************************************)
(* Lexing only *)
(*****************************************************************************)

let tokens2 file =
  let token = Lexer_java.token in
  Parse_info.tokenize_all_and_adjust_pos ~unicode_hack:true
    file token TH.visitor_info_of_tok TH.is_eof

let tokens a =
  Common.profile_code "Java parsing.tokens" (fun () -> tokens2 a)

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)
let parse2 filename =

  let stat = Parse_info.default_stat filename in
  let filelines = Common2.cat_array filename in

  let toks = tokens filename in
  let toks = Parsing_hacks_java.fix_tokens toks in

  let tr, lexer, lexbuf_fake = 
    Parse_info.mk_lexer_for_yacc toks TH.is_comment in

  let checkpoint = TH.line_of_tok tr.PI.current in

  let elems =
    try (
      (* -------------------------------------------------- *)
      (* Call parser *)
      (* -------------------------------------------------- *)
      Left
        (Common.profile_code "Parser_java.main" (fun () ->
          Parser_java.goal lexer lexbuf_fake
        ))
    ) with Parsing.Parse_error ->

      let line_error = TH.line_of_tok tr.PI.current in

      let _passed_before_error = tr.PI.passed in
      let current = tr.PI.current in

      (* no error recovery, the whole file is discarded *)
      tr.PI.passed <- List.rev toks;

      let info_of_bads = Common2.map_eff_rev TH.info_of_tok tr.PI.passed in

      Right (info_of_bads, line_error, current)
  in

  match elems with
  | Left xs ->
      stat.PI.correct <- (Common.cat filename |> List.length);
      (Some xs, toks), stat

  | Right (_info_of_bads, line_error, cur) ->
      if not !Flag.error_recovery
      then raise (PI.Parsing_error (TH.info_of_tok cur));

      if !Flag.show_parsing_error
      then pr2 ("parse error \n = " ^ error_msg_tok cur);
      let checkpoint2 = Common.cat filename |> List.length in

      if !Flag.show_parsing_error
      then Parse_info.print_bad line_error (checkpoint, checkpoint2) filelines;
      stat.PI.bad     <- Common.cat filename |> List.length;
      (None, toks), stat

let parse a =
  Common.profile_code "Parse_java.parse" (fun () -> parse2 a)

let parse_program file =
  let ((ast, _toks), _stat) = parse file in
  Common2.some ast

let parse_string (w : string)
    : (Ast_java.program option * Parser_java.token list) * Parse_info.parsing_stat =
  Common2.with_tmp_file ~str:w ~ext:"java" parse

(*****************************************************************************)
(* Sub parsers *)
(*****************************************************************************)

(* for sgrep/spatch *)
let any_of_string s = 
  Common2.with_tmp_file ~str:s ~ext:"java" (fun file ->
    let toks = tokens file in
    let toks = Parsing_hacks_java.fix_tokens toks in
    let _tr, lexer, lexbuf_fake = PI.mk_lexer_for_yacc toks TH.is_comment in
    Parser_java.sgrep_spatch_pattern lexer lexbuf_fake
  )
