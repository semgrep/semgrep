(* Yoann Padioleau
 *
 * Copyright (C) 2010 Facebook
 * Copyright (C) 2019 r2c
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
module TH   = Token_helpers_go
module PI = Parse_info
module Lexer = Lexer_go

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
let error_msg_tok tok =
  Parse_info.error_message_info (TH.info_of_tok tok)

(*****************************************************************************)
(* Lexing only *)
(*****************************************************************************)

let tokens2 file =
  let token lexbuf =
    Lexer.token lexbuf
  in
  Parse_info.tokenize_all_and_adjust_pos
    file token TH.visitor_info_of_tok TH.is_eof

let tokens a =
  Common.profile_code "Parse_go.tokens" (fun () -> tokens2 a)

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)
let parse2 filename =
  (* this can throw Parse_info.Lexical_error *)
  let toks_orig = tokens filename in
  let toks = Common.exclude TH.is_comment_or_space toks_orig in
  (* insert implicit SEMICOLON and replace some LBRACE with LBODY *)
  let toks = Parsing_hacks_go.fix_tokens toks in
  let tr, lexer, lexbuf_fake =
    Parse_info.mk_lexer_for_yacc toks TH.is_irrelevant in

  try
    (* -------------------------------------------------- *)
    (* Call parser *)
    (* -------------------------------------------------- *)
    let xs =
      Common.profile_code "Parser_go.file" (fun () ->
        Parser_go.file  lexer lexbuf_fake
      )
    in
    {PI. ast = xs; tokens = toks_orig; stat = PI.correct_stat filename }

  with Parsing.Parse_error ->

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
    {PI. ast = []; tokens = toks_orig; stat = PI.bad_stat filename }

let parse a =
  Common.profile_code "Parse_go.parse" (fun () -> parse2 a)

let parse_program file =
  let res = parse file in
  res.PI.ast

(*****************************************************************************)
(* Sub parsers *)
(*****************************************************************************)

let (program_of_string: string -> Ast_go.program) = fun s ->
  Common2.with_tmp_file ~str:s ~ext:"go" (fun file ->
    parse_program file
  )

(* for sgrep/spatch *)
let any_of_string s =
  Common.save_excursion Flag_parsing.sgrep_mode true (fun () ->
    Common2.with_tmp_file ~str:s ~ext:"go" (fun file ->
      let toks_orig = tokens file in
      let toks = Common.exclude TH.is_comment_or_space toks_orig in
      (* insert implicit SEMICOLON and replace some LBRACE with LBODY *)
      let toks = Parsing_hacks_go.fix_tokens toks in
      let tr, lexer, lexbuf_fake = PI.mk_lexer_for_yacc toks TH.is_irrelevant in
      (* -------------------------------------------------- *)
      (* Call parser *)
      (* -------------------------------------------------- *)
      try
        Parser_go.sgrep_spatch_pattern lexer lexbuf_fake
      with Parsing.Parse_error ->
        let cur = tr.PI.current in
        pr2 ("parse error \n = " ^ error_msg_tok cur);
        raise (PI.Parsing_error (TH.info_of_tok cur))
    ))
