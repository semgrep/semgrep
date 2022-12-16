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

module Flag = Flag_parsing
module TH   = Token_helpers_ml
module PI = Parse_info

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*****************************************************************************)
(* Error diagnostic  *)
(*****************************************************************************)
let error_msg_tok tok =
  Parse_info.error_message_info (TH.info_of_tok tok)

(*****************************************************************************)
(* Lexing only *)
(*****************************************************************************)
let tokens file =
  let token = Lexer_ml.token in
  Parse_info.tokenize_all_and_adjust_pos
    file token TH.visitor_info_of_tok TH.is_eof
[@@profiling]

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
      Common.profile_code "Parser_ml.main" (fun () ->
        if filename =~ ".*\\.mli"
        then Parser_ml.interface      lexer lexbuf_fake
        else Parser_ml.implementation lexer lexbuf_fake
      )
    in
    {PI. ast = xs; tokens = toks; stat }

  with Parsing.Parse_error   ->

    let cur = tr.PI.current in
    if not !Flag.error_recovery
    then raise (PI.Parsing_error (TH.info_of_tok cur));

    if !Flag.show_parsing_error
    then begin
      pr2 ("parse error \n = " ^ error_msg_tok cur);
      let filelines = Common2.cat_array filename in
      let checkpoint2 = Common.cat filename |> List.length in
      let line_error = TH.line_of_tok cur in
      Parse_info.print_bad line_error (0, checkpoint2) filelines;
    end;

    stat.PI.error_line_count <- stat.PI.total_line_count;
    {PI. ast = []; tokens = toks; stat }

let parse a =
  Common.profile_code "Parse_ml.parse" (fun () -> parse2 a)

let parse_program file =
  let res = parse file in
  res.PI.ast

(*****************************************************************************)
(* Sub parsers *)
(*****************************************************************************)
let type_of_string s =
  let lexbuf = Lexing.from_string s in
  let rec lexer lexbuf =
    let res = Lexer_ml.token lexbuf in
    if TH.is_comment res
    then lexer lexbuf
    else res
  in
  let ty = Parser_ml.type_for_lsp lexer lexbuf in
  ty

(* for sgrep/spatch *)
let any_of_string  s =
  Common.save_excursion Flag_parsing.sgrep_mode true (fun () ->
    Common2.with_tmp_file ~str:s ~ext:"ml" (fun file ->
      let toks = tokens file in
      let _tr, lexer, lexbuf_fake = PI.mk_lexer_for_yacc toks TH.is_comment in
      (* -------------------------------------------------- *)
      (* Call parser *)
      (* -------------------------------------------------- *)
      Parser_ml.semgrep_pattern lexer lexbuf_fake
    ))
