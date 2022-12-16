(* Yoann Padioleau
 *
 * Copyright (C) 2011 Facebook
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

module Ast = Ast_css
module Flag = Flag_parsing
module T = Parser_css
module TH   = Token_helpers_css
module PI = Parse_info

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type program2 = Ast.stylesheet * T.token list

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
let error_msg_tok tok =
  Parse_info.error_message_info (TH.info_of_tok tok)

(*****************************************************************************)
(* Lexing only *)
(*****************************************************************************)

let tokens2 file =
  let token lexbuf = Lexer_css.token lexbuf in
  Parse_info.tokenize_all_and_adjust_pos
    file token TH.visitor_info_of_tok TH.is_eof

let tokens a =
  Common.profile_code "Parse_css.tokens" (fun () -> tokens2 a)

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let parse2 filename =
  let toks = tokens filename in
  let tr, lexer, lexbuf_fake =
    Parse_info.mk_lexer_for_yacc toks TH.is_comment in

  (* -------------------------------------------------- *)
  (* Call parser *)
  (* -------------------------------------------------- *)
  try
    (Common.profile_code "Parser_css.main" (fun () ->
       Parser_css.stylesheet lexer lexbuf_fake, toks
     ))
  with Parsing.Parse_error ->
    let current = tr.PI.current in

    if not !Flag.error_recovery
    then raise (Parse_info.Parsing_error (TH.info_of_tok current));

    if !Flag.show_parsing_error
    then pr2 ("parse error \n = " ^ error_msg_tok current);
    [], toks

let parse a =
  Common.profile_code "Parse_css.parse" (fun () -> parse2 a)
