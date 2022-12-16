
(* Yoann Padioleau
 *
 * Copyright (C) 2020 r2c
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
open Parse_js
module PI = Parse_info
module TH = Token_helpers_js
module Flag = Flag_parsing

let error_msg_tok tok =
  Parse_info.error_message_info (TH.info_of_tok tok)

let parse_program filename =
  let toks = tokens filename in
  (* need need parsing hacks fix I think *)
  let tr, lexer, lexbuf_fake =
    PI.mk_lexer_for_yacc toks TH.is_comment in

  try
    Parser_js.json lexer lexbuf_fake
  with Parsing.Parse_error ->
    let cur = tr.PI.current in
    if !Flag.show_parsing_error
    then pr2 ("parse error \n = " ^ error_msg_tok cur);
    raise (PI.Parsing_error (TH.info_of_tok cur))

let any_of_string str =
  Common.save_excursion Flag_parsing.sgrep_mode true (fun () ->
    Common2.with_tmp_file ~str ~ext:"json" (fun file ->
      let toks = tokens file in
      let _tr, lexer, lexbuf_fake = PI.mk_lexer_for_yacc toks TH.is_comment in
      (* bugfix: currently Parser_js.sgrep_spatch_pattern does not
       * recognize full expression as a start, it uses
       * assignment_expr_no_stmt because of possible ambiguities
       * when seeing { } which can be a block or an object,
       * so let's call directly Parser_js.json_pattern
      *)
      match Parser_js.json_pattern lexer lexbuf_fake with
      | Ast_js.Expr e ->
          Ast_json.E e
      | Ast_js.Partial (Ast_js.PartialSingleField (v1, v2, v3)) ->
          Ast_json.PartialSingleField (v1, v2, v3)
      | _ -> failwith "not a json expression"
    ))
