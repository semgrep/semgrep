(* Yoann Padioleau
 *
 * Copyright (C) 2020 Semgrep Inc.
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
open Parse_js
open Fpath_.Operators
module TH = Token_helpers_js
module Flag = Flag_parsing
module Log = Log_lib_parsing.Log

let error_msg_tok tok = Parsing_helpers.error_message_info (TH.info_of_tok tok)

let parse_program (filename : Fpath.t) =
  let toks = tokens (Parsing_helpers.file !!filename) in
  (* need need parsing hacks fix I think *)
  let tr, lexer, lexbuf_fake =
    Parsing_helpers.mk_lexer_for_yacc toks TH.is_comment
  in

  try Parser_js.json lexer lexbuf_fake with
  | Parsing.Parse_error ->
      let cur = tr.Parsing_helpers.current in
      if !Flag.show_parsing_error then
        Log.err (fun m -> m "parse error \n = %s" (error_msg_tok cur));
      raise (Parsing_error.Syntax_error (TH.info_of_tok cur))

let any_of_string str =
  Common.save_excursion Flag_parsing.sgrep_mode true (fun () ->
      let toks = tokens (Parsing_helpers.Str str) in
      let _tr, lexer, lexbuf_fake =
        Parsing_helpers.mk_lexer_for_yacc toks TH.is_comment
      in
      (* bugfix: currently Parser_js.sgrep_spatch_pattern does not
       * recognize full expression as a start, it uses
       * assignment_expr_no_stmt because of possible ambiguities
       * when seeing { } which can be a block or an object,
       * so let's call directly Parser_js.json_pattern
       *)
      match Parser_js.json_pattern lexer lexbuf_fake with
      | Ast_js.Expr e -> Ast_json.E e
      | Ast_js.Partial (Ast_js.PartialSingleField (v1, v2, v3)) ->
          Ast_json.PartialSingleField (v1, v2, v3)
      | _ -> failwith "not a json expression")
