(* Yoann Padioleau
 *
 * Copyright (C) 2010 Facebook
 * Copyright (C) 2019 Semgrep Inc.
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
module TH = Token_helpers_python
module PS = Parsing_stat
module Lexer = Lexer_python
module T = Parser_python
module Log = Log_lib_parsing.Log

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type parsing_mode =
  | Python2
  | Python3
  (* will start with Python3 and fallback to Python2 in case of an error *)
  | Python

(*****************************************************************************)
(* Error diagnostic  *)
(*****************************************************************************)
let error_msg_tok tok = Parsing_helpers.error_message_info (TH.info_of_tok tok)

(*****************************************************************************)
(* Lexing only *)
(*****************************************************************************)

let tokens parsing_mode input_source =
  let state = Lexer.create () in
  let python2 = parsing_mode =*= Python2 in
  let token lexbuf =
    try
      match Lexer.top_mode state with
      | Lexer.STATE_TOKEN -> Lexer.token python2 state lexbuf
      | Lexer.STATE_OFFSET -> failwith "impossibe STATE_OFFSET in python lexer"
      | Lexer.STATE_UNDERSCORE_TOKEN ->
          let tok = Lexer._token python2 state lexbuf in
          (match (tok, Lexer.top_mode state) with
          | T.TCommentSpace _, _ -> ()
          | T.FSTRING_START _, _ -> ()
          | _, Lexer.STATE_UNDERSCORE_TOKEN ->
              (* Note that _token() may have changed the top state.
               * For example, after having lexed 'f"{foo '", which puts us in a state
               * ST_UNDERSCORE_TOKEN with a full stack of [ST_UNDERSCORE_TOKEN;
               * ST_IN_F_STRING_DOUBLE; ST_UNDERSCORE_TOKEN], encountering a '}' will
               * pop the stack and leave ST_IN_FSTRING_DOUBLE at the top, which we
               * don't want to replace with ST_TOKEN. This is why we should switch
               * back to ST_TOKEN only when the current state is
               * STATE_UNDERSCORE_TOKEN. *)
              Lexer.set_mode state Lexer.STATE_TOKEN
          | _ -> ());
          tok
      | Lexer.STATE_IN_FSTRING_SINGLE pre ->
          Lexer.fstring_single state pre lexbuf
      | Lexer.STATE_IN_FSTRING_DOUBLE pre ->
          Lexer.fstring_double state pre lexbuf
      | Lexer.STATE_IN_FSTRING_TRIPLE_SINGLE pre ->
          Lexer.fstring_triple_single state pre lexbuf
      | Lexer.STATE_IN_FSTRING_TRIPLE_DOUBLE pre ->
          Lexer.fstring_triple_double state pre lexbuf
    with
    (* This can happen with "empty_mode stack" in Lexer.top_mode(), or
     * even "hd" in Lexer.pop_mode().
     * It's better to generate proper parsing error exn.
     *)
    | Failure s ->
        Parsing_error.lexical_error s lexbuf;
        T.EOF (Tok.tok_of_lexbuf lexbuf)
  in
  Parsing_helpers.tokenize_all_and_adjust_pos input_source token
    TH.visitor_info_of_tok TH.is_eof
[@@profiling]

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let rec parse ?(parsing_mode = Python) (filename : Fpath.t) =
  let stat = Parsing_stat.default_stat !!filename in

  (* this can throw Parse_info.Lexical_error *)
  let toks = tokens parsing_mode (Parsing_helpers.file !!filename) in
  let toks = Parsing_hacks_python.fix_tokens toks in

  let tr, lexer, lexbuf_fake =
    Parsing_helpers.mk_lexer_for_yacc toks TH.is_comment
  in

  try
    (* -------------------------------------------------- *)
    (* Call parser *)
    (* -------------------------------------------------- *)
    let xs =
      Profiling.profile_code "Parser_python.main" (fun () ->
          Parser_python.main lexer lexbuf_fake)
    in
    { Parsing_result.ast = xs; tokens = toks; stat }
  with
  | Parsing.Parse_error ->
      (* There are still lots of python2 code out there, it would be sad to
       * not parse them just because they use the print and exec special
       * statements, which are not compatible with python3, hence
       * the special error recovery trick below.
       * For the rest Python2 is mostly compatible with Python3.
       *
       * Note that we do the error recovery only when we think a print
       * or exec identifiers was involved. Otherwise every parse errors
       * would trigger a parsing with a python2 mode, which change the
       * significance of the print and exec identifiers, which may give
       * strange error messages for python3 code.
       *)
      if
        parsing_mode =*= Python
        && tr.Parsing_helpers.passed |> List_.take_safe 10
           |> List.exists (function
                | T.NAME (("print" | "exec"), _)
                | T.ASYNC _
                | T.AWAIT _
                | T.NONLOCAL _
                | T.TRUE _
                | T.FALSE _ ->
                    true
                | _ -> false)
      then
        (* note that we cant use tokens as the tokens are actually different
         * in Python2 mode, but we could optimize things a bit and just
         * transform those tokens here *)
        parse ~parsing_mode:Python2 filename
      else
        let cur = tr.Parsing_helpers.current in
        if not !Flag.error_recovery then
          raise (Parsing_error.Syntax_error (TH.info_of_tok cur));

        if !Flag.show_parsing_error then (
          Log.err (fun m -> m "parse error \n = %s" (error_msg_tok cur));
          let filelines = UFile.cat_array filename in
          let checkpoint2 = UFile.cat filename |> List.length in
          let line_error = Tok.line_of_tok (TH.info_of_tok cur) in
          Log.err (fun m ->
              m "%s"
                (Parsing_helpers.show_parse_error_line line_error
                   (0, checkpoint2) filelines)));
        stat.PS.error_line_count <- stat.PS.total_line_count;
        { Parsing_result.ast = []; tokens = toks; stat }
[@@profiling]

let parse_program ?parsing_mode file =
  let res = parse ?parsing_mode file in
  res.Parsing_result.ast

(*****************************************************************************)
(* Sub parsers *)
(*****************************************************************************)

let program_of_string (caps : < Cap.tmp >) (s : string) : AST_python.program =
  CapTmp.with_temp_file caps#tmp ~contents:s ~suffix:".py" parse_program

let type_of_string ?(parsing_mode = Python) s =
  let lexbuf = Lexing.from_string s in
  let is_python2 = parsing_mode =*= Python2 in
  let state = Lexer.create () in
  let rec lexer lexbuf =
    let res = Lexer_python.token is_python2 state lexbuf in
    if TH.is_comment res then lexer lexbuf else res
  in
  let ty = Parser_python.type_for_lsif lexer lexbuf in
  ty

(* for sgrep/spatch *)
let any_of_string ?(parsing_mode = Python) s =
  Common.save_excursion Flag_parsing.sgrep_mode true (fun () ->
      let toks = tokens parsing_mode (Parsing_helpers.Str s) in
      let toks = Parsing_hacks_python.fix_tokens toks in
      let _tr, lexer, lexbuf_fake =
        Parsing_helpers.mk_lexer_for_yacc toks TH.is_comment
      in
      (* -------------------------------------------------- *)
      (* Call parser *)
      (* -------------------------------------------------- *)
      Parser_python.sgrep_spatch_pattern lexer lexbuf_fake)

(*****************************************************************************)
(* Fuzzy parsing *)
(*****************************************************************************)

(*
let parse_fuzzy file =
  let toks = tokens (Parsing_helpers.File file) in
  let trees = Parse_fuzzy.mk_trees { Parse_fuzzy.
     tokf = TH.info_of_tok;
     kind = TH.token_kind_of_tok;
  } toks
  in
  trees, toks
*)
