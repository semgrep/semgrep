(*s: pfff/lang_python/parsing/Parse_python.ml *)
(* Yoann Padioleau
 * 
 * Copyright (C) 2010 Facebook
 * Copyright (C) 2019 r2c
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License (GPL)
 * version 2 as published by the Free Software Foundation.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * file license.txt for more details.
 *)
open Common 

module Flag = Flag_parsing
module TH   = Token_helpers_python
module PI = Parse_info
module Lexer = Lexer_python
module T = Parser_python

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)
(*s: type [[Parse_python.program_and_tokens]] *)
(* the token list contains also the comment-tokens *)
type program_and_tokens = 
  AST_python.program option * Parser_python.token list
(*e: type [[Parse_python.program_and_tokens]] *)

(*s: type [[Parse_python.parsing_mode]] *)
type parsing_mode =
  | Python2
  | Python3
  (* will start with Python3 and fallback to Python2 in case of an error *)
  | Python
(*e: type [[Parse_python.parsing_mode]] *)

(*****************************************************************************)
(* Error diagnostic  *)
(*****************************************************************************)
(*s: function [[Parse_python.error_msg_tok]] *)
let error_msg_tok tok = 
  Parse_info.error_message_info (TH.info_of_tok tok)
(*e: function [[Parse_python.error_msg_tok]] *)

(*****************************************************************************)
(* Lexing only *)
(*****************************************************************************)

(*s: function [[Parse_python.tokens2]] *)
let tokens2 parsing_mode file = 
  let state = Lexer.create () in
  let python2 = parsing_mode = Python2 in
  let token lexbuf = 
    match Lexer.top_mode state with
    | Lexer.STATE_TOKEN -> 
      Lexer.token python2 state lexbuf
    | Lexer.STATE_OFFSET -> 
        failwith "impossibe STATE_OFFSET in python lexer"
    | Lexer.STATE_UNDERSCORE_TOKEN -> 
      let tok = Lexer._token python2 state lexbuf in
      (match tok, Lexer.top_mode state with
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
      | _ -> ()
      );
      tok
    | Lexer.STATE_IN_FSTRING_SINGLE ->
       Lexer.fstring_single state lexbuf
    | Lexer.STATE_IN_FSTRING_DOUBLE ->
       Lexer.fstring_double state lexbuf
    | Lexer.STATE_IN_FSTRING_TRIPLE_SINGLE ->
       Lexer.fstring_triple_single state lexbuf
    | Lexer.STATE_IN_FSTRING_TRIPLE_DOUBLE ->
       Lexer.fstring_triple_double state lexbuf
  in
  Parse_info.tokenize_all_and_adjust_pos ~unicode_hack:true
    file token TH.visitor_info_of_tok TH.is_eof
(*e: function [[Parse_python.tokens2]] *)

(*s: function [[Parse_python.tokens]] *)
let tokens a b = 
  Common.profile_code "Parse_python.tokens" (fun () -> tokens2 a b)
(*e: function [[Parse_python.tokens]] *)

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

(*s: function [[Parse_python.parse_basic]] *)
let rec parse_basic ?(parsing_mode=Python) filename = 
  let stat = Parse_info.default_stat filename in

  (* this can throw Parse_info.Lexical_error *)
  let toks = tokens parsing_mode filename in
  let toks = Parsing_hacks_python.fix_tokens toks in

  let tr, lexer, lexbuf_fake = 
    Parse_info.mk_lexer_for_yacc toks TH.is_comment in

  try 
    (* -------------------------------------------------- *)
    (* Call parser *)
    (* -------------------------------------------------- *)
    let xs =
      Common.profile_code "Parser_python.main" (fun () ->
        Parser_python.main lexer lexbuf_fake
      )
    in
    stat.PI.correct <- (Common.cat filename |> List.length);
    (Some xs, toks), stat

  with Parsing.Parse_error ->

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
    if parsing_mode = Python &&
       (tr.PI.passed |> Common.take_safe 10 |> List.exists (function
         | T.NAME (("print" | "exec"), _) 
         | T.ASYNC _ | T.AWAIT _ | T.NONLOCAL _ | T.TRUE _ | T.FALSE _
              -> true
         | _ -> false))
    then 
        (* note that we cant use tokens as the tokens are actually different
         * in Python2 mode, but we could optimize things a bit and just
         * transform those tokens here *)
        parse_basic ~parsing_mode:Python2 filename
    else begin
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
  
      stat.PI.bad     <- Common.cat filename |> List.length;
      (None, toks), stat
     end
(*e: function [[Parse_python.parse_basic]] *)


(*s: function [[Parse_python.parse]] *)
let parse ?parsing_mode a = 
  Common.profile_code "Parse_python.parse" (fun () -> 
      parse_basic ?parsing_mode a)
(*e: function [[Parse_python.parse]] *)

(*s: function [[Parse_python.parse_program]] *)
let parse_program ?parsing_mode file = 
  let ((astopt, _toks), _stat) = parse ?parsing_mode file in
  Common2.some astopt
(*e: function [[Parse_python.parse_program]] *)

(*****************************************************************************)
(* Sub parsers *)
(*****************************************************************************)

let (program_of_string: string -> AST_python.program) = fun s -> 
  Common2.with_tmp_file ~str:s ~ext:"py" (fun file ->
    parse_program file
  )

(*s: function [[Parse_python.any_of_string]] *)
(* for sgrep/spatch *)
let any_of_string ?(parsing_mode=Python) s = 
  Common2.with_tmp_file ~str:s ~ext:"py" (fun file ->
    let toks = tokens parsing_mode file in
    let toks = Parsing_hacks_python.fix_tokens toks in  
    let _tr, lexer, lexbuf_fake = PI.mk_lexer_for_yacc toks TH.is_comment in
    (* -------------------------------------------------- *)
    (* Call parser *)
    (* -------------------------------------------------- *)
    Parser_python.sgrep_spatch_pattern lexer lexbuf_fake
  )
(*e: function [[Parse_python.any_of_string]] *)


(*****************************************************************************)
(* Fuzzy parsing *)
(*****************************************************************************)

(*
let parse_fuzzy file =
  let toks = tokens file in
  let trees = Parse_fuzzy.mk_trees { Parse_fuzzy.
     tokf = TH.info_of_tok;
     kind = TH.token_kind_of_tok;
  } toks 
  in
  trees, toks
*)
(*e: pfff/lang_python/parsing/Parse_python.ml *)
