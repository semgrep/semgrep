(* Yoann Padioleau
 *
 * Copyright (C) 2009-2011 Facebook
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

module Ast  = Cst_php
module Flag = Flag_parsing
module Flag_php = Flag_parsing_php
module TH   = Token_helpers_php
module PI = Parse_info

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* A PHP parser.
 *
 * related work:
 *  - miamide, also in ocaml, but didn't support all of PHP
 *  - https://github.com/sfindeisen/phphard, also written in ocaml, but
 *    seems pretty rudimentary
*)

(*****************************************************************************)
(* Error diagnostic  *)
(*****************************************************************************)
let error_msg_tok tok =
  PI.error_message_info (TH.info_of_tok tok)

(*****************************************************************************)
(* Lexing only *)
(*****************************************************************************)
let tokens2 ?(init_state=Lexer_php.INITIAL) file =
  Lexer_php.reset();
  Lexer_php._mode_stack := [init_state];

  let token lexbuf =
    let tok =
      (* for yyless emulation *)
      match !Lexer_php._pending_tokens with
      | x::xs ->
          Lexer_php._pending_tokens := xs;
          x
      | [] ->
          (match Lexer_php.current_mode () with
           | Lexer_php.INITIAL ->
               Lexer_php.initial lexbuf
           | Lexer_php.ST_IN_SCRIPTING ->
               Lexer_php.st_in_scripting lexbuf
           | Lexer_php.ST_IN_SCRIPTING2 ->
               Lexer_php.st_in_scripting lexbuf
           | Lexer_php.ST_DOUBLE_QUOTES ->
               Lexer_php.st_double_quotes lexbuf
           | Lexer_php.ST_BACKQUOTE ->
               Lexer_php.st_backquote lexbuf
           | Lexer_php.ST_LOOKING_FOR_PROPERTY ->
               Lexer_php.st_looking_for_property lexbuf
           | Lexer_php.ST_LOOKING_FOR_VARNAME ->
               Lexer_php.st_looking_for_varname lexbuf
           | Lexer_php.ST_VAR_OFFSET ->
               Lexer_php.st_var_offset lexbuf
           | Lexer_php.ST_START_HEREDOC s ->
               Lexer_php.st_start_heredoc s lexbuf
           | Lexer_php.ST_START_NOWDOC s ->
               Lexer_php.st_start_nowdoc s lexbuf

          )
    in
    if not (TH.is_comment tok)
    then Lexer_php._last_non_whitespace_like_token := Some tok;
    tok
  in
  Parse_info.tokenize_all_and_adjust_pos
    file token TH.visitor_info_of_tok TH.is_eof

let tokens ?init_state a =
  Common.profile_code "Parse_php.tokens" (fun () -> tokens2 ?init_state a)


let is_comment v =
  TH.is_comment v ||
  (* TODO a little bit specific to FB ? *)
  (match v with
   | Parser_php.T_OPEN_TAG _ -> true
   | Parser_php.T_CLOSE_TAG _ -> true
   | _ -> false
  )

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let parse2 ?(pp=(!Flag_php.pp_default)) filename =

  let orig_filename = filename in
  let filename =
    (* note that now that pfff support XHP constructs directly,
     * this code is not that needed.
    *)
    match pp with
    | None -> orig_filename
    | Some cmd ->
        Common.profile_code "Parse_php.pp_maybe" (fun () ->

          let pp_flag = if !Flag_php.verbose_pp then "-v" else "" in

          (* The following requires the preprocessor command to
           * support the -q command line flag.
           *
           * Maybe a little bit specific to XHP and xhpize ... But
           * because I use as a convention that 0 means no_need_pp, if
           * the preprocessor does not support -q, it should return an
           * error code, in which case we will fall back to the regular
           * case. *)
          let cmd_need_pp =
            spf "%s -q %s %s" cmd pp_flag filename in
          if !Flag_php.verbose_pp then pr2 (spf "executing %s" cmd_need_pp);
          let ret = Sys.command cmd_need_pp in
          if ret = 0
          then orig_filename
          else begin
            Common.profile_code "Parse_php.pp" (fun () ->
              let tmpfile = Common.new_temp_file "pp" ".pphp" in
              let fullcmd =
                spf "%s %s %s > %s" cmd pp_flag filename tmpfile in
              if !Flag_php.verbose_pp then pr2 (spf "executing %s" fullcmd);
              let ret = Sys.command fullcmd in
              if ret <> 0
              then failwith "The preprocessor command returned an error code";
              tmpfile
            )
          end
        )
  in

  let stat = PI.default_stat filename in
  let filelines = Common2.cat_array filename in

  let toks = tokens filename in
  (* note that now that pfff support XHP constructs directly,
   * this code is not that needed.
  *)
  let toks =
    if filename = orig_filename
    then toks
    else (* Pp_php.adapt_tokens_pp ~tokenizer:tokens ~orig_filename toks *)
      failwith "no more pp"
  in
  let toks = Parsing_hacks_php.fix_tokens toks in

  let tr, lexer, lexbuf_fake =
    Parse_info.mk_lexer_for_yacc toks is_comment in

  let checkpoint = TH.line_of_tok tr.PI.current in

  let elems =
    try (
      (* -------------------------------------------------- *)
      (* Call parser *)
      (* -------------------------------------------------- *)
      Left
        (Common.profile_code "Parser_php.main" (fun () ->
           Parser_php.main lexer lexbuf_fake
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
      {PI. ast = xs; tokens = toks; stat }
  | Right (info_of_bads, line_error, cur) ->

      if not !Flag.error_recovery
      then raise (PI.Parsing_error (TH.info_of_tok cur));

      if !Flag.show_parsing_error
      then pr2 ("parse error\n = " ^ error_msg_tok cur);
      let checkpoint2 = Common.cat filename |> List.length in

      if !Flag.show_parsing_error
      then PI.print_bad line_error (checkpoint, checkpoint2) filelines;
      (* TODO: just count the skipped lines; Use Hashtbl.length strategy *)
      stat.PI.error_line_count <- stat.PI.total_line_count;

      let info_item = (List.rev tr.PI.passed) in
      {PI. ast = [Ast.NotParsedCorrectly info_of_bads]; tokens=info_item; stat}

let parse ?pp a =
  Common.profile_code "Parse_php.parse" (fun () -> parse2 ?pp a)

let parse_program ?pp file =
  let res = parse ?pp file in
  res.PI.ast

(*****************************************************************************)
(* Sub parsers *)
(*****************************************************************************)

let any_of_string s =
  Common.save_excursion Flag_parsing.sgrep_mode true (fun () ->
    Common2.with_tmp_file ~str:s ~ext:"java" (fun file ->
      let toks = tokens ~init_state:Lexer_php.ST_IN_SCRIPTING file in
      let toks = Parsing_hacks_php.fix_tokens toks in
      let _tr, lexer, lexbuf_fake = PI.mk_lexer_for_yacc toks is_comment in
      Parser_php.semgrep_pattern lexer lexbuf_fake
    ))

(*
 * todo: obsolete now with parse_any ? just redirect to parse_any ?
 *
 * This function is useful not only to test but also in our own code
 * as a shortcut to build complex expressions
 *)
let (expr_of_string: string -> Cst_php.expr) = fun s ->
  let tmpfile = Common.new_temp_file "pfff_expr_of_s" "php" in
  Common.write_file tmpfile ("<?php \n" ^ s ^ ";\n");

  let ast = parse_program tmpfile in

  let res =
    (match ast with
     | [Ast.TopStmt (Ast.ExprStmt (e, _tok));Ast.FinalDef _] -> e
     | _ -> failwith "only expr pattern are supported for now"
    )
  in
  Common.erase_this_temp_file tmpfile;
  res

(* It is clearer for our testing code to programmatically build source files
 * so that all the information about a test is in the same
 * file. You don't have to open extra files to understand the test
 * data. This function is useful mostly for our unit tests
*)
let (program_of_string: string -> Cst_php.program) = fun s ->
  let tmpfile = Common.new_temp_file "pfff_expr_of_s" "php" in
  Common.write_file tmpfile ("<?php \n" ^ s ^ "\n");
  let ast = parse_program tmpfile in
  Common.erase_this_temp_file tmpfile;
  ast

(* use program_of_string when you can *)
let tmp_php_file_from_string ?(header="<?php\n") s =
  let tmp_file = Common.new_temp_file "test" ".php" in
  Common.write_file ~file:tmp_file (header ^ s);
  tmp_file


(* this function is useful mostly for our unit tests *)
let (tokens_of_string: string -> Parser_php.token list) = fun s ->
  let tmpfile = Common.new_temp_file "pfff_tokens_of_s" "php" in
  Common.write_file tmpfile ("<?php \n" ^ s ^ "\n");
  let toks = tokens tmpfile in
  Common.erase_this_temp_file tmpfile;
  toks


(* A fast-path parser of xdebug expressions in xdebug dumpfiles.
 * See xdebug.ml *)
let (xdebug_expr_of_string: string -> Cst_php.expr) = fun _s ->
(*
  let lexbuf = Lexing.from_string s in
  let expr = Parser_php.expr basic_lexer_skip_comments lexbuf in
  expr
*)
  raise Todo
