(* Yoann Padioleau
 *
 * Copyright (C) 2010, 2013 Facebook
 * Copyright (C) 2019 Semgrep Inc.
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
open Fpath_.Operators
module Flag = Flag_parsing
module Ast = Ast_js
module TH = Token_helpers_js
module PS = Parsing_stat
module Log = Log_parser_javascript.Log
module LogLib = Log_lib_parsing.Log

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Error diagnostic  *)
(*****************************************************************************)
let error_msg_tok tok = Parsing_helpers.error_message_info (TH.info_of_tok tok)

(*****************************************************************************)
(* Helpers  *)
(*****************************************************************************)

(* TODO: switch to 'deriving visitor' *)
module V = Visitor_js

let extract_info_visitor recursor =
  let globals = ref [] in
  let hooks =
    {
      V.default_visitor with
      V.kinfo = (fun (_k, _) i -> Stack_.push i globals);
    }
  in
  let vout = V.mk_visitor hooks in
  recursor vout;
  List.rev !globals

let ii_of_any any = extract_info_visitor (fun visitor -> visitor any)

(* Now that we parse individual items separately, and that we do not
 * rely on EOF as a final marker, we need to take care when
 * running the parser on the next item. Indeed, certain
 * items such as if and try have optional trailing element
 * (an else branch, a finally stmt) that forces the parser to
 * lookahead and consume an extra token in lexer_function to
 * decide to reduce or not the current item.
 * Before running the parser on the next item we need to put
 * back this consumed token back in the stream!
 *
 * alt:
 *  - use ii_of_any and check if tr.current is in it
 *    WARNING!: this requires that the last token of what is
 *    parsed is in the CST! otherwise this will reintroduce in
 *    the stream an extra token so take care!
 *  - match for If without else and Try without Finally in AST
 *    (only cases?)
 *  - less: ask on caml list if can access parser state?
 *    but Parsing.parser_env is abstract and no much API around it.
 *
 * see also top comment in tests/js/items.js
 *
 *)
let put_back_lookahead_token_if_needed tr item_opt =
  match item_opt with
  | None -> ()
  | Some item ->
      let iis = ii_of_any (Ast.Program [ item ]) in
      let current = tr.Parsing_helpers.current in
      let info = TH.info_of_tok current in
      (* bugfix: without test on is_origintok, the parser timeout
       * TODO: why?
       *)
      if (not (Tok.is_origintok info)) || List.mem info iis then ()
      else (
        (* TODO: could sanity check that what we put back make sense, for
         * example we should never put back a closing '}', which can
         * happen if the item returned is incomplete and does not contain
         * all the tokens (more risky now that we use ast_js.ml instead of
         * cst_js.ml)
         *)
        Log.debug (fun m ->
            m "putting back lookahead token %s" (Dumper.dump current));
        tr.Parsing_helpers.rest <- current :: tr.Parsing_helpers.rest;
        tr.Parsing_helpers.passed <-
          List_.tl_exn "unexpected empty list" tr.Parsing_helpers.passed)

(*****************************************************************************)
(* ASI (Automatic Semicolon Insertion) part 2 *)
(*****************************************************************************)

(* To get the right to do an ASI, the parse error needs to be
 * on a new line. In some cases though the current offending token might
 * not be the first token on the line. Indeed in
 *   if(true) continue
 *   x = y;
 * the parser does not generate a parse error at 'x' but at '=' because
 * 'continue' can accept an identifier.
 * In fact, the situation is worse, because for
 *   if(true) continue
 *   x;
 * we must generate two independent statements, even though it does
 * look like a parse error. To handle those cases we
 * need a parsing_hack phase that inserts semicolon after the
 * continue if there is a newline after.
 *)

let rec line_previous_tok xs =
  match xs with
  | [] -> None
  | x :: xs ->
      if TH.is_comment x then line_previous_tok xs else Some (TH.line_of_tok x)

let asi_opportunity charpos last_charpos_error cur tr =
  match tr.Parsing_helpers.passed with
  | _ when charpos <= !last_charpos_error -> None
  | [] -> None
  (* see tests/js/parsing/asi_incr_bis.js *)
  | offending
    :: ((Parser_js.T_INCR _ | Parser_js.T_DECR _) as real_offender)
    :: xs -> (
      match (line_previous_tok xs, offending) with
      | Some line, _ when TH.line_of_tok real_offender > line ->
          Some ([ offending ], real_offender, xs)
      | _, (Parser_js.T_RCURLY _ | Parser_js.EOF _) ->
          Some ([], offending, real_offender :: xs)
      | _ -> None)
  | offending :: xs -> (
      match (line_previous_tok xs, cur) with
      | Some line, _ when TH.line_of_tok cur > line -> Some ([], offending, xs)
      | _, (Parser_js.T_RCURLY _ | Parser_js.EOF _) -> Some ([], offending, xs)
      | _ -> None)

let asi_insert charpos last_charpos_error tr
    (passed_before, passed_offending, passed_after) =
  let info = TH.info_of_tok passed_offending in
  let virtual_semi = Parser_js.T_VIRTUAL_SEMICOLON (Ast.fakeInfoAttach info) in
  Log.debug (fun m ->
      m "ASI: insertion fake ';' at %s" (Tok.stringpos_of_tok info));

  let toks =
    List.rev passed_after
    @ [ virtual_semi; passed_offending ]
    @ passed_before @ tr.Parsing_helpers.rest
  in
  (* like in Parse_info.mk_tokens_state *)
  tr.Parsing_helpers.rest <- toks;
  tr.Parsing_helpers.current <- List_.hd_exn "impossible empty list" toks;
  tr.Parsing_helpers.passed <- [];
  (* try again!
   * This significantly slow-down parsing, especially on minimized
   * files. Indeed, minimizers put all the code inside a giant
   * function, which means no incremental parsing, and leverage ASI
   * before right curly brace to save one character (hmmm). This means
   * that we parse again and again the same series of tokens, just
   * progressing a bit more everytime, and restarting from scratch.
   * This is quadratic behavior.
   *)
  last_charpos_error := charpos

(*****************************************************************************)
(* Lexing only *)
(*****************************************************************************)

let tokens input_source =
  Lexer_js.reset ();
  let token lexbuf =
    let tok =
      match Lexer_js.current_mode () with
      | Lexer_js.ST_IN_CODE -> Lexer_js.initial lexbuf
      | Lexer_js.ST_IN_XHP_TAG current_tag ->
          Lexer_js.st_in_xhp_tag current_tag lexbuf
      | Lexer_js.ST_IN_XHP_TEXT current_tag ->
          Lexer_js.st_in_xhp_text current_tag lexbuf
      | Lexer_js.ST_IN_BACKQUOTE -> Lexer_js.backquote lexbuf
    in
    if not (TH.is_comment tok) then
      Lexer_js._last_non_whitespace_like_token := Some tok;
    tok
  in
  Parsing_helpers.tokenize_all_and_adjust_pos input_source token
    TH.visitor_info_of_tok TH.is_eof
[@@profiling]

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let parse2 opt_timeout (filename : Fpath.t) =
  let stat = Parsing_stat.default_stat !!filename in

  let toks = tokens (Parsing_helpers.file !!filename) in
  let toks = Parsing_hacks_js.fix_tokens toks in
  let toks = Parsing_hacks_js.fix_tokens_ASI toks in

  let tr, lexer, lexbuf_fake =
    Parsing_helpers.mk_lexer_for_yacc toks TH.is_comment
  in

  let last_charpos_error = ref 0 in

  let rec parse_module_item_or_eof tr =
    try
      let item =
        (* -------------------------------------------------- *)
        (* Call parser *)
        (* -------------------------------------------------- *)
        Profiling.profile_code "Parser_js.module_item" (fun () ->
            Parser_js.module_item_or_eof lexer lexbuf_fake)
      in
      (* this seems optional *)
      Parsing.clear_parser ();
      put_back_lookahead_token_if_needed tr item;
      Either.Left item
    with
    | Parsing.Parse_error -> (
        (* coupling: update also any_of_string if you modify the code below *)
        let cur = tr.Parsing_helpers.current in
        let info = TH.info_of_tok cur in
        let charpos = Tok.bytepos_of_tok info in

        (* try Automatic Semicolon Insertion *)
        match asi_opportunity charpos last_charpos_error cur tr with
        | None ->
            if !Flag.show_parsing_error then
              LogLib.err (fun m -> m "parse error \n = %s" (error_msg_tok cur));
            Right cur
        | Some (passed_before, passed_offending, passed_after) ->
            asi_insert charpos last_charpos_error tr
              (passed_before, passed_offending, passed_after);
            parse_module_item_or_eof tr)
  in
  let rec aux tr =
    let line_start = TH.line_of_tok tr.Parsing_helpers.current in
    let res = parse_module_item_or_eof tr in
    tr.Parsing_helpers.passed <- [];
    (*
    let _passed = tr.PI.passed in
    let lines =
      try
        let (head, _middle, last) = Common2.head_middle_tail passed in
        let line1 = TH.line_of_tok last in
        let line2 = TH.line_of_tok head in
        line2 - line1 (* +1? *)
      with _ -> 1
    in
*)
    match res with
    (* EOF *)
    | Either.Left None -> []
    | Either.Left (Some x) ->
        Log.debug (fun m ->
            m "%s" (spf "parsed: %s" (Ast.Program [ x ] |> Ast_js.show_any)));

        x :: aux tr
    | Either.Right err_tok ->
        let max_line = UFile.cat filename |> List.length in
        (if !Flag.show_parsing_error then
           let filelines = UFile.cat_array filename in
           let cur = tr.Parsing_helpers.current in
           let line_error = TH.line_of_tok cur in
           Log.err (fun m ->
               m "%s"
                 (Parsing_helpers.show_parse_error_line line_error
                    (line_start, min max_line (line_error + 10))
                    filelines)));
        if !Flag.error_recovery then (
          (* todo? try to recover? call 'aux tr'? but then can be really slow*)
          (* TODO: count a bad line twice? use Hashtbl.length tech instead *)
          stat.PS.error_line_count <-
            stat.PS.error_line_count + (max_line - line_start);
          [])
        else raise (Parsing_error.Syntax_error (TH.info_of_tok err_tok))
  in
  let items =
    match
      Time_limit.set_timeout_opt ~name:"Parse_js.parse" opt_timeout (fun () ->
          aux tr)
    with
    | Some res -> res
    | None ->
        if !Flag.show_parsing_error then
          Log.err (fun m -> m "TIMEOUT on %s" !!filename);
        stat.PS.error_line_count <- stat.PS.total_line_count;
        stat.PS.have_timeout <- true;
        []
  in
  { Parsing_result.ast = items; tokens = toks; stat }

let parse ?timeout a =
  Profiling.profile_code "Parse_js.parse" (fun () -> parse2 timeout a)

let parse_program file =
  let res = parse file in
  res.Parsing_result.ast

let program_of_string (caps : < Cap.tmp >) (w : string) : Ast.a_program =
  CapTmp.with_temp_file caps#tmp ~contents:w ~suffix:".js" parse_program

(*****************************************************************************)
(* Sub parsers *)
(*****************************************************************************)

let type_of_string s =
  let lexbuf = Lexing.from_string s in
  let rec lexer lexbuf =
    let res = Lexer_js.initial lexbuf in
    if TH.is_comment res then lexer lexbuf else res
  in
  let ty = Parser_js.type_for_lsif lexer lexbuf in
  ty

(* for sgrep/spatch *)
let any_of_string s =
  Common.save_excursion Flag_parsing.sgrep_mode true (fun () ->
      let toks = tokens (Parsing_helpers.Str s) in
      let toks = Parsing_hacks_js.fix_tokens toks in
      let toks = Parsing_hacks_js.fix_tokens_ASI toks in

      let tr, lexer, lexbuf_fake =
        Parsing_helpers.mk_lexer_for_yacc toks TH.is_comment
      in
      let last_charpos_error = ref 0 in

      let rec parse_pattern tr =
        try Parser_js.sgrep_spatch_pattern lexer lexbuf_fake with
        | Parsing.Parse_error -> (
            let cur = tr.Parsing_helpers.current in
            let info = TH.info_of_tok cur in
            let charpos = Tok.bytepos_of_tok info in
            (* try Automatic Semicolon Insertion *)
            match asi_opportunity charpos last_charpos_error cur tr with
            | None -> raise Parsing.Parse_error
            | Some (passed_before, passed_offending, passed_after) ->
                asi_insert charpos last_charpos_error tr
                  (passed_before, passed_offending, passed_after);
                parse_pattern tr)
      in
      parse_pattern tr)
