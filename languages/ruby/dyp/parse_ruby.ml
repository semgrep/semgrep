(* Mike Furr
 *
 * Copyright (C) 2010 Mike Furr
 * Copyright (C) 2020 r2c
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *     * Redistributions of source code must retain the above copyright
 *       notice, this list of conditions and the following disclaimer.
 *     * Redistributions in binary form must reproduce the above copyright
 *       notice, this list of conditions and the following disclaimer in the
 *       documentation and/or other materials provided with the distribution.
 *     * Neither the name of the <organization> nor the
 *       names of its contributors may be used to endorse or promote products
 *       derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL <COPYRIGHT HOLDER> BE LIABLE FOR ANY
 * DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *)
open Common
module Flag = Flag_parsing
module PS = Parsing_stat
module TH = Token_helpers_ruby
module HH = Parser_ruby_helpers
module Utils = Utils_ruby

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(*****************************************************************************)
(* Error diagnostic  *)
(*****************************************************************************)
let error_msg_tok tok = Parsing_helpers.error_message_info (TH.info_of_tok tok)

(*****************************************************************************)
(* Lexing only *)
(*****************************************************************************)

(* todo? reuse Parse_info.tokenize_all_and_adjust_pos, but that
 * would require to have completely independent lexer and parser
 * which seems not possible with Ruby.
 *)
let mk_lexer filename input_source =
  let state = Lexer_parser_ruby.create ("top_lexer", Lexer_ruby.top_lexer) in

  let table =
    match input_source with
    | Parsing_helpers.File file ->
        Pos.full_charpos_to_pos_large (Fpath.to_string file)
    | Parsing_helpers.Str str -> Pos.full_charpos_to_pos_str str
  in

  let adjust_info (ii : Tok.t) =
    (* could assert pinfo.filename = file ? *)
    match ii with
    | Tok.OriginTok pi -> (
        try Tok.OriginTok (Tok.complete_location filename table pi) with
        | Invalid_argument "index out of bounds" ->
            (* TODO: fix! *)
            (* pr2_gen pi *)
            pr2_once (spf "TODO:%s: adjust info out-of-bounds" filename);
            Tok.OriginTok pi)
    | _ -> failwith "adjust_info: no an OriginTok"
  in
  let toks = ref [] in

  (* set the environment *)
  HH.clear_env ();
  let env = Utils.default_opt Utils.StrSet.empty None in
  HH.set_env env;

  let rec lexer lexbuf =
    let tok =
      try Lexer_ruby.token state lexbuf with
      | Parsing_error.Lexical_error (s, info) ->
          raise (Parsing_error.Lexical_error (s, adjust_info info))
    in
    if !Flag_parsing.debug_lexer then Common.pr2_gen tok;

    let tok = tok |> TH.visitor_info_of_tok adjust_info in
    Common.push tok toks;
    if TH.is_comment tok then lexer lexbuf else tok
  in
  (toks, lexer)

exception Parse_ruby_timeout

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let parse2 opt_timeout file =
  let stat = Parsing_stat.default_stat file in

  Common.with_open_infile file (fun chan ->
      let toks, lexer = mk_lexer file (Parsing_helpers.file file) in
      try
        (* -------------------------------------------------- *)
        (* Call parser *)
        (* -------------------------------------------------- *)
        let lexbuf = Lexing.from_channel chan in
        let lst =
          (* GLR parsing can be very time consuming *)
          match
            Time_limit.set_timeout_opt ~name:"Parse_ruby.parse" opt_timeout
              (fun () -> Parser_ruby.main lexer lexbuf)
          with
          | Some res -> res
          | None -> raise Parse_ruby_timeout
        in

        (* check for ambiguous parse trees *)
        let l = List.map fst lst in
        let l' =
          HH.uniq_list (fun a b -> if Ast_ruby.equal_stmts a b then 0 else -1) l
        in
        HH.do_fail "program" l' Ast_ruby.show_program;

        let ast = Common.hd_exn "unexpected empty list" l' in
        (*orig-todo? Ast.mod_ast (replace_heredoc state) ast*)
        { Parsing_result.ast; tokens = List.rev !toks; stat }
      with
      | (Dyp.Syntax_error | Failure _ | Stack.Empty | Parse_ruby_timeout) as exn
        ->
          let cur =
            match !toks with
            | [] -> failwith (spf "No token at all for %s" file)
            | x :: _xs -> x
          in
          (* todo: need to fix those! *)
          let s = Common.exn_to_s exn in

          (* pr2 (spf "Exn on %s = %s" file s); *)
          if (not !Flag.error_recovery) && exn =*= Dyp.Syntax_error then
            raise (Parsing_error.Syntax_error (TH.info_of_tok cur));
          if (not !Flag.error_recovery) && exn <> Dyp.Syntax_error then
            raise (Parsing_error.Other_error (s, TH.info_of_tok cur));

          if !Flag.show_parsing_error && exn =*= Dyp.Syntax_error then (
            pr2 ("parse error \n = " ^ error_msg_tok cur);
            let filelines = Common2.cat_array file in
            let checkpoint2 = Common.cat file |> List.length in
            let line_error = Tok.line_of_tok (TH.info_of_tok cur) in
            Parsing_helpers.print_bad line_error (0, checkpoint2) filelines);

          stat.PS.error_line_count <- stat.PS.total_line_count;
          if exn =*= Parse_ruby_timeout then stat.PS.have_timeout <- true;
          { Parsing_result.ast = []; tokens = List.rev !toks; stat })

let parse ?timeout file = parse2 timeout file

let parse_program file =
  let res = parse file in
  res.Parsing_result.ast

(* for semgrep *)
let any_of_string ?timeout str =
  Common.save_excursion Flag_parsing.sgrep_mode true (fun () ->
      let _toks, lexer = mk_lexer "<file>" (Parsing_helpers.Str str) in
      let lexbuf = Lexing.from_string str in
      try
        (* -------------------------------------------------- *)
        (* Call parser *)
        (* -------------------------------------------------- *)
        let lst =
          (* GLR parsing can be very time consuming *)
          match
            Time_limit.set_timeout_opt ~name:"Parse_ruby.any_of_string" timeout
              (fun () -> Parser_ruby.sgrep_spatch_pattern lexer lexbuf)
          with
          | Some res -> res
          | None -> raise Parse_ruby_timeout
        in

        (* check for ambiguous parse trees *)
        let l = List.map fst lst in
        let l' =
          HH.uniq_list (fun a b -> if Ast_ruby.equal_any a b then 0 else -1) l
        in
        HH.do_fail "any" l' Ast_ruby.show_any;

        let ast = Common.hd_exn "unexpected empty list" l' in
        ast
      with
      | (Dyp.Syntax_error | Failure _ | Stack.Empty | Parse_ruby_timeout) as exn
        ->
          Exception.catch_and_reraise exn)
