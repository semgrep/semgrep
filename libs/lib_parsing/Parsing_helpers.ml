(* Yoann Padioleau
 *
 * Copyright (C) 2010 Facebook
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

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Some helpers for the different lexers and parsers in pfff *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type input_source = Str of string | File of Fpath.t

let file s = File (Fpath.v s)

(* Many parsers need to interact with the lexer, or use tricks around
 * the stream of tokens, or do some error recovery, or just need to
 * pass certain tokens (like the comments token) which requires
 * to have access to this stream of remaining tokens.
 * The tokens_state type helps.
 *)
type 'tok tokens_state = {
  mutable rest : 'tok list;
  mutable current : 'tok;
  (* it's passed since last "checkpoint", not passed from the beginning *)
  mutable passed : 'tok list;
      (* if want to do some lalr(k) hacking ... cf yacfe.
       * mutable passed_clean : 'tok list;
       * mutable rest_clean :   'tok list;
       *)
}

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

let mk_tokens_state toks =
  {
    rest = toks;
    current = Common.hd_exn "unexpected empty list" toks;
    passed =
      []
      (* passed_clean = [];
       * rest_clean = (toks +> List.filter TH.is_not_comment);
       *);
  }

(* pad: hack around ocamllex to emulate the yyless() of flex. The semantic
 * is not exactly the same than yyless(), so I use yyback() instead.
 * http://my.safaribooksonline.com/book/programming/flex/9780596805418/a-reference-for-flex-specifications/yyless
 *)
let yyback n lexbuf =
  lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_curr_pos - n;
  let currp = lexbuf.Lexing.lex_curr_p in
  lexbuf.Lexing.lex_curr_p <-
    { currp with Lexing.pos_cnum = currp.Lexing.pos_cnum - n }

(*****************************************************************************)
(* Adjust file pos *)
(*****************************************************************************)

(* Why is it better to first get all the tokens?
 * Why not lex on-demand as yacc requires more tokens?
 * There are a few reasons:
 *  - for parsing hacks, it's easier to work on the full list
 *  - for error recovery strategy, it's easier to work on the full list
 *  - we do not need to care about line/col in the lexer and do that here
 *  - we can have comments as tokens (useful for codemap/efuns) and
 *    skip them easily with one Common.exclude
 *)
let tokenize_and_adjust_pos lexbuf table filename tokenizer visitor_tok is_eof =
  let adjust_info (ii : Tok.t) =
    (* could assert pinfo.filename = file ? *)
    Tok.(
      match ii with
      | OriginTok pi -> OriginTok (Tok.complete_location filename table pi)
      | ExpandedTok (pi, vloc) ->
          ExpandedTok (Tok.complete_location filename table pi, vloc)
      | FakeTokStr (s, vpi_opt) -> FakeTokStr (s, vpi_opt)
      | Ab -> raise Common.Impossible)
  in
  let rec tokens_aux acc =
    let tok =
      try tokenizer lexbuf with
      | Parsing_error.Lexical_error (s, info) ->
          raise (Parsing_error.Lexical_error (s, adjust_info info))
    in
    if !Flag_parsing.debug_lexer then Common.pr2_gen tok;
    let tok = tok |> visitor_tok adjust_info in
    if is_eof tok then List.rev (tok :: acc) else tokens_aux (tok :: acc)
  in
  tokens_aux []

let tokenize_all_and_adjust_pos input_source tokenizer visitor_tok is_eof =
  match input_source with
  | Str str ->
      let lexbuf = Lexing.from_string str in
      let table = Pos.full_charpos_to_pos_str str in
      tokenize_and_adjust_pos lexbuf table "<file>" tokenizer visitor_tok is_eof
  | File path ->
      let file = Fpath.to_string path in
      Common.with_open_infile file (fun chan ->
          let lexbuf = Lexing.from_channel chan in
          let table = Pos.full_charpos_to_pos_large file in
          tokenize_and_adjust_pos lexbuf table file tokenizer visitor_tok is_eof)

(* Hacked lex. Ocamlyacc expects a function returning one token at a time
 * but we actually lex all the file so we need a wrapper to turn that
 * into a stream.
 * This function use refs passed by parse. 'tr' means 'token refs'.
 *
 * Why pass is_comment? Why not skip comments before?
 *  - for error recovery to still return comments for separate entities?
 *  - TODO?
 *)
let mk_lexer_for_yacc toks is_comment =
  let tr = mk_tokens_state toks in
  let rec lexer lexbuf =
    match tr.rest with
    | [] ->
        Common.pr2 "LEXER: ALREADY AT END";
        tr.current
    | v :: xs ->
        tr.rest <- xs;
        tr.current <- v;
        tr.passed <- v :: tr.passed;
        if is_comment v then lexer lexbuf else v
  in
  let lexbuf_fake = Lexing.from_function (fun _buf _n -> raise Impossible) in
  (tr, lexer, lexbuf_fake)

(*****************************************************************************)
(* Error location report *)
(*****************************************************************************)

(* return line x col x str_line  from a charpos. This function is quite
 * expensive so don't use it to get the line x col from every token in
 * a file. Instead use full_charpos_to_pos.
 *)
let (info_from_charpos : int -> filename -> int * int * string) =
 fun charpos filename ->
  (* Currently lexing.ml does not handle the line number position.
   * Even if there is some fields in the lexing structure, they are not
   * maintained by the lexing engine :( So the following code does not work:
   *   let pos = Lexing.lexeme_end_p lexbuf in
   *   sprintf "at file %s, line %d, char %d" pos.pos_fname pos.pos_lnum
   *      (pos.pos_cnum - pos.pos_bol) in
   * Hence this function to overcome the previous limitation.
   *)
  let chan = open_in_bin filename in
  let linen = ref 0 in
  let posl = ref 0 in
  let rec charpos_to_pos_aux last_valid =
    let s =
      try Some (input_line chan) with
      | End_of_file when charpos =|= last_valid -> None
    in
    incr linen;
    match s with
    | Some s ->
        let s = s ^ "\n" in
        if !posl + String.length s > charpos then (
          close_in chan;
          (!linen, charpos - !posl, s))
        else (
          posl := !posl + String.length s;
          charpos_to_pos_aux !posl)
    | None -> (!linen, charpos - !posl, "\n")
  in
  let res = charpos_to_pos_aux 0 in
  close_in chan;
  res
 [@@profiling]

(* Decalage is here to handle stuff such as cpp which include file and who
 * can make shift.
 *)
let (error_messagebis : filename -> string * int -> int -> string) =
 fun filename (lexeme, lexstart) decalage ->
  let charpos = lexstart + decalage in
  let tok = lexeme in
  let line, pos, linecontent = info_from_charpos charpos filename in
  let s = Common2.chop linecontent in
  let s =
    (* this happens in Javascript for minified files *)
    if String.length s > 200 then
      String.sub s 0 100 ^ " (TOO LONG, SHORTEN!)..."
    else s
  in
  spf
    "File \"%s\", line %d, column %d,  charpos = %d\n\
    \    around = '%s', whole content = %s" filename line pos charpos tok s

let error_message filename (lexeme, lexstart) =
  try error_messagebis filename (lexeme, lexstart) 0 with
  | End_of_file ->
      "PB in Common.error_message, position " ^ i_to_s lexstart
      ^ " given out of file:" ^ filename

let error_message_token_location (info : Tok.location) =
  let filename = info.pos.file in
  let lexeme = info.str in
  let lexstart = info.pos.charpos in
  try error_messagebis filename (lexeme, lexstart) 0 with
  | End_of_file ->
      "PB in Common.error_message, position " ^ i_to_s lexstart
      ^ " given out of file:" ^ filename

let error_message_info info =
  let loc = Tok.unsafe_loc_of_tok info in
  error_message_token_location loc

let print_bad line_error (start_line, end_line) filelines =
  pr2 ("badcount: " ^ i_to_s (end_line - start_line));

  for i = start_line to end_line do
    let s = filelines.(i) in
    let line =
      (* this happens in Javascript for minified files *)
      if String.length s > 200 then
        String.sub s 0 100 ^ " (TOO LONG, SHORTEN!)..."
      else s
    in

    if i =|= line_error then pr2 ("BAD:!!!!!" ^ " " ^ line)
    else pr2 ("bad:" ^ " " ^ line)
  done
