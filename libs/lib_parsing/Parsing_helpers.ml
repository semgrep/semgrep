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
open Parse_info

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Some helpers for the different lexers and parsers in pfff *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* Many parsers need to interact with the lexer, or use tricks around
 * the stream of tokens, or do some error recovery, or just need to
 * pass certain tokens (like the comments token) which requires
 * to have access to this stream of remaining tokens.
 * The token_state type helps.
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

type input_stream = Str of string | File of Common.filename

let mk_tokens_state toks =
  {
    rest = toks;
    current = List.hd toks;
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

(*
I used to have:
 type program2 = toplevel2 list
  (* the token list contains also the comment-tokens *)
  and toplevel2 = Ast_php.toplevel * Parser_php.token list
type program_with_comments = program2

and a function below called distribute_info_items_toplevel that
would distribute the list of tokens to each toplevel entity.
This was when I was storing parts of AST in berkeley DB and when
I wanted to get some information about an entity (a function, a class)
I wanted to get the list also of tokens associated with that entity.

Now I just have
 type program_and_tokens = Ast_php.program * Parser_php.token list
because I don't use berkeley DB. I use codegraph and an entity_finder
we just focus on use/def and does not store huge asts on disk.


let rec distribute_info_items_toplevel2 xs toks filename =
  match xs with
  | [] -> raise Impossible
  | [Ast_php.FinalDef e] ->
      (* assert (null toks) ??? no cos can have whitespace tokens *)
      let info_item = toks in
      [Ast_php.FinalDef e, info_item]
  | ast::xs ->

      (match ast with
      | Ast_js.St (Ast_js.Nop None) ->
          distribute_info_items_toplevel2 xs toks filename
      | _ ->


      let ii = Lib_parsing_php.ii_of_any (Ast.Toplevel ast) in
      (* ugly: I use a fakeInfo for lambda f_name, so I have
       * have to filter the abstract info here
       *)
      let ii = List.filter PI.is_origintok ii in
      let (min, max) = PI.min_max_ii_by_pos ii in

      let toks_before_max, toks_after =
(* on very huge file, this function was previously segmentation fault
 * in native mode because span was not tail call
 *)
        Common.profile_code "spanning tokens" (fun () ->
        toks +> Common2.span_tail_call (fun tok ->
          match PI.compare_pos (TH.info_of_tok tok) max with
          | -1 | 0 -> true
          | 1 -> false
          | _ -> raise Impossible
        ))
      in
      let info_item = toks_before_max in
      (ast, info_item)::distribute_info_items_toplevel2 xs toks_after filename

let distribute_info_items_toplevel a b c =
  Common.profile_code "distribute_info_items" (fun () ->
    distribute_info_items_toplevel2 a b c
  )

 *)
let full_charpos_to_pos_large file =
  let chan = open_in_bin file in
  let size = Common2.filesize file + 2 in

  (* old: let arr = Array.create size  (0,0) in *)
  let arr1 = Bigarray.Array1.create Bigarray.int Bigarray.c_layout size in
  let arr2 = Bigarray.Array1.create Bigarray.int Bigarray.c_layout size in
  Bigarray.Array1.fill arr1 0;
  Bigarray.Array1.fill arr2 0;

  let charpos = ref 0 in
  let line = ref 0 in

  let full_charpos_to_pos_aux () =
    try
      while true do
        let s = input_line chan in
        incr line;
        let len = String.length s in

        (* '... +1 do'  cos input_line does not return the trailing \n *)
        let col = ref 0 in
        for i = 0 to len - 1 + 1 do
          (* old: arr.(!charpos + i) <- (!line, i); *)
          arr1.{!charpos + i} <- !line;
          arr2.{!charpos + i} <- !col;
          (* ugly: hack for weird windows files containing a single
           * carriage return (\r) instead of a carriage return + newline
           * (\r\n) to delimit newlines. Not recognizing those single
           * \r as a newline marker prevents Javascript ASI to correctly
           * insert semicolons.
           * note: we could fix info_from_charpos() too, but it's not
           * used for ASI so simpler to leave it as is.
           *)
          if i < len - 1 && String.get s i =$= '\r' then (
            incr line;
            col := -1);
          incr col
        done;
        charpos := !charpos + len + 1
      done
    with
    | End_of_file ->
        for
          i = !charpos
          to (* old: Array.length arr *)
             Bigarray.Array1.dim arr1 - 1
        do
          (* old: arr.(i) <- (!line, 0); *)
          arr1.{i} <- !line;
          arr2.{i} <- 0
        done;
        ()
  in
  full_charpos_to_pos_aux ();
  close_in chan;
  fun i -> (arr1.{i}, arr2.{i})
  [@@profiling]

(* This is mostly a copy-paste of full_charpos_to_pos_large,
   but using a string for a target instead of a file. *)
let full_charpos_to_pos_str s =
  let size = String.length s + 2 in

  (* old: let arr = Array.create size  (0,0) in *)
  let arr1 = Bigarray.Array1.create Bigarray.int Bigarray.c_layout size in
  let arr2 = Bigarray.Array1.create Bigarray.int Bigarray.c_layout size in
  Bigarray.Array1.fill arr1 0;
  Bigarray.Array1.fill arr2 0;

  let charpos = ref 0 in
  let line = ref 0 in
  let str_lines = String.split_on_char '\n' s in

  let full_charpos_to_pos_aux () =
    List.iter
      (fun s ->
        incr line;
        let len = String.length s in

        (* '... +1 do'  cos input_line does not return the trailing \n *)
        let col = ref 0 in
        for i = 0 to len - 1 + 1 do
          (* old: arr.(!charpos + i) <- (!line, i); *)
          arr1.{!charpos + i} <- !line;
          arr2.{!charpos + i} <- !col;
          (* ugly: hack for weird windows files containing a single
           * carriage return (\r) instead of a carriage return + newline
           * (\r\n) to delimit newlines. Not recognizing those single
           * \r as a newline marker prevents Javascript ASI to correctly
           * insert semicolons.
           * note: we could fix info_from_charpos() too, but it's not
           * used for ASI so simpler to leave it as is.
           *)
          if i < len - 1 && String.get s i =$= '\r' then (
            incr line;
            col := -1);
          incr col
        done;
        charpos := !charpos + len + 1)
      str_lines
  in
  full_charpos_to_pos_aux ();
  fun i -> (arr1.{i}, arr2.{i})
  [@@profiling]

(* Currently, lexing.ml, in the standard OCaml libray, does not handle
 * the line number position.
 * Even if there are certain fields in the lexing structure, they are not
 * maintained by the lexing engine so the following code does not work:
 *
 *   let pos = Lexing.lexeme_end_p lexbuf in
 *   sprintf "at file %s, line %d, char %d" pos.pos_fname pos.pos_lnum
 *      (pos.pos_cnum - pos.pos_bol) in
 *
 * Hence those types and functions below to overcome the previous limitation,
 * (see especially complete_token_location_large()).
 * alt:
 *   - in each lexer you need to take care of newlines and update manually
 *     the field.
 *)
let complete_token_location_large filename table x =
  {
    x with
    file = filename;
    line = fst (table x.charpos);
    column = snd (table x.charpos);
  }

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
  let adjust_info ii =
    {
      ii with
      token =
        (* could assert pinfo.filename = file ? *)
        (match ii.token with
        | OriginTok pi ->
            OriginTok (complete_token_location_large filename table pi)
        | ExpandedTok (pi, vpi, off) ->
            ExpandedTok
              (complete_token_location_large filename table pi, vpi, off)
        | FakeTokStr (s, vpi_opt) -> FakeTokStr (s, vpi_opt)
        | Ab -> raise Common.Impossible);
    }
  in
  let rec tokens_aux acc =
    let tok =
      try tokenizer lexbuf with
      | Lexical_error (s, info) -> raise (Lexical_error (s, adjust_info info))
    in
    if !Flag_parsing.debug_lexer then Common.pr2_gen tok;
    let tok = tok |> visitor_tok adjust_info in
    if is_eof tok then List.rev (tok :: acc) else tokens_aux (tok :: acc)
  in
  tokens_aux []

let tokenize_all_and_adjust_pos input_stream tokenizer visitor_tok is_eof =
  match input_stream with
  | Str str ->
      let lexbuf = Lexing.from_string str in
      let table = full_charpos_to_pos_str str in
      tokenize_and_adjust_pos lexbuf table "<file>" tokenizer visitor_tok is_eof
  | File file ->
      Common.with_open_infile file (fun chan ->
          let lexbuf = Lexing.from_channel chan in
          let table = full_charpos_to_pos_large file in
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

let adjust_pinfo_wrt_base base_loc loc =
  (* Note that charpos and columns are 0-based, whereas lines are 1-based. *)
  {
    loc with
    charpos = base_loc.charpos + loc.charpos;
    line = base_loc.line + loc.line - 1;
    column =
      (if loc.line =|= 1 then base_loc.column + loc.column else loc.column);
    file = base_loc.file;
  }

(* Token locations are supposed to denote the beginning of a token.
   Suppose we are interested in instead having line, column, and charpos of
   the end of a token instead.
   This is something we can do at relatively low cost by going through and inspecting
   the contents of the token, plus the start information.
*)
let get_token_end_info loc =
  let line, col =
    Stdcompat.String.fold_left
      (fun (line, col) c ->
        match c with
        | '\n' -> (line + 1, 0)
        | _ -> (line, col + 1))
      (loc.line, loc.column) loc.str
  in
  (line, col, loc.charpos + String.length loc.str)

let fix_token_location fix ii =
  {
    ii with
    token =
      (match ii.token with
      | OriginTok pi -> OriginTok (fix pi)
      | ExpandedTok (pi, vpi, off) -> ExpandedTok (fix pi, vpi, off)
      | FakeTokStr (s, vpi_opt) -> FakeTokStr (s, vpi_opt)
      | Ab -> Ab);
  }

let adjust_info_wrt_base base_loc ii =
  fix_token_location (adjust_pinfo_wrt_base base_loc) ii

(*****************************************************************************)
(* Error location report *)
(*****************************************************************************)

(*---------------------------------------------------------------------------*)
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

let error_message_token_location info =
  let filename = info.file in
  let lexeme = info.str in
  let lexstart = info.charpos in
  try error_messagebis filename (lexeme, lexstart) 0 with
  | End_of_file ->
      "PB in Common.error_message, position " ^ i_to_s lexstart
      ^ " given out of file:" ^ filename

let error_message_info info =
  let pinfo = unsafe_token_location_of_info info in
  error_message_token_location pinfo

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
