(* Yoann Padioleau
 *
 * Copyright (C) 2021 R2C
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

module Flag = Flag_parsing
module TH   = Token_helpers_scala
module PI = Parse_info

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Driver for the Scala parser
*)

(*****************************************************************************)
(* Error diagnostic  *)
(*****************************************************************************)
let _error_msg_tok tok =
  Parse_info.error_message_info (TH.info_of_tok tok)

(*****************************************************************************)
(* Lexing only *)
(*****************************************************************************)

let tokens file =
  Lexer_scala.reset();
  let token lexbuf =
    let tok =
      match Lexer_scala.current_mode() with
      | Lexer_scala.ST_IN_CODE ->
          Lexer_scala.token lexbuf
      | Lexer_scala.ST_IN_INTERPOLATED_DOUBLE ->
          Lexer_scala.in_interpolated_double lexbuf
      | Lexer_scala.ST_IN_INTERPOLATED_TRIPLE ->
          Lexer_scala.in_interpolated_triple lexbuf
    in
    tok
  in
  (* set to false to parse correctly arrows *)
  Parse_info.tokenize_all_and_adjust_pos
    file token TH.visitor_info_of_tok TH.is_eof
[@@profiling]

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)
let parse filename =

  let stat = Parse_info.default_stat filename in
  let toks = tokens filename in

 (*
  let tr, lexer, lexbuf_fake =
    Parse_info.mk_lexer_for_yacc toks TH.is_comment in
  *)
  try
    (* -------------------------------------------------- *)
    (* Call parser *)
    (* -------------------------------------------------- *)
    (*
    let xs =
      Parser_scala.compilationUnit    lexer lexbuf_fake
    in
    *)
    let xs = Parser_scala_recursive_descent.parse toks in
    { PI.ast = xs; tokens = toks; stat }

  with PI.Parsing_error cur when !Flag.error_recovery
                              && not !Flag.debug_parser    ->
      if !Flag.show_parsing_error
      then begin
        pr2 ("parse error \n = " ^ (Parse_info.error_message_info cur));
        let filelines = Common2.cat_array filename in
        let checkpoint2 = Common.cat filename |> List.length in
        let line_error = PI.line_of_info cur in
        Parse_info.print_bad line_error (0, checkpoint2) filelines;
      end;
      stat.PI.error_line_count <- stat.PI.total_line_count;
      { PI.ast = []; tokens = toks; stat }
[@@profiling]

let parse_program file =
  let res = parse file in
  res.PI.ast

(*****************************************************************************)
(* Sub parsers *)
(*****************************************************************************)
(* for semgrep *)
let any_of_string s =
  Common.save_excursion Flag_parsing.sgrep_mode true (fun () ->
    Common2.with_tmp_file ~str:s ~ext:"scala" (fun file ->
      let toks = tokens file in
      (* -------------------------------------------------- *)
      (* Call parser *)
      (* -------------------------------------------------- *)
      Parser_scala_recursive_descent.semgrep_pattern toks
    ))

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let find_source_files_of_dir_or_files xs =
  Common.files_of_dir_or_files_no_vcs_nofilter xs
  |> List.filter (fun filename ->
    match File_type.file_type_of_file filename with
    | File_type.PL (File_type.Scala) -> true
    | _ -> false
  ) |> Common.sort
