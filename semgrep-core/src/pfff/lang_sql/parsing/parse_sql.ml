(* Yoann Padioleau
 *
 * Copyright (C) 2010 Facebook
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

module PI = Parse_info
module TH   = Token_helpers_sql

(*****************************************************************************)
(* Lexing only *)
(*****************************************************************************)

let tokens2 file =
  let token lexbuf = Lexer_sql.lexer lexbuf in
  Parse_info.tokenize_all_and_adjust_pos
    file token TH.visitor_info_of_tok TH.is_eof

let tokens a =
  Common.profile_code "Parse_sql.tokens" (fun () -> tokens2 a)

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let parse2 file =
  Common.with_open_infile file (fun chan ->
    let lexbuf = Lexing.from_channel chan in

    let _expr = Parser_sql.main Lexer_sql.lexer lexbuf in
    ()
  )

let parse a =
  Common.profile_code "Parse_sql.parse" (fun () -> parse2 a)

let parse_string str =
  let lexbuf = Lexing.from_string str in
  let expr = Parser_sql.main Lexer_sql.lexer lexbuf in
  expr
