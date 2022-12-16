(* Yoann Padioleau
 *
 * Copyright (C) 2014 Facebook
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
module TH   = Token_helpers_rust

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* the token list contains also the comment-tokens *)
type program_and_tokens =
  Ast_rust.program (* NotParsedCorrectly if parse error *) *
  Parser_rust.token list

(*****************************************************************************)
(* Lexing only *)
(*****************************************************************************)

let tokens2 file =
  let token = Lexer_rust.token in
  Parse_info.tokenize_all_and_adjust_pos
    file token TH.visitor_info_of_tok TH.is_eof

let tokens a =
  Common.profile_code "Parse_rust.tokens" (fun () -> tokens2 a)

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let parse2 filename =
  let stat = Parse_info.default_stat filename in
  let toks_orig = tokens filename in
  (* TODO *)
  ((), toks_orig), stat

let parse a =
  Common.profile_code "Parse_rust.parse" (fun () -> parse2 a)
