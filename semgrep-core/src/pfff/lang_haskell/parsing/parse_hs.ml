(* Yoann Padioleau
 *
 * Copyright (C) 2010 Facebook
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
module PI = Parse_info

module TH = Parser_hs

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* the token list contains also the comment-tokens *)
type program_and_tokens = Ast_hs.program * Parser_hs.token list

(*****************************************************************************)
(* Lexing only *)
(*****************************************************************************)

(* could factorize and take the tokenf and visitor_of_infof in argument
 * but sometimes copy-paste is ok.
*)
let tokens2 file =
  let table     = Parse_info.full_charpos_to_pos_large file in

  Common.with_open_infile file (fun chan ->
    let lexbuf = Lexing.from_channel chan in

    let ftoken lexbuf =
      Lexer_hs.token lexbuf
    in

    let rec tokens_aux acc =
      let tok = ftoken lexbuf in
      if !Flag.debug_lexer then Common.pr2_gen tok;

      let tok = tok |> TH.visitor_info_of_tok (fun ii ->
        { ii with PI.token=
                    (* could assert pinfo.filename = file ? *)
                    match ii.PI.token with
                    | PI.OriginTok pi ->
                        PI.OriginTok
                          (PI.complete_token_location_large file table pi)
                    | _ -> raise Todo
        })
      in

      if TH.is_eof tok
      then List.rev (tok::acc)
      else tokens_aux (tok::acc)
    in
    tokens_aux []
  )


let tokens a =
  Common.profile_code "Parse_hs.tokens" (fun () -> tokens2 a)

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let parse2 filename =
  let stat = Parse_info.default_stat filename in
  let toks_orig = tokens filename in
  (* TODO *)
  ((), toks_orig), stat

let parse a =
  Common.profile_code "Parse_hs.parse" (fun () -> parse2 a)
