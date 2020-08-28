(* Yoann Padioleau
 *
 * Copyright (C) 2012 Yoann Padioleau
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

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * Just a small wrapper around the C++ parser
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type program_and_tokens = 
  Ast_c.program option * Parser_cpp.token list

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let parse file =
  let (ast2, stat) = Parse_cpp.parse_with_lang ~lang:Flag_parsing_cpp.C file in
  let ast = ast2 |> List.map fst in
  let toks = ast2 |> List.map snd |> List.flatten in
  let ast_opt, stat = 
    try Some (Ast_c_build.program ast), stat
    with exn ->
      pr2 (spf "PB: Ast_c_build, on %s (exn = %s)" file (Common.exn_to_s exn));
      (*None, { stat with Stat.bad = stat.Stat.bad + stat.Stat.correct } *)
      raise exn
  in
  (ast_opt, toks), stat

let parse_program file =
  let (program_and_tokens, _stat) = parse file in
  Common2.some (fst program_and_tokens)

let any_of_string str =
  let any = Parse_cpp.any_of_string Flag_parsing_cpp.C str in
  Ast_c_build.any any

