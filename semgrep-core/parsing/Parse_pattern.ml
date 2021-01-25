(*s: semgrep/parsing/Parse_pattern.ml *)
(*s: pad/r2c copyright *)
(* Yoann Padioleau
 *
 * Copyright (C) 2019-2021 r2c
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
(*e: pad/r2c copyright *)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Mostly a wrapper around pfff Parse_generic.
*)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let extract_pattern_from_tree_sitter_result res =
  match res.Tree_sitter_run.Parsing_result.program, res.errors with
  | None, _ -> failwith "no pattern found"
  | Some x, [] -> x
  (* todo: return error location, use Error module like in Parse_code *)
  | Some _, _::_ -> failwith "error parsing the pattern"


(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)
let parse_pattern lang str =
  let any =
    match lang with
    | Lang.Csharp ->
        let res = Parse_csharp_tree_sitter.parse_pattern str in
        extract_pattern_from_tree_sitter_result res
    | Lang.Lua ->
        let res = Parse_lua_tree_sitter.parse_pattern str in
        extract_pattern_from_tree_sitter_result res
    | Lang.Rust ->
        let res = Parse_rust_tree_sitter.parse_pattern str in
        extract_pattern_from_tree_sitter_result res
    (* use pfff *)
    | _ -> Parse_generic.parse_pattern lang str
  in
  (* TODO: Caching.prepare_pattern any; commented because of test regressions *)
  Check_pattern.check lang any;
  any

(*e: semgrep/parsing/Parse_pattern.ml *)
