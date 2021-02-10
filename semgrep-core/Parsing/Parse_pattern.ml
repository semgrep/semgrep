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
open Common

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Mostly a wrapper around pfff Parse_generic.
*)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let dump_and_print_errors dumper (res : 'a Tree_sitter_run.Parsing_result.t) =
  (match res.program with
   | Some cst -> dumper cst
   | None -> failwith "unknown error from tree-sitter parser"
  );
  res.errors |> List.iter (fun err ->
    pr2 (Tree_sitter_run.Tree_sitter_error.to_string ~color:true err)
  )

let extract_pattern_from_tree_sitter_result (res : 'a Tree_sitter_run.Parsing_result.t) (print_errors : bool) =
  match res.Tree_sitter_run.Parsing_result.program, res.errors with
  | None, _ -> failwith "no pattern found"
  | Some x, [] -> x
  | Some _, _ :: _ ->
      if print_errors then
        res.errors |> List.iter (fun err ->
          pr2 (Tree_sitter_run.Tree_sitter_error.to_string ~color:true err)
        );
      failwith "error parsing the pattern"


(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)
let parse_pattern lang ?(print_errors = false) str =
  let any =
    match lang with
    | Lang.Csharp ->
        let res = Parse_csharp_tree_sitter.parse_pattern str in
        extract_pattern_from_tree_sitter_result res print_errors
    | Lang.Lua ->
        let res = Parse_lua_tree_sitter.parse_pattern str in
        extract_pattern_from_tree_sitter_result res print_errors
    | Lang.Rust ->
        let res = Parse_rust_tree_sitter.parse_pattern str in
        extract_pattern_from_tree_sitter_result res print_errors
    | Lang.Kotlin ->
        let res = Parse_kotlin_tree_sitter.parse_pattern str in
        extract_pattern_from_tree_sitter_result res print_errors
    (* use pfff *)
    | _ -> Parse_generic.parse_pattern lang str
  in
  Caching.prepare_pattern any;
  Check_pattern.check lang any;
  any

let dump_tree_sitter_pattern_cst lang file =
  match lang with
  | Lang.Csharp ->
      Tree_sitter_c_sharp.Parse.file file
      |> dump_and_print_errors Tree_sitter_c_sharp.CST.dump_tree
  | Lang.Lua ->
      Tree_sitter_lua.Parse.file file
      |> dump_and_print_errors Tree_sitter_lua.CST.dump_tree
  | Lang.Rust ->
      Tree_sitter_rust.Parse.file file
      |> dump_and_print_errors Tree_sitter_rust.CST.dump_tree
  | Lang.Kotlin ->
      Tree_sitter_kotlin.Parse.file file
      |> dump_and_print_errors Tree_sitter_kotlin.CST.dump_tree
  | _ -> ()

(*e: semgrep/parsing/Parse_pattern.ml *)
