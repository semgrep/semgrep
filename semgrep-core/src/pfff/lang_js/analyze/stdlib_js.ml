(* Yoann Padioleau
 *
 * Copyright (C) 2019 r2c
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
module TH = Token_helpers_js
module E = Entity_code
module F = Ast_fuzzy

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* This module gives access to the Javascript standard library entities,
 * which is especially useful for Graph_code_js.
 *
 * related:
 *  - Flow standard lib definitions (small set)
 *    https://github.com/facebook/flow/tree/master/lib
 *  - Typescript standard lib definitions (bigger set)
 *    https://github.com/microsoft/TypeScript/tree/master/src/lib
 *  - TypeScript generator, which generates the .generated.d.ts above
 *    https://github.com/Microsoft/TSJS-lib-generator
 *  - Closure compiler lib definitions (untyped)
 *    https://github.com/google/closure-compiler/tree/master/externs
 *    extended in Semmle
 *    https://github.com/Semmle/ql/tree/master/javascript/externs
 *
 *  - TypeScript definition manager (deprecated)
 *    https://github.com/typings/typings
 *  - High quality Typescript type definitions (looks like the new standard)
 *    http://definitelytyped.org/ especially
 *    https://github.com/DefinitelyTyped/DefinitelyTyped/blob/master/types/node/globals.d.ts
 *
 * src: https://stackoverflow.com/questions/46696266/where-can-i-find-documentation-for-typescripts-built-in-types-and-standard-libr
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(*****************************************************************************)
(* Constants *)
(*****************************************************************************)
let path_stdlib =
  Filename.concat Config_pfff.path_pfff_home "data/js_stdlib/stdlib.js"

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
let extract_defs file =
  let toks =
    (* Flow/Typescript and JSX have lexing conflicts *)
    Common.save_excursion Flag_parsing_js.jsx false (fun () ->
        Parse_js.tokens file)
  in
  let toks = Common.exclude TH.is_comment toks in
  let trees =
    Lib_ast_fuzzy.mk_trees
      { Lib_ast_fuzzy.tokf = TH.info_of_tok; kind = TH.token_kind_of_tok }
      toks
  in
  (* process only toplevel elements, not the one
   * under 'declare module xx { ... }', by not recursing inside
   * F.Braces
   *)
  let rec aux acc = function
    | [] -> List.rev acc
    | F.Tok ("function", _) :: F.Tok (s, _) :: xs ->
        aux ((E.Function, s) :: acc) xs
    | F.Tok ("var", _) :: F.Tok (s, _) :: xs -> aux ((E.Global, s) :: acc) xs
    | F.Tok ("class", _) :: F.Tok (s, _) :: xs -> aux ((E.Class, s) :: acc) xs
    | _x :: xs -> aux acc xs
  in
  aux [] trees

let add_and_report_if_already_there already file str =
  if Hashtbl.mem already str then (
    let dupfile = Hashtbl.find already str in
    pr2 (spf "entity %s defined both in %s and %s" str dupfile file);
    true)
  else (
    Hashtbl.replace already str file;
    false)

(*****************************************************************************)
(* Stdlib.js generator *)
(*****************************************************************************)

(* example of use:
 * ./codegraph_build -lang js -build_stdlib ~/r2c/other-progs/facebook/flow/lib/core.js  ~/pfff/data/js_stdlib/
 *)
let extract_from_sources files dst =
  let defs = files |> List.map (fun file -> (file, extract_defs file)) in
  let dstfile = Filename.concat dst "stdlib.js" in
  let auxfile = Filename.concat dst "aux.js" in
  let already = Hashtbl.create 101 in
  let lines =
    defs
    |> List.map (fun (file, xs) ->
           [
             "";
             "// ------------------------------------------------------";
             spf "// from %s" file;
             "// ------------------------------------------------------";
           ]
           @ (xs
             |> Common.map_filter (fun (kind, s) ->
                    let str =
                      match kind with
                      | E.Function -> spf "function %s() { }" s
                      | E.Class -> spf "class %s { }" s
                      | E.Global -> spf "var %s;" s
                      | _ ->
                          failwith
                            (spf "extract_from_source: unrecognized kind %s"
                               (E.string_of_entity_kind kind))
                    in
                    (* Flow allow some overloading so the same function can be defined twice
                     * but with different types. Codegraph does not like that.
                     * Flow also have the same function name or class in different files.
                     *)
                    if add_and_report_if_already_there already file str then
                      None
                    else Some str)))
    |> List.flatten
  in
  let lines =
    [ "//Auto-generated by extract_from_sources (do not edit this file)" ]
    @ lines
    @ [
        "";
        "// ------------------------------------------------------";
        "// Aux file";
        "// ------------------------------------------------------";
      ]
    @ Common.cat auxfile
  in
  let s = String.concat "\n" lines in
  Common.write_file ~file:dstfile s;
  pr2 (spf "Standard Javascript library file %s is ready" dstfile);
  ()
