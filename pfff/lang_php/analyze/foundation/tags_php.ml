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
open Common

open Cst_php
module Ast = Cst_php

module Tags = Tags_file
module E = Entity_code

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * Making a better TAGS file. M-x idx => it finds it!
 * It does not go to $idx in a file. Works for XHP. Works with
 * completion. Essentially a thin adapter over defs_uses_php.ml.
 *
 * Bench: time to process ~/www ? 7min the first time, which
 * is quite longer than ctags. But what is the price of correctness ?
 * Moreover one can easily put this into a cron and even shares
 * the results of such a cron to multiple developers via NFS.
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let tag_of_ident filelines name kind =
  let info = Ast.info_of_ident name in
  Tags.tag_of_info filelines info kind

(*****************************************************************************)
(* Main function *)
(*****************************************************************************)

let tags_of_ast ast filelines =
  let defs = Defs_uses_php.defs_of_any (Program ast) in

  defs |> List.map (fun (name, enclosing_name_opt, kind) ->
    match kind with
    | E.Class ->
        (match name with
        | Name _ -> [tag_of_ident filelines name kind]
        | XhpName (xs, tok) ->
            (* XHP classes have a different syntax for their definitions
             * (class :x:foo ...) and their uses ($xhp = <x:foo ...). People
             * want to find such class using either forms so here we
             * generate two tags.
             *)
            let s1 = ":" ^ Common.join ":" xs in
            let s2 = Common.join ":" xs in
            [Tags.tag_of_info filelines (Parse_info.rewrap_str s1 tok) kind;
             Tags.tag_of_info filelines (Parse_info.rewrap_str s2 tok) kind;
            ]
        )
    | E.Function | E.Constant | E.Type ->
        [ tag_of_ident filelines name kind]
    | E.Method ->
        (match enclosing_name_opt with
        | None -> raise Impossible
        | Some class_name ->
            (* Generate a 'class::method' tag. Can then have
             * a nice completion and know all the methods available
             * in a class (the short Eiffel-like profile).
             *)
            let info = Ast.info_of_ident name in
            let info' = Parse_info.rewrap_str
              (Ast.str_of_ident class_name  ^ "::" ^ Ast.str_of_ident name) info in

            let s = Ast.str_of_ident name in
            let yieldmagic =
              if s =~ "^yield\\(.*\\)" then
                let tail = Common.matched1 s in
                [ "gen"; "prepare"; "get"] |>
                List.map (fun w ->
                  let info = Parse_info.rewrap_str
                    (Ast.str_of_ident class_name ^ "::" ^ w ^ tail) info in
                  Tags.tag_of_info filelines info kind)
              else [] in

            let prepmagic =
              if s =~ "^prepare\\(.*\\)" then
                let tail = Common.matched1 s in
                let info = Parse_info.rewrap_str
                  (Ast.str_of_ident class_name ^ "::gen" ^ tail) info in
                [ Tags.tag_of_info filelines info kind ]
              else [] in

            [Tags.tag_of_info filelines info' kind] @ yieldmagic @ prepmagic
        )
    | _ ->
        failwith "your defs_of_any is probably wrong"
  ) |> List.flatten

(* obsolete ? stuff with heavy_tagging ?
 * if heavy_tagging then begin
 * let info = Ast.info_of_name name in
 * let s = Ast.name name in
 * let info' = Ast.rewrap_str ("F_" ^ s) info in
 * Common.push2 (tag_of_info filelines info') defs;
 * end;
 * let s = Ast.name name in
 * if heavy_tagging then begin
 * let info = Ast.info_of_name name in
 * let info' = Ast.rewrap_str ("C_" ^ s) info in
 * Common.push2 (tag_of_info filelines info') defs;
 * end;
 * if heavy_tagging then begin
 * let info' = Ast.rewrap_str ("M_" ^ s) info in
 * Common.push2 (tag_of_info filelines info') defs;
 * end;
 *)

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let php_defs_of_files_or_dirs ?(verbose=false) ?(include_hack=false) xs =
  let files = Lib_parsing_php.find_source_files_of_dir_or_files ~include_hack:include_hack xs in

  files |> Console.progress ~show:verbose (fun k ->
   List.map (fun file ->
    k ();
    let (ast) =
      try  Parse_php.parse_program file
      with Parsing.Parse_error ->
        Common.pr2 (spf "warning: parsing problem in %s" file);
        []
    in
    let filelines = Common2.cat_array file in
    let defs =
      try tags_of_ast ast filelines
      with
      | Timeout -> raise Timeout
      | exn ->
          pr2 (spf "PB with %s, exn = %s" file (Common.exn_to_s exn));
          []
    in
    (file, defs)
  ))
  |> Tags_file.add_method_tags_when_unambiguous
