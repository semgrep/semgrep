(* Yoann Padioleau
 *
 * Copyright (C) 2021 r2c
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

module FT = File_type
module R = Rule
module E = Error_code

let logger = Logging.get_logger [__MODULE__]

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
let (lang_of_rules: Rule.t list -> Lang.t) = fun rs ->
  match rs |> Common.find_some_opt (fun r ->
    match r.R.languages with
    | R.L (l, _) -> Some l
    | _ -> None
  ) with
  | Some l -> l
  | None -> failwith "could not find a language"


let (ext_of_lang: Lang.t -> string) = fun lang ->
  match Lang.ext_of_lang lang with
  | [] -> failwith (spf "no extension for %s" (Lang.show lang))
  | x::_ -> x

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let test_rules xs =
  let fullxs =
    xs
    |> File_type.files_of_dirs_or_files (function
      | FT.Config (FT.Yaml (* | FT.Json*) | FT.Jsonnet) -> true | _ -> false)
    |> Skip_code.filter_files_if_skip_list ~root:xs
  in
  fullxs |> List.iter (fun file ->
    logger#info "processing rule %s" file;
    let rules = Parse_rule.parse file in
    rules |> List.iter Check_rule.check;

    let lang = lang_of_rules rules in
    let ext = ext_of_lang lang in
    let (d,b,_) = Common2.dbe_of_filename file in
    let target = Common2.filename_of_dbe (d,b,ext) in

    if not (Sys.file_exists target)
    then failwith (spf "could not find a target for %s" file);

    logger#info "processing target %s" target;

    (* expected *)
    let expected_error_lines = E.expected_error_lines_of_files [target] in

    (* actual *)
    let {Parse_target. ast; _} =
      Parse_target.parse_and_resolve_name_use_pfff_or_treesitter lang target
    in
    E.g_errors := [];
    let matches =
      Semgrep.check false (fun _ _ -> ()) rules (target, lang, ast) in
    matches |> List.iter JSON_report.match_to_error;

    let actual_errors = !E.g_errors in
    actual_errors |> List.iter (fun e ->
      logger#info "found error: %s" (E.string_of_error e)
    );
    E.compare_actual_to_expected actual_errors expected_error_lines
  )
