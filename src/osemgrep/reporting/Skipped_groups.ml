(* Yoann Padioleau, Robur
 *
 * Copyright (C) 2024 Semgrep Inc.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * LICENSE for more details.
 *)
open Common
module Out = Semgrep_output_v1_t

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Skipped targets grouped in different categories for reporting purpose *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type t = {
  (* targets skipped because of file targeting spec *)
  ignored : Out.skipped_target list;
  size : Out.skipped_target list;
  include_ : Out.skipped_target list;
  exclude : Out.skipped_target list;
  always : Out.skipped_target list;
  other : Out.skipped_target list;
  (* targets possibly skipped because there was a parsing/matching/...
   * error while running the engine on it (see errors_to_skipped() below)
   *)
  errors : Out.skipped_target list;
}

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let errors_to_skipped (errors : Out.core_error list) : Out.skipped_target list =
  errors
  |> List_.filter_map (fun Out.{ location; message; rule_id; _ } ->
         let* loc = location in
         Some
           Out.
             {
               path = loc.path;
               reason = Analysis_failed_parser_or_internal_error;
               details = Some message;
               rule_id;
             })

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let group (skipped : Out.skipped_target list) : t =
  let groups =
    skipped
    |> Assoc.group_by (fun (skipped : Out.skipped_target) ->
           match skipped.reason with
           | Gitignore_patterns_match
           | Semgrepignore_patterns_match ->
               `Semgrepignore
           | Too_big
           | Exceeded_size_limit ->
               `Size
           | Cli_include_flags_do_not_match -> `Include
           | Cli_exclude_flags_match -> `Exclude
           | Analysis_failed_parser_or_internal_error -> `Error
           | Always_skipped -> `Always
           | Excluded_by_config
           | Wrong_language
           | Minified
           | Binary
           | Dotfile
           | Nonexistent_file
           | Irrelevant_rule
           | Too_many_matches
           | Insufficient_permissions ->
               `Other)
  in
  {
    ignored = List.assoc_opt `Semgrepignore groups ||| [];
    include_ =
      (try List.assoc `Include groups with
      | Not_found -> []);
    exclude =
      (try List.assoc `Exclude groups with
      | Not_found -> []);
    size =
      (try List.assoc `Size groups with
      | Not_found -> []);
    always =
      (try List.assoc `Always groups with
      | Not_found -> []);
    other =
      (try List.assoc `Other groups with
      | Not_found -> []);
    errors =
      (try List.assoc `Error groups with
      | Not_found -> []);
  }
