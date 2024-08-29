(* Yoann Padioleau
 *
 * Copyright (C) 2021 Semgrep Inc.
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
open Fpath_.Operators
module FT = File_type
module R = Rule
module E = Core_error
module TCM = Test_compare_matches

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let test_rules ?(unit_testing = false) (caps : Core_scan.caps) xs =
  let fullxs, _skipped_paths =
    xs
    |> File_type.files_of_dirs_or_files (function
         | FT.Config FT.Yaml -> true
         | _ -> false)
    |> List_.exclude (fun filepath ->
           (* .test.yaml files are YAML target files rather than config files! *)
           Fpath.has_ext ".test.yaml" filepath
           || Fpath.has_ext ".rule.yaml" filepath)
    |> Skip_code.filter_files_if_skip_list ~root:xs
  in

  let newscore = Common2.empty_score () in
  let ext = "rule" in
  let total_mismatch = ref 0 in

  fullxs
  |> List.iter (fun file ->
         Logs.info (fun m -> m "test_rules: processing rule file %s" !!file);

         (* just a sanity check *)
         (* rules |> List.iter Check_rule.check; *)
         let target =
           try
             let d, b, ext = Filename_.dbe_of_filename !!file in
             Common2.readdir_to_file_list d @ Common2.readdir_to_link_list d
             |> List_.find_some (fun file2 ->
                    let path2 = Filename.concat d file2 |> Fpath.v in
                    (* Config files have a single .yaml extension (assumption),
                     * but test files may have multiple extensions, e.g.
                     * ".test.yaml" (YAML test files), ".sites-available.conf",
                     * ... *)
                    match Filename_.dbe_of_filename_many_ext_opt file2 with
                    | None -> None
                    | Some (_, b2, ext2) ->
                        if
                          b = b2 && ext <> ext2
                          (* .yaml.j2 files are Jinja2 templates to generate Semgrep config files *)
                          && ext2 <> "yaml.j2"
                          (* ugly: jsonnet exclusion below because of some .jsonnet and .yaml
                           * ambiguities in tests/rules *)
                          && ext2 <> "jsonnet"
                        then Some path2
                        else None)
           with
           | Not_found -> failwith (spf "could not find a target for %s" !!file)
         in
         Logs.info (fun m -> m "test_rules: processing target %s" !!target);
         (* expected *)
         (* not tororuleid! not ok:! *)
         let regexp = ".*\\b\\(ruleid\\|todook\\):.*" in
         let expected_error_lines =
           TCM.expected_error_lines_of_files ~regexp [ target ]
         in

         (* actual *)
         let actual_errors =
           try Check_rule.run_checks caps file [ target ] with
           | exn ->
               failwith
                 (spf "exn on %s (exn = %s)" !!file (Common.exn_to_s exn))
         in
         actual_errors
         |> List.iter (fun e ->
                Logs.err (fun m ->
                    m "test_rules: found error: %s" (E.string_of_error e)));
         match
           TCM.compare_actual_to_expected
             ~to_location:TCM.location_of_core_error actual_errors
             expected_error_lines
         with
         | Ok () -> Hashtbl.add newscore !!file Common2.Ok
         | Error (num_errors, msg) ->
             Logs.err (fun m -> m "%s" msg);
             Hashtbl.add newscore !!file (Common2.Pb msg);
             total_mismatch := !total_mismatch + num_errors;
             if unit_testing then Alcotest.fail msg);
  if not unit_testing then
    Logs.info (fun m ->
        m "%s" (Parsing_stat.regression_information ~ext xs newscore));
  Logs.info (fun m -> m "total mismatch: %d" !total_mismatch)
