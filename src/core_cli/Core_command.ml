(* Yoann Padioleau
 *
 * Copyright (C) 2020-2024 Semgrep Inc.
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
module Out = Semgrep_output_v1_j
module E = Core_error

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Semgrep-core scan logic. The command-line parsing part is now done in
 * Core_CLI.ml. Here the functions are all passed a Core_scan_config.t
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* also used in Pro_core_command.ml *)
let output_core_results (caps : < Cap.stdout ; Cap.exit >)
    (result_or_exn : Core_result.result_or_exn) (config : Core_scan_config.t) :
    unit =
  (* TODO: delete this comment and -stat_matches
   * note: uncomment the following and use semgrep-core -stat_matches
   * to debug too-many-matches issues.
   * Common2.write_value matches "/tmp/debug_matches";
   *)
  match config.output_format with
  (* note that the dots have been displayed before in Core_scan.scan ()
   * for pysemgrep. Here we print the matches (and errors).
   *)
  | Json _ -> (
      let res =
        match result_or_exn with
        | Ok r -> r
        | Error exn ->
            let err = E.exn_to_error None Fpath_.fake_file exn in
            Core_result.mk_result_with_just_errors [ err ]
      in
      let res =
        Logs_.with_debug_trace
          "Core_command.core_output_of_matches_and_errors.1" (fun () ->
            Core_json_output.core_output_of_matches_and_errors res)
      in
      (*
        Not pretty-printing the json output (Yojson.Safe.prettify)
        because it kills performance, adding an extra 50% time on our
        old calculate_ci_perf.py benchmark.
        User should use an external tool like jq or ydump (latter comes with
        yojson) for pretty-printing json.
      *)
      let s = Out.string_of_core_output res in
      Logs.debug (fun m ->
          m "size of returned JSON string: %d" (String.length s));
      CapConsole.print caps#stdout s;
      match result_or_exn with
      | Error exn ->
          Core_exit_code.exit_semgrep caps#exit (Unknown_exception exn)
      | Ok _ -> ())
  (* The matches have already been printed before in Core_scan.scan(). We just
   * print the errors here (and matching explanations).
   * LATER: you should now use osemgrep for this
   *)
  | Text _ -> (
      match result_or_exn with
      | Ok res ->
          if config.matching_explanations then
            res.explanations
            |> Option.iter (List.iter Matching_explanation.print);
          if not (List_.null res.errors) then (
            Logs.warn (fun m ->
                m "some files were skipped or only partially analyzed");
            res.errors
            |> List.iter (fun err ->
                   Logs.warn (fun m -> m "%s" (E.string_of_error err))))
      | Error exn -> Exception.reraise exn)
  | NoOutput -> ()

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let run_conf (caps : < Cap.stdout ; Cap.tmp ; Cap.exit >)
    (config : Core_scan_config.t) : unit =
  match config.rule_source with
  | None -> failwith "you need to pass a rule source with -rules"
  | Some _ ->
      let res = Core_scan.scan (caps :> < Cap.tmp >) config in
      output_core_results (caps :> < Cap.stdout ; Cap.exit >) res config
