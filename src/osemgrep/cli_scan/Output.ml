open Common
module Out = Semgrep_output_v1_j

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
   'semgrep scan' output.

   Partially translated from output.py

   LATER? It would be nice to move this file in osemgrep/reporting/, but
   it currently depends on Scan_CLI.conf and Core_runner so simpler to keep
   here for now.

   We're using Common.pr below, not Logs.app, because even with --quiet
   we want semgrep to report the findings.
*)

(*****************************************************************************)
(* Autofix *)
(*****************************************************************************)
(* TODO? It is a bit weird to have code modification done in a module called
 * Output.ml, but we need the cli_output to perform the autofix,
 * so easier to put the code here for now.
 *)

let apply_fixes (conf : Scan_CLI.conf) (cli_output : Out.cli_output) =
  (* TODO fix_regex *)
  let edits : Textedit.t list =
    Common.map_filter
      (fun (result : Out.cli_match) ->
        let path = result.Out.path in
        let* fix = result.Out.extra.fix in
        let start = result.Out.start.offset in
        let end_ = result.Out.end_.offset in
        Some { Textedit.path; start; end_; replacement_text = fix })
      cli_output.results
  in
  Textedit.apply_edits ~dryrun:conf.dryrun edits

let apply_fixes_and_warn (conf : Scan_CLI.conf) (cli_output : Out.cli_output) =
  (* TODO report when we fail to apply a fix because it overlaps with another?
   *  Currently it looks like the Python CLI will just blindly apply
   * overlapping fixes, probably breaking code.
   *
   * At some point we could re-run Semgrep on all files where some fixes
   * haven't been applied because they overlap with other fixes, and repeat
   * until all are applied (with some bounds to prevent divergence). This
   * probably happens rarely enough that it would be very fast, and it would
   * make the autofix experience better.
   *)
  let modified_files, _failed_fixes = apply_fixes conf cli_output in
  if not conf.dryrun then
    if modified_files <> [] then
      Logs.info (fun m ->
          m "successfully modified %s."
            (String_utils.unit_str (List.length modified_files) "file"))
    else Logs.info (fun m -> m "no files modified.")

(*****************************************************************************)
(* Format dispatcher *)
(*****************************************************************************)

let dispatch_output_format (output_format : Output_format.t)
    (conf : Scan_CLI.conf) (cli_output : Out.cli_output) =
  (* TOPORT? Sort keys for predictable output. Helps with snapshot tests *)
  match output_format with
  | Json ->
      let s = Out.string_of_cli_output cli_output in
      pr s
  | Vim ->
      cli_output.results
      |> List.iter (fun (m : Out.cli_match) ->
             match m with
             | { check_id; path; start; extra = { message; severity; _ }; _ } ->
                 let parts =
                   [
                     path;
                     spf "%d" start.line;
                     spf "%d" start.col;
                     (* TOPORT? restrict to just I|E|W ? *)
                     spf "%c" severity.[0];
                     check_id;
                     message;
                   ]
                 in
                 pr (String.concat ":" parts))
  | Emacs ->
      (* TOPORT? sorted(rule_matches, key=lambda r: (r.path, r.rule_id)) *)
      cli_output.results
      |> List.iter (fun (m : Out.cli_match) ->
             match m with
             | {
              check_id;
              path;
              start;
              end_;
              extra = { message; severity; _ };
              _;
             } ->
                 let severity = String.lowercase_ascii severity in
                 let severity_and_ruleid =
                   if check_id = Constants.cli_rule_id then severity
                   else
                     let xs =
                       check_id |> Str.split (Str.regexp_string ".") |> List.rev
                     in
                     match xs with
                     | [] -> severity
                     | x :: _ -> spf "%s(%s)" severity x
                 in
                 let line =
                   (* ugly: redoing the work done in cli_match_of_core_match.
                    * we can't use m.extra.lines because this field actually
                    * contains a string, not a string list.
                    *)
                   match
                     Cli_json_output.lines_of_file (start, end_) (Fpath.v path)
                   with
                   | [] -> ""
                   | x :: _ -> x (* TOPORT rstrip? *)
                 in
                 let parts =
                   [
                     path;
                     spf "%d" start.line;
                     spf "%d" start.col;
                     (* TOPORT? restrict to just I|E|W ? *)
                     severity_and_ruleid;
                     line;
                     message;
                   ]
                 in
                 pr (String.concat ":" parts))
  | Text ->
      Text_output.pp_cli_output ~max_chars_per_line:conf.max_chars_per_line
        ~max_lines_per_finding:conf.max_lines_per_finding
        ~color_output:conf.force_color Format.std_formatter cli_output
  | Gitlab_sast
  | Gitlab_secrets
  | Junit_xml
  | Sarif ->
      pr
        (spf "TODO: output format %s not supported yet"
           (Output_format.show output_format))

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

(* python: mix of output.OutputSettings(), output.OutputHandler(), and
 * output.output() all at once.
 * TODO: take a more precise conf than Scan_CLI.conf at some point
 *)
let output_result (conf : Scan_CLI.conf) (res : Core_runner.result) :
    Out.cli_error list =
  (* In theory, we should build the JSON CLI output only for the
   * Json conf.output_format, but cli_output contains lots of data-structures
   * that are useful for the other formats (e.g., Vim, Emacs), so we build
   * it here.
   *)
  let cli_output : Out.cli_output =
    Cli_json_output.cli_output_of_core_results ~logging_level:conf.logging_level
      ~rules_source:conf.rules_source res
  in
  let cli_output =
    let keep_ignored =
      (not conf.nosem) (* --disable-nosem *) || false
      (* TODO(dinosaure): [false] depends on the output formatter. Currently,
         we just have the JSON output. *)
    in
    Nosemgrep.process_ignores ~keep_ignored ~strict:conf.Scan_CLI.strict
      cli_output
  in
  (* ugly: but see the comment above why we do it here *)
  if conf.autofix then apply_fixes_and_warn conf cli_output;
  dispatch_output_format conf.output_format conf cli_output;
  cli_output.errors
