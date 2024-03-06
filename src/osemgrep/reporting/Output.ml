open Common
open Fpath_.Operators
module OutJ = Semgrep_output_v1_j
module OutT = Semgrep_output_v1_t

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
   'semgrep scan' output.

   Partially translated from output.py

   We're using Out.put() below, not Logs.app(), because we want to output
   findings on stdout (Logs.app uses stderr). That also mean semgrep will
   display findings even with --quiet.
*)

type conf = {
  nosem : bool;
  autofix : bool;
  dryrun : bool;
  strict : bool;
  (* maybe should define an Output_option.t, or add a record to
   * Output_format.Text *)
  force_color : bool;
  logging_level : Logs.level option;
  (* Display options *)
  (* mix of --json, --emacs, --vim, etc. *)
  output_format : Output_format.t;
  max_chars_per_line : int;
  max_lines_per_finding : int;
}
[@@deriving show]

let default : conf =
  {
    nosem = true;
    autofix = false;
    dryrun = false;
    strict = false;
    logging_level = Some Logs.Warning;
    output_format = Output_format.Text;
    force_color = false;
    max_chars_per_line = 160;
    max_lines_per_finding = 10;
  }
(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let string_of_severity (severity : OutJ.match_severity) : string =
  OutJ.string_of_match_severity severity
  |> JSON.remove_enclosing_quotes_of_jstring

(*****************************************************************************)
(* Format dispatcher *)
(*****************************************************************************)

let dispatch_output_format (output_format : Output_format.t) (conf : conf)
    (cli_output : OutJ.cli_output) is_logged_in (hrules : Rule.hrules) =
  (* TOPORT? Sort keys for predictable output. Helps with snapshot tests *)
  match output_format with
  | Json ->
      let s = OutJ.string_of_cli_output cli_output in
      Out.put s
  | Vim ->
      cli_output.results
      |> List.iter (fun (m : OutJ.cli_match) ->
             match m with
             | { check_id; path; start; extra = { message; severity; _ }; _ } ->
                 let parts =
                   [
                     !!path;
                     spf "%d" start.line;
                     spf "%d" start.col;
                     (* TOPORT? restrict to just I|E|W ? *)
                     spf "%c" (string_of_severity severity).[0];
                     Rule_ID.to_string check_id;
                     message;
                   ]
                 in
                 Out.put (String.concat ":" parts))
  | Emacs ->
      (* TOPORT? sorted(rule_matches, key=lambda r: (r.path, r.rule_id)) *)
      cli_output.results
      |> List.iter (fun (m : OutJ.cli_match) ->
             match m with
             | {
              check_id;
              path;
              start;
              end_;
              extra = { message; severity; _ };
              _;
             } ->
                 let severity =
                   String.lowercase_ascii (string_of_severity severity)
                 in
                 let severity_and_ruleid =
                   if check_id =*= Constants.rule_id_for_dash_e then severity
                   else
                     match Rule_ID.last_elt_opt check_id with
                     | None -> severity
                     | Some x -> spf "%s(%s)" severity x
                 in
                 let line =
                   (* ugly: redoing the work done in cli_match_of_core_match.
                    * we can't use m.extra.lines because this field actually
                    * contains a string, not a string list.
                    *)
                   match
                     Semgrep_output_utils.lines_of_file_at_range (start, end_)
                       path
                   with
                   | [] -> ""
                   | x :: _ -> x (* TOPORT rstrip? *)
                 in
                 let parts =
                   [
                     !!path;
                     spf "%d" start.line;
                     spf "%d" start.col;
                     (* TOPORT? restrict to just I|E|W ? *)
                     severity_and_ruleid;
                     line;
                     message;
                   ]
                 in
                 Out.put (String.concat ":" parts))
  | Text ->
      Matches_report.pp_cli_output ~max_chars_per_line:conf.max_chars_per_line
        ~max_lines_per_finding:conf.max_lines_per_finding
        ~color_output:conf.force_color Format.std_formatter cli_output
  (* matches have already been displayed in a file_match_results_hook *)
  | Incremental -> ()
  | Sarif ->
      let sarif_json =
        Sarif_output.sarif_output is_logged_in hrules cli_output
      in
      Out.put (Sarif.Sarif_v_2_1_0_j.string_of_sarif_json_schema sarif_json)
  | Junit_xml ->
      let junit_xml = Junit_xml_output.junit_xml_output cli_output in
      Out.put junit_xml
  | Gitlab_sast ->
      let gitlab_sast_json = Gitlab_output.sast_output cli_output.results in
      Out.put (Yojson.Basic.to_string gitlab_sast_json)
  | Gitlab_secrets ->
      let gitlab_secrets_json =
        Gitlab_output.secrets_output cli_output.results
      in
      Out.put (Yojson.Basic.to_string gitlab_secrets_json)
  | Files_only ->
      let files =
        List.fold_left
          (fun acc (m : OutJ.cli_match) ->
            Common2.StringSet.add (Fpath.to_string m.path) acc)
          Common2.StringSet.empty cli_output.results
      in
      Out.put (files |> Common2.StringSet.to_list |> String.concat "\n")

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

(* This function takes a core runner output and makes it suitable for the user,
 * by filtering out nosem, setting messages, adding fingerprinting etc.
 *)
let preprocess_result (conf : conf) (res : Core_runner.result) : OutJ.cli_output
    =
  let cli_output : OutJ.cli_output =
    Cli_json_output.cli_output_of_core_results ~dryrun:conf.dryrun
      ~logging_level:conf.logging_level res.core res.hrules res.scanned
  in
  cli_output |> fun results ->
  {
    results with
    results = Cli_json_output.index_match_based_ids results.results;
  }

(* python: mix of output.OutputSettings(), output.OutputHandler(), and
 * output.output() all at once.
 * TODO: take a more precise conf than Scan_CLI.conf at some point
 *)
let output_result (conf : conf) (profiler : Profiler.t) ~is_logged_in
    (res : Core_runner.result) : OutJ.cli_output =
  (* In theory, we should build the JSON CLI output only for the
   * Json conf.output_format, but cli_output contains lots of data-structures
   * that are useful for the other formats (e.g., Vim, Emacs), so we build
   * it here.
   *)
  let cli_output () = preprocess_result conf res in
  (* TOPORT? output.output() *)
  let cli_output = Profiler.record profiler ~name:"ignores_times" cli_output in
  dispatch_output_format conf.output_format conf cli_output is_logged_in
    res.hrules;
  cli_output
[@@profiling]
