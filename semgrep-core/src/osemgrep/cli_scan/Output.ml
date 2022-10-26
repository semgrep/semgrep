open Common
module Out = Semgrep_output_v0_j

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
   'semgrep scan' output.

   Partially translated from output.py

   For now only the JSON output is supported.
   TODO? move most of the content of this file to Output_JSON.ml?
*)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let output_result (conf : Scan_CLI.conf) (res : Core_runner.result) : unit =
  let cli_output : Out.cli_output =
    Cli_json_output.cli_output_of_core_results conf res
  in
  (* TOPORT? Sort keys for predictable output. Helps with snapshot tests *)
  match conf.output_format with
  | Json ->
      let s = Out.string_of_cli_output cli_output in
      pr s
  | Vim ->
      (* alt: could start from res instead of cli_output *)
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
      (* alt: could start from res instead of cli_output *)
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
                   (* ugly: redoing the work done in cli_match_of_core_match *)
                   match Cli_json_output.lines_of_file (start, end_) path with
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
  | Text
  | Gitlab_sast
  | Gitlab_secrets
  | Junit_xml
  | Sarif ->
      pr
        (spf "TODO: output format %s not supported yet"
           (Constants.show_output_format conf.output_format))
