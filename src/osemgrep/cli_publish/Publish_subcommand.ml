open Common
module Out = Semgrep_output_v1_t

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
   Parse a semgrep-publish command, execute it and exit.

   semgrep-publish is a subcommand which takes in a file or folder containing
   Semgrep rule(s), and uploads them to the Registry.

   Translated from publish.py
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)
type caps = < Cap.stdout ; Cap.network ; Cap.tmp >

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let get_test_code_for_config upload_target =
  let config_filenames =
    match Rule_tests.get_config_filenames upload_target with
    | [] -> [ upload_target ]
    | other -> other
  in
  let config_test_filenames =
    Rule_tests.get_config_test_filenames upload_target config_filenames
      upload_target
  in
  (config_filenames, config_test_filenames)

let upload_rule caps rule_file (conf : Publish_CLI.conf) test_code_file =
  let rule_file = Fpath.to_string rule_file in
  (* THINK: is this the same as get_config(...).get_rules()? *)
  let rules, errors =
    let rules_and_origins, errors =
      Rule_fetching.rules_from_dashdash_config ~token_opt:(Some caps#token)
        ~rewrite_rule_ids:true
        (caps :> < Cap.network ; Cap.tmp >)
        (Rules_config.File (Fpath.v rule_file))
    in
    let rules, invalid_rules =
      Rule_fetching.partition_rules_and_invalid rules_and_origins
    in
    ( rules,
      (invalid_rules
      |> List_.map (fun ((_, rule_id, _) as err) ->
             Rule_error.mk_error ~rule_id (InvalidRule err)))
      @ errors )
  in

  match (errors, rules) with
  | _ :: _, _ ->
      Logs.err (fun m ->
          m "    Invalid rule definition: %s is invalid: %s" rule_file
            (errors
            |> List_.map Rule_error.string_of_error
            |> String.concat ", "));
      false
  | _, [ rule ] -> (
      (* TODO: This emits a "fatal: No remote configured to list refs from."
         when run from a non git repository, even though in this command, it
         shouldn't be fatal and is handled.
         This output needs to be suppressed.
      *)
      let project_url =
        match Git_wrapper.project_url () with
        | None -> "<none>"
        | Some url -> url
      in
      let rule_source = UFile.read_file (Fpath.v rule_file) in
      let rule =
        let origin_note =
          Common.spf "published from %s in %s" rule_file project_url
        in
        let metadata =
          rule.metadata |> Option.map JSON.to_yojson
          |> Option.map (fun yojson ->
                 JSON.update yojson
                   (`Assoc [ ("rule-origin-note", `String origin_note) ]))
          |> Option.map JSON.from_yojson
        in
        { rule with metadata }
      in

      let request_json =
        let language =
          match Analyzer.to_langs rule.target_analyzer with
          | [] -> `Null
          | lang :: _ -> `String (Lang.to_string lang)
        in
        let deployment_id =
          match Semgrep_App.get_deployment_from_token caps with
          | None -> `Null
          | Some config -> `Int config.id
        in
        let test_target =
          match test_code_file with
          | None -> `Null
          | Some file -> `String (UFile.read_file file)
        in
        let registry_id =
          match conf.registry_id with
          | None -> `Null
          | Some registry_id -> `String registry_id
        in
        (* LATER: define this structure in ATD *)
        let rule = Yaml.of_string_exn rule_source in
        `Assoc
          [
            ("definition", JSON.ezjsonm_to_yojson rule);
            ( "visibility",
              `String (Publish_CLI.string_of_visibility conf.visibility) );
            ("language", language);
            ("deployment_id", deployment_id);
            ("test_target", test_target);
            ("registry_check_id", registry_id);
          ]
      in

      let semgrep_url = !Semgrep_envvars.v.semgrep_url in
      match Semgrep_App.upload_rule_to_registry caps request_json with
      | Error e ->
          Logs.err (fun m -> m "%s" e);
          false
      | Ok text ->
          let yojson = Yojson.Safe.from_string text in
          let visibility_str =
            Publish_CLI.string_of_visibility conf.visibility
          in
          (match conf.visibility with
          | Publish_CLI.Public ->
              Logs.app (fun m ->
                  m "    Pull request created for this public rule at: %s"
                    Yojson.Safe.Util.(member "pr_url" yojson |> to_string))
          | Unlisted ->
              Logs.app (fun m ->
                  m "    Published %s rule at %s/s/%s" visibility_str
                    (Uri.to_string semgrep_url)
                    Yojson.Safe.Util.(member "id" yojson |> to_string))
          | _ ->
              Logs.app (fun m ->
                  m "    Published %s rule at %s/r/%s" visibility_str
                    (Uri.to_string semgrep_url)
                    Yojson.Safe.Util.(member "path" yojson |> to_string)));
          true)
  | _ ->
      Logs.err (fun m ->
          m
            "    Rule contains more than one rule: only yaml files with a \
             single can be published");
      false

(*****************************************************************************)
(* Main logic *)
(*****************************************************************************)

let run_conf (caps : < caps ; .. >) (conf : Publish_CLI.conf) : Exit_code.t =
  CLI_common.setup_logging ~force_color:false ~level:conf.common.logging_level;
  let settings = Semgrep_settings.load () in
  match settings.Semgrep_settings.api_token with
  | Some token -> (
      let config_filenames, config_test_filenames =
        get_test_code_for_config (Fpath.v conf.upload_target)
      in

      match (config_filenames, conf.visibility) with
      | [], _ ->
          Logs.err (fun m ->
              m "No valid semgrep rules found in %s" conf.upload_target);
          Exit_code.fatal ~__LOC__
      | _, Publish_CLI.Public when List.length config_filenames <> 1 ->
          Logs.err (fun m ->
              m
                "Only one public rule can be uploaded at a time: specify a \
                 single Semgrep rule");
          Exit_code.fatal ~__LOC__
      | _, Publish_CLI.Public when Option.is_none conf.registry_id ->
          Logs.err (fun m -> m "--visibility=public requires --registry-id");
          Exit_code.fatal ~__LOC__
      | _ ->
          Logs.app (fun m ->
              m "Found %d configs to publish with visibility %s"
                (List.length config_filenames)
                (Publish_CLI.string_of_visibility conf.visibility));

          let fail_count =
            List.fold_left
              (fun fail_count config_filename ->
                let test_cases =
                  match
                    List.assoc_opt config_filename config_test_filenames
                  with
                  | None -> []
                  | Some test_cases -> test_cases
                in
                Logs.app (fun m ->
                    m "--> Uploading %s (test cases: %s)"
                      (Fpath.to_string config_filename)
                      (Common2.string_of_list Fpath.to_string test_cases));
                let first_test_case =
                  match test_cases with
                  | [] -> None
                  | first :: _ -> Some first
                in
                let caps = Auth.cap_token_and_network_and_tmp token caps in
                if not (upload_rule caps config_filename conf first_test_case)
                then fail_count + 1
                else fail_count)
              0 config_filenames
          in

          if fail_count =*= 0 then Exit_code.ok ~__LOC__
          else (
            Logs.err (fun m -> m "%d rules failed to upload" fail_count);
            Exit_code.fatal ~__LOC__))
  | None ->
      Logs.err (fun m -> m "run `semgrep login` before using upload");
      Exit_code.fatal ~__LOC__

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let main (caps : < caps ; .. >) (argv : string array) : Exit_code.t =
  let conf = Publish_CLI.parse_argv argv in
  run_conf caps conf
