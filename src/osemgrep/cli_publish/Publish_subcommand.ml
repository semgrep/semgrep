open Common
module Out = Semgrep_output_v1_t
module Http_helpers = Http_helpers.Make (Lwt_platform)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
   Parse a semgrep-publish command, execute it and exit.

   Translated from publish.py
 *)

(*****************************************************************************)
(* Test helpers *)
(*****************************************************************************)

(* These aren't technically from publish.py, but from test.py, which is not
   yet ported.
   I have ported these individually, which could be used in the future when
   we port test.py
*)
let fixtest_suffix = ".fixed"
let yml_extensions = [ ".yml"; ".yaml" ]
let yml_test_suffixes = Common.map (fun ext -> ".test" ^ ext) yml_extensions

(* old: was a thing where we split up the file exts into a suffix list, then
   compared other suffix lists
   seems easier to just keep the ext as a string and check if it ends with
   the proper suffix
*)
let is_config_fixtest_suffix path =
  Fpath.basename path |> String.ends_with ~suffix:fixtest_suffix

let is_config_test_suffix path =
  let name = Fpath.basename path in
  List.exists (fun suffix -> String.ends_with ~suffix name) yml_test_suffixes
  && not (is_config_fixtest_suffix path)

let is_config_suffix path =
  let name = Fpath.basename path in
  List.exists (fun suffix -> String.ends_with ~suffix name) yml_extensions
  && (not (is_config_fixtest_suffix path))
  && not (is_config_test_suffix path)

(* Brandon: I don't really understand what this code is really for. *)
let relatively_eq parent_target target parent_config config =
  match
    ( Fpath.relativize ~root:parent_target target,
      Fpath.relativize ~root:parent_config config )
  with
  | Some rel1, Some rel2 ->
      let l1 = Fpath.segs rel1 in
      let l2 = Fpath.segs rel2 in
      let s = List.length l2 in
      if List.length l1 < s then false
      else
        let s' = s - 1 in
        (* now l1 must be equal or greater in length to l2
           l1: x1 x2 ... xs xs+1 ... xn
           l2: y1 y2 ... ys

           we would like to check that all elements up to the sth index (exclusive)
           are equal

           and that xs and ys are the same, modulo suffix
        *)
        List.equal ( = ) (Common.take s' l1) (Common.take s' l2)
        && Fpath.equal
             (List.nth l1 s' |> Fpath.v |> Fpath.rem_ext ~multi:true)
             (List.nth l2 s' |> Fpath.v |> Fpath.rem_ext ~multi:true)
  | _ -> false

let get_config_filenames original_config =
  if Common2.is_file (Fpath.to_string original_config) then [ original_config ]
  else
    let configs =
      Common2.(glob (spf "%s/**" (Fpath.to_string original_config)))
    in
    configs
    |> Common.map_filter (fun file ->
           let fpath = Fpath.v file in
           if
             is_config_suffix fpath
             && (not (String.starts_with ~prefix:"." (Fpath.basename fpath)))
             && not
                  (String.starts_with ~prefix:"."
                     (Fpath.basename (Fpath.parent fpath)))
           then Some fpath
           else None)

let get_config_test_filenames original_config configs original_target =
  if
    Common2.is_file (Fpath.to_string original_config)
    && Common2.is_file (Fpath.to_string original_target)
  then [ (original_config, [ original_target ]) ]
  else
    let targets =
      (if Common2.is_file (Fpath.to_string original_target) then
         Common2.(
           glob (spf "%s/**" (Fpath.to_string (Fpath.parent original_target))))
       else Common2.(glob (spf "%s/**" (Fpath.to_string original_target))))
      |> Common.map Fpath.v
    in

    let target_matches_config target config =
      let correct_suffix =
        (is_config_test_suffix target || not (is_config_suffix target))
        && not (is_config_fixtest_suffix target)
      in
      (Fpath.is_file_path original_target
      || relatively_eq original_target target original_config config)
      && Fpath.is_file_path target && correct_suffix
    in

    Common.map
      (fun config ->
        ( config,
          List.filter
            (fun target -> target_matches_config target config)
            targets ))
      configs

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let get_test_code_for_config target =
  let config_filenames =
    match get_config_filenames target with
    | [] -> [ target ]
    | other -> other
  in
  let config_test_filenames =
    get_config_test_filenames target config_filenames target
  in
  (config_filenames, config_test_filenames)

let upload_rule token rule_file (conf : Publish_CLI.conf) test_code_file =
  let rule_file = Fpath.to_string rule_file in
  (* THINK: is this the same as get_config(...).get_rules()? *)
  let rules, errors =
    try
      let rules, errors =
        Rule_fetching.rules_from_rules_source ~token_opt:(Some token)
          ~rewrite_rule_ids:true ~registry_caching:false
          (Rules_source.Configs [ rule_file ])
        |> Rule_fetching.partition_rules_and_errors
      in
      ( rules,
        Common.map
          (fun ((_, rule_id, _) as err) ->
            Rule.{ rule_id = Some rule_id; kind = InvalidRule err })
          errors )
    with
    (* TODO: Why is this needed? This exception should have been handled and
       converted at `Parse_rule` time...
    *)
    | Parsing_error.Other_error (s, t) ->
        ([], [ { rule_id = None; kind = InvalidYaml (s, t) } ])
    | Rule.Error error -> ([], [ error ])
  in

  match (errors, rules) with
  | _ :: _, _ ->
      Logs.err (fun m ->
          m "    Invalid rule definition: %s is invalid: %s" rule_file
            (errors |> Common.map Rule.string_of_error |> String.concat ", "));
      false
  | _, [ rule ] -> (
      (* TODO: This emits a "fatal: No remote configured to list refs from."
         when run from a non git repository, even though in this command, it
         shouldn't be fatal and is handled.
         This output needs to be suppressed.
      *)
      let project_url =
        match Git_wrapper.get_project_url () with
        | None -> "<none>"
        | Some url -> url
      in
      let rule_source = File.read_file (Fpath.v rule_file) in
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
          match Xlang.to_langs rule.target_analyzer with
          | [] -> `Null
          | lang :: _ -> `String (Lang.to_string lang)
        in
        let deployment_id =
          match Semgrep_App.get_deployment_from_token ~token with
          | None -> `Null
          | Some config -> `Int config.id
        in
        let test_target =
          match test_code_file with
          | None -> `Null
          | Some file -> `String (File.read_file file)
        in
        let registry_id =
          match conf.registry_id with
          | None -> `Null
          | Some registry_id -> `String registry_id
        in
        (* TODO *)
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

      (* THINK: is this how we get the semgrep URL? *)
      let semgrep_url = !Semgrep_envvars.v.semgrep_url in
      let uri = Uri.(with_path semgrep_url "api/registry/rules") in
      let body = JSON.string_of_json (JSON.from_yojson request_json) in
      let headers =
        [
          ("Content-Type", "application/json");
          ("User-Agent", Fmt.str "Semgrep/%s" Version.version);
          ("Authorization", "Bearer " ^ token);
        ]
      in
      match Http_helpers.post ~body ~headers uri with
      | Error (status_code, text) ->
          Logs.err (fun m ->
              m "    Failed to upload rule with status code %d" status_code);
          Logs.err (fun m -> m "%s" text);
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

let run_conf (conf : Publish_CLI.conf) : Exit_code.t =
  let settings = Semgrep_settings.load () in
  match settings.Semgrep_settings.api_token with
  | Some token -> (
      let config_filenames, config_test_filenames =
        get_test_code_for_config (Fpath.v conf.target)
      in

      match (config_filenames, conf.visibility) with
      | [], _ ->
          Logs.err (fun m -> m "No valid semgrep rules found in %s" conf.target);
          Exit_code.fatal
      | _, Publish_CLI.Public when List.length config_filenames <> 1 ->
          Logs.err (fun m ->
              m
                "Only one public rule can be uploaded at a time: specify a \
                 single Semgrep rule");
          Exit_code.fatal
      | _, Publish_CLI.Public when Option.is_none conf.registry_id ->
          Logs.err (fun m -> m "--visibility=public requires --registry-id");
          Exit_code.fatal
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

                if not (upload_rule token config_filename conf first_test_case)
                then fail_count + 1
                else fail_count)
              0 config_filenames
          in

          if fail_count =*= 0 then Exit_code.ok
          else (
            Logs.err (fun m -> m "%d rules failed to upload" fail_count);
            Exit_code.fatal))
  | _ ->
      Logs.err (fun m -> m "run `semgrep login` before using upload");
      Exit_code.fatal

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let main (argv : string array) : Exit_code.t =
  let conf = Publish_CLI.parse_argv argv in
  run_conf conf
