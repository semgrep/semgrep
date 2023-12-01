open Testutil
open Fpath_.Operators
module OutJ = Semgrep_output_v1_t
module In = Input_to_core_t

(** Try to test all of the more complex parts of the LS, but save the e2e stuff
    for the python side as testing there is easier *)
(*****************************************************************************)
(* Mocks *)
(*****************************************************************************)

let checked_command cmd =
  match Bos.OS.Cmd.run_status cmd with
  | Ok (`Exited 0) -> ()
  | _ -> failwith (Common.spf "Error running cmd: %s" (Bos.Cmd.to_string cmd))

let setup_git workspace =
  Git_wrapper.init workspace;
  checked_command
    Bos.Cmd.(
      v "git" % "-C" % Fpath.to_string workspace % "config" % "user.email"
      % "baselinetest@semgrep.com");
  checked_command
    Bos.Cmd.(
      v "git" % "-C" % Fpath.to_string workspace % "config" % "user.name"
      % "Baseline Test");
  checked_command
    Bos.Cmd.(
      v "git" % "-C" % Fpath.to_string workspace % "checkout" % "-B" % "main")

let mock_session () =
  let capabilities = Lsp.Types.ServerCapabilities.create () in
  let session = Session.create capabilities in
  session

let set_session_targets (session : Session.t) folders =
  { session with workspace_folders = folders }

let mock_run_results (files : string list) : Core_runner.result =
  let pattern_string = "print(...)" in
  let lang = Lang.Python in
  let fk = Tok.unsafe_fake_tok "" in
  let xlang = Xlang.L (lang, []) in
  let pattern = Parse_pattern.parse_pattern lang pattern_string in
  let xpat = Xpattern.mk_xpat (Xpattern.Sem (lazy pattern, lang)) in
  let xpat = xpat (pattern_string, fk) in
  let rule = Rule.rule_of_xpattern xlang xpat in
  let rule = { rule with id = (Rule_ID.of_string "print", fk) } in
  let hrules = Rule.hrules_of_rules [ rule ] in
  let scanned = List_.map (fun f -> Fpath.v f) files |> Set_.of_list in
  let match_of_file file =
    let (extra : OutJ.core_match_extra) =
      {
        message = Some "test";
        metavars = [];
        dataflow_trace = None;
        fix = None;
        engine_kind = `OSS;
        validation_state = Some `No_validator;
        extra_extra = None;
        severity = None;
        metadata = None;
      }
    in
    let (m : OutJ.core_match) =
      {
        check_id = Rule_ID.of_string "print";
        (* inherited location *)
        start = { line = 1; col = 1; offset = 1 };
        end_ = { line = 1; col = 1; offset = 1 };
        path = Fpath.v file;
        extra;
      }
    in
    m
  in
  let matches = List_.map match_of_file files in
  let (core : OutJ.core_output) =
    {
      version = None;
      results = matches;
      errors = [];
      (* extra *)
      paths = { skipped = None; scanned = [] };
      skipped_rules = [];
      explanations = None;
      time = None;
      rules_by_engine = None;
      engine_requested = Some `OSS;
      (* If the engine requested is OSS, there must be no
         interfile requested languages *)
      interfile_languages_used = Some [];
    }
  in
  Core_runner.{ core; hrules; scanned }

let mock_workspace ?(git = false) () =
  let rand_dir () =
    let uuid = Uuidm.v `V4 in
    let dir_name = "test_workspace_" ^ Uuidm.to_string uuid in
    let dir = Filename.concat (Filename.get_temp_dir_name ()) dir_name in
    Unix.mkdir dir 0o777;
    dir
  in
  let workspace = rand_dir () in
  let workspace = Fpath.v workspace in
  if git then setup_git workspace |> ignore;
  workspace

let add_file ?(git = false) ?(dirty = false)
    ?(content = "print(\"hello world\")\n") workspace () =
  let file = Filename.temp_file ~temp_dir:!!workspace "test" ".py" in
  let oc = open_out_bin file in
  output_string oc content;
  close_out oc;
  if git then Git_wrapper.add workspace [ Fpath.v file ];
  if (not dirty) && git then Git_wrapper.commit workspace "test";
  file

let with_mock_envvars f () =
  (* TODO: we should simply do:
   *    Semgrep_envvars.with_envvar "SEMGREP_APP_TOKEN" "123456789" f
   * but we then get CI failures on build-js-tests
   * see https://github.com/semgrep/semgrep/pull/9285
   * because of the use of putenv in Semgrep_envvars.with_envvar,
   * even after adding a fake on in js/node_shared/unix.js
   *)
  let old_settings = !Semgrep_envvars.v in
  let new_settings = { old_settings with app_token = Some "123456789" } in
  Common.save_excursion Semgrep_envvars.v new_settings f

(*****************************************************************************)
(* Tests *)
(*****************************************************************************)

let session_targets () =
  let test_session expected workspace_folders only_git_dirty =
    let session = mock_session () in
    let user_settings = { session.user_settings with only_git_dirty } in
    let session = { session with user_settings; workspace_folders } in
    let session = set_session_targets session workspace_folders in
    let targets = session |> Session.targets |> List_.map Fpath.to_string in
    let targets = List_.sort targets in
    let expected = List_.sort expected in
    Alcotest.(check (list string)) "targets" expected targets
  in
  let test_session_basic git only_git_dirty () =
    let workspace = mock_workspace ~git () in
    let file1 = add_file ~git workspace () in
    let file2 = add_file workspace () in
    let files = [ file1; file2 ] in
    let expected = files in
    test_session expected [ workspace ] only_git_dirty
  in
  let test_git_dirty () =
    let workspace = mock_workspace ~git:true () in
    let _file1 = add_file ~git:true workspace () in
    let file2 = add_file ~git:true ~dirty:true workspace () in
    let file3 = add_file workspace () in
    let expected = [ file2; file3 ] in
    test_session expected [ workspace ] true
  in
  let test_multi_workspaces only_git_dirty () =
    let workspace1 = mock_workspace ~git:true () in
    let workspace2 = mock_workspace ~git:true () in
    let file1 = add_file ~git:true ~dirty:true workspace1 () in
    let file2 = add_file ~git:true ~dirty:true workspace2 () in
    let file3 = add_file ~git:true ~dirty:true workspace2 () in
    let expected = [ file1; file2; file3 ] in
    test_session expected [ workspace1; workspace2 ] only_git_dirty
  in
  let test_multi_some_dirty only_git_dirty () =
    let workspace1 = mock_workspace ~git:true () in
    let workspace2 = mock_workspace ~git:false () in
    let file1 = add_file ~git:true ~dirty:true workspace1 () in
    let file2 = add_file ~git:false workspace2 () in
    let file3 = add_file ~git:false workspace2 () in
    let expected = [ file1; file2; file3 ] in
    test_session expected [ workspace1; workspace2 ] only_git_dirty
  in
  let tests =
    [
      ("Test no git", test_session_basic false false);
      ("Test no git with only_git_dirty", test_session_basic false true);
      ("Test git", test_session_basic true false);
      ("Test git with dirty files", test_git_dirty);
      ( "Test multiple workspaces (only_git_dirty: true)",
        test_multi_workspaces true );
      ( "Test multiple workspaces (only_git_dirty: false)",
        test_multi_workspaces false );
      ( "Test multiple workspaces with some dirty (only_git_dirty: true)",
        test_multi_some_dirty true );
      ( "Test multiple workspaces with some dirty (only_git_dirty: false)",
        test_multi_some_dirty false );
    ]
  in
  pack_tests "Session Targets" tests

let processed_run () =
  let test_processed_run files expected only_git_dirty =
    let results = mock_run_results files in
    let matches = Processed_run.of_matches ~only_git_dirty results in
    let final_files =
      matches |> List_.map (fun (m : OutJ.cli_match) -> !!(m.path))
    in
    let final_files = List_.sort final_files in
    let expected = List_.sort expected in
    Alcotest.(check (list string)) "processed run" expected final_files
  in
  let test_processed only_git_dirty git () =
    let workspace = mock_workspace ~git () in
    let file1 = add_file ~git workspace () in
    let file2 = add_file ~git workspace () in
    let oc = open_out_gen [ Open_wronly; Open_append ] 0o666 file2 in
    output_string oc "print(\"hello world\")";
    close_out oc;
    let files = [ file1; file2 ] in
    let expected = files in
    test_processed_run files expected only_git_dirty
  in
  let test_git_dirty_lines () =
    let workspace = mock_workspace ~git:true () in
    let file1 = add_file ~git:true workspace () in
    let file2 = add_file ~git:true workspace () in
    let file3 = add_file workspace () in
    let oc = open_out_gen [ Open_wronly; Open_append ] 0o666 file1 in
    output_string oc "string =\"different line changed\"\n";
    close_out oc;
    let oc = open_out_bin file2 in
    output_string oc "print(\"hello world\") # changed line\n";
    close_out oc;
    let files = [ file1; file2; file3 ] in
    let expected = [ file2; file3 ] in
    test_processed_run files expected true
  in
  let test_nosem () =
    let workspace = mock_workspace () in
    let file1 =
      add_file ~content:"print(\"hello world\") # nosem" workspace ()
    in
    let file2 =
      add_file ~content:"# nosem\nprint(\"hello world\")" workspace ()
    in
    let file3 =
      add_file ~content:"# print(\"hello world\") # nosemgrep: print" workspace
        ()
    in
    let file4 =
      add_file ~content:"# print(\"hello world\") # nosemgrep: other_rule"
        workspace ()
    in
    let files = [ file1; file2; file3; file4 ] in
    let expected = [ file4 ] in
    test_processed_run files expected false
  in
  let tests =
    [
      ("Test no git", test_processed false false);
      ("Test git", test_processed false true);
      ("Test only git dirty with no git", test_processed true false);
      ("Test only git dirty with dirty files", test_git_dirty_lines);
      ("Test nosem", test_nosem);
    ]
  in
  pack_tests "Processed Run" tests

let ci_tests () =
  let with_ci_client =
    let make_fn (req : Cohttp.Request.t) body =
      ignore body;
      let uri = req |> Cohttp.Request.uri |> Uri.path in
      Http_mock_client.check_method `GET req.meth;
      let body =
        match uri with
        | "/api/agent/deployments/scans/config" ->
            Http_mock_client.body_of_file "./tests/ls/ci/rule_conf_resp.json"
        | "/api/agent/deployments/current" ->
            Http_mock_client.body_of_file "./tests/login/ok_response.json"
        | _ ->
            failwith (Printf.sprintf "Unexpected request to %s in CI tests" uri)
      in
      Lwt.return Http_mock_client.(basic_response body)
    in
    Http_mock_client.with_testing_client make_fn
  in
  let test_cache_session () =
    let session = mock_session () in
    Lwt_platform.run (Session.cache_session session);
    let rules = session.cached_session.rules in
    Alcotest.(check int) "rules" 1 (List.length rules);
    let skipped_fingerprints = session.cached_session.skipped_fingerprints in
    Alcotest.(check int)
      "skipped_fingerprints" 1
      (List.length skipped_fingerprints)
  in
  let tests =
    [
      ( "Test session cache",
        with_mock_envvars (with_ci_client test_cache_session) );
    ]
  in
  pack_tests "CI Tests" tests

let test_ls_libev () = Lwt_platform.set_engine ()

let libev_tests =
  pack_tests "Lib EV tests" [ ("Test LS with libev", test_ls_libev) ]

let tests =
  pack_suites "Language Server (unit)"
    [ session_targets (); processed_run (); ci_tests (); libev_tests ]
