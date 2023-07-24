open Lsp
open Types
open Testutil
open File.Operators
module Out = Output_from_core_t
module In = Input_to_core_t

(** Try to test all of the more complex parts of the LS, but save the e2e stuff
    for the python side as testing there is easier *)

let mock_session () =
  let mock_config = Runner_config.default in
  let capabilities = ServerCapabilities.create () in
  let session = Session.create capabilities mock_config in
  session

let set_session_targets (session : Session.t) files =
  let target_mappings =
    Common.map
      (fun file ->
        { In.path = file; language = L (Python, []); rule_nums = [] })
      files
  in
  let targets : In.targets = { target_mappings; rule_ids = [] } in
  let target_source = Some (Runner_config.Targets targets) in
  let config = { session.config with target_source } in
  { session with config }

let mock_run_results (files : string list) : Pattern_match.t list * Rule.t list
    =
  let pattern_string = "print(...)" in
  let lang = Lang.Python in
  let fk = Tok.unsafe_fake_tok "" in
  let xlang = Xlang.L (lang, []) in
  let pattern = Parse_pattern.parse_pattern lang pattern_string in
  let xpat = Xpattern.mk_xpat (Xpattern.Sem (lazy pattern, lang)) in
  let xpat = xpat (pattern_string, fk) in
  let rule = Rule.rule_of_xpattern xlang xpat in
  let rule = { rule with id = (Rule_ID.of_string "print", fk) } in
  let rule_id =
    {
      Pattern_match.id = fst rule.id;
      message = rule.message;
      languages = [ lang ];
      fix = None;
      pattern_string;
    }
  in
  let match_of_file file =
    let range_loc : Tok.location * Tok.location =
      ( { str = ""; pos = { charpos = 0; line = 1; column = 0; file } },
        {
          str = "";
          pos =
            {
              charpos = String.length "print(\"hello world\")";
              line = 1;
              column = 0;
              file;
            };
        } )
    in
    {
      Pattern_match.rule_id;
      file;
      range_loc;
      tokens = Lazy.from_val [];
      env = [];
      taint_trace = None;
      engine_kind = Pattern_match.OSS;
    }
  in
  let matches = Common.map match_of_file files in
  (matches, [ rule ])

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
  if git then Git_wrapper.init workspace |> ignore;
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

let session_targets () =
  let test_session files expected workspace_folders only_git_dirty =
    let session = mock_session () in
    let session = { session with only_git_dirty; workspace_folders } in
    let session = set_session_targets session files in
    let { In.target_mappings; _ } = Session.targets session in
    let targets = Common.map (fun target -> target.In.path) target_mappings in
    let targets = Common.sort targets in
    let expected = Common.sort expected in
    Alcotest.(check (list string)) "targets" expected targets
  in
  let test_session_basic git only_git_dirty () =
    let workspace = mock_workspace ~git () in
    let file1 = add_file ~git workspace () in
    let file2 = add_file workspace () in
    let files = [ file1; file2 ] in
    let expected = files in
    test_session files expected [ workspace ] only_git_dirty
  in
  let test_git_dirty () =
    let workspace = mock_workspace ~git:true () in
    let file1 = add_file ~git:true workspace () in
    let file2 = add_file ~git:true ~dirty:true workspace () in
    let file3 = add_file workspace () in
    let files = [ file1; file2; file3 ] in
    let expected = [ file2; file3 ] in
    test_session files expected [ workspace ] true
  in
  let test_multi_workspaces only_git_dirty () =
    let workspace1 = mock_workspace ~git:true () in
    let workspace2 = mock_workspace ~git:true () in
    let file1 = add_file ~git:true ~dirty:true workspace1 () in
    let file2 = add_file ~git:true ~dirty:true workspace2 () in
    let file3 = add_file ~git:true ~dirty:true workspace2 () in
    let files = [ file1; file2; file3 ] in
    let expected = files in
    test_session files expected [ workspace1; workspace2 ] only_git_dirty
  in
  let test_multi_some_dirty only_git_dirty () =
    let workspace1 = mock_workspace ~git:true () in
    let workspace2 = mock_workspace ~git:false () in
    let file1 = add_file ~git:true ~dirty:true workspace1 () in
    let file2 = add_file ~git:false workspace2 () in
    let file3 = add_file ~git:false workspace2 () in
    let files = [ file1; file2; file3 ] in
    let expected = [ file1; file2; file3 ] in
    test_session files expected [ workspace1; workspace2 ] only_git_dirty
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
    let matches, rules = mock_run_results files in
    let hrules = Rule.hrules_of_rules rules in
    let matches, _ =
      Processed_run.of_matches ~only_git_dirty matches hrules
        (File.Path.of_strings files)
    in
    let final_files =
      matches
      |> Common.map (fun ((m, _) : Processed_run.t) -> m.Out.location.path)
    in
    let final_files = Common.sort final_files in
    let expected = Common.sort expected in
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

let tests =
  pack_suites "Language Server" [ session_targets (); processed_run () ]
