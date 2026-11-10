(*
   Copyright (c) 2025 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
(* Unit tests for the UCmd module *)

let test_quote_command_for_bash () =
  let args = [ "ls"; ""; "a b"; "a>b"; "AB"; "42"; "_.-"; "do"; "time" ] in
  let cmd_str = UCmd.quote_command_for_bash args in
  Alcotest.(check string)
    "equal" "ls '' 'a b' 'a>b' AB 42 _.- 'do' time" cmd_str

let skipped = if Sys.win32 then Some "OS-dependent quoting" else None
let skipped_win32 = if Sys.win32 then Some "requires Unix shell" else None
let windows_only = if Sys.win32 then None else Some "Windows-only"

let resolve ?(files = []) ~path name =
  UCmd.resolve_windows_program ~path ~is_file:(fun p -> List.mem p files) name

let check_resolved msg expected actual =
  Alcotest.(check (option string)) msg expected actual

let test_resolve_path_entries () =
  let files =
    [
      ".\\git.exe";
      "rel\\git.exe";
      "C:rel\\git.exe";
      "\\root\\git.exe";
      "ab:\\x\\git.exe";
      "C:\\Program Files\\Git\\cmd\\git.exe";
      "\\\\server\\share\\bin\\git.exe";
      "\\\\?\\C:\\tools\\git.exe";
      "C:\\tools\\git.exe";
      "C:\\a;b\\git.exe";
    ]
  in
  [
    ( "skips non-fully-qualified entries",
      [
        "";
        ".";
        "rel";
        "C:rel";
        "\\root";
        "ab:\\x";
        "\"C:\\Program Files\\Git\\cmd\"";
      ],
      Some "C:\\Program Files\\Git\\cmd\\git.exe" );
    ( "UNC",
      [ "\\\\server\\share\\bin\\" ],
      Some "\\\\server\\share\\bin\\git.exe" );
    ("extended-length", [ "\\\\?\\C:\\tools" ], Some "\\\\?\\C:\\tools\\git.exe");
    ("drive with /", [ "C:/tools" ], Some "C:\\tools\\git.exe");
    ("quoted ;", [ "\"C:\\a;b\"" ], Some "C:\\a;b\\git.exe");
  ]
  |> List.iter (fun (msg, entries, expected) ->
      check_resolved msg expected
        (resolve ~files ~path:(Some (String.concat ";" entries)) "git"))

let test_resolve_suffix_and_missing () =
  let files = [ "C:\\bin\\python3.11"; "C:\\bin\\yarn.cmd" ] in
  let path = Some "C:\\bin" in
  check_resolved "extension kept" (Some "C:\\bin\\python3.11")
    (resolve ~files ~path "python3.11");
  check_resolved "no PATHEXT" None (resolve ~files ~path "yarn");
  check_resolved "no PATH" None (resolve ~files:[ "git.exe" ] ~path:None "git")

let test_resolve_explicit_paths () =
  [ ".\\gradlew.bat"; "venv/bin/pip"; "C:git" ]
  |> List.iter (fun name ->
      check_resolved name (Some name) (resolve ~path:None name))

let system_root () =
  Option.value ~default:"C:\\Windows" (Sys.getenv_opt "SystemRoot")

let cmd_exe () = Fpath.(v (system_root ()) / "System32" / "cmd.exe")

(* A copy of cmd.exe in the current directory must not run via a bare name. *)
let test_windows_no_cwd_lookup () =
  Testutil_files.with_tempdir ~chdir:true (fun dir ->
      let planted = "semgrep-ucmd-planted" in
      Testutil_files.copy ~src:(cmd_exe ())
        ~dst:Fpath.(dir / (planted ^ ".exe"));
      let cmd = (Cmd.Name planted, [ "/c"; "exit 7" ]) in
      let check_error api = function
        | Error (`Msg msg) ->
            if
              (not (String_.contains ~term:planted msg))
              || String_.contains ~term:(Fpath.to_string dir) msg
            then Alcotest.failf "%s: unexpected error: %s" api msg
        | Error _ -> Alcotest.failf "%s: unexpected error" api
        | Ok _ -> Alcotest.failf "%s ran the planted program" api
      in
      check_error "run_subprocess" (UCmd.run_subprocess cmd);
      check_error "string_of_run" (UCmd.string_of_run ~trim:true cmd);
      check_error "string_of_run_with_stderr"
        (fst (UCmd.string_of_run_with_stderr ~trim:true cmd));
      check_error "lines_of_run" (UCmd.lines_of_run ~trim:true cmd);
      check_error "status_of_run" (UCmd.status_of_run ~quiet:true cmd);
      (* Rejection only: eio_windows has no process manager. *)
      Eio_main.run (fun env ->
          check_error "string_of_run_with_timeout"
            (fst
               (UCmd.string_of_run_with_timeout
                  (env :> UCmd.eio_env)
                  ~timeout_seconds:10. ~trim:true cmd)));
      match UCmd.cmd_to_list (planted ^ " /c exit 0") with
      | exception Failure _ -> ()
      | _ -> Alcotest.fail "cmd_to_list ran a shell command")

(* PATH lookup and explicit paths, e.g. build wrappers, still work, and the
   current directory is not consulted. *)
let test_windows_path_and_explicit () =
  Testutil_files.with_tempdir ~chdir:true (fun dir ->
      let local = "semgrep-ucmd-local.exe" in
      Testutil_files.copy ~src:(cmd_exe ()) ~dst:Fpath.(dir / local);
      let check_exit name code =
        match
          UCmd.status_of_run ~quiet:true
            (Cmd.Name name, [ "/c"; Printf.sprintf "exit %d" code ])
        with
        | Ok (`Exited n) when n = code -> ()
        | Ok _ -> Alcotest.failf "%s: unexpected status" name
        | Error (`Msg msg) -> Alcotest.failf "%s: %s" name msg
      in
      check_exit "cmd" 3;
      check_exit (".\\" ^ local) 4;
      check_exit (Fpath.to_string Fpath.(dir / local)) 5;
      (* The parent's PATH selects the program; the child gets its own env. *)
      let mkdir name =
        let d = Fpath.(dir / name) in
        Testutil_files.mkdir d;
        d
      in
      let bad = mkdir "bad" and good = mkdir "good" in
      let first = "semgrep-ucmd-first" and tool = "semgrep-ucmd-tool" in
      let bad_first = Fpath.(bad / (first ^ ".exe")) in
      UFile.write_file ~file:bad_first "no program";
      UFile.write_file ~file:Fpath.(dir / (tool ^ ".exe")) "no program";
      Testutil_files.copy ~src:(cmd_exe ()) ~dst:Fpath.(good / (first ^ ".exe"));
      Testutil_files.copy ~src:(cmd_exe ()) ~dst:Fpath.(good / (tool ^ ".exe"));
      let old_path = Sys.getenv_opt "PATH" |> Option.value ~default:"" in
      Unix.putenv "PATH"
        (String.concat ";"
           [ Fpath.to_string bad; Fpath.to_string good; old_path ]);
      Common.protect
        ~finally:(fun () -> Unix.putenv "PATH" old_path)
        (fun () ->
          let child_path = "C:\\semgrep-child-path" in
          let env =
            Cmd.env_of_list ~inherit_parent_env:false
              [ ("PATH", child_path); ("SystemRoot", system_root ()) ]
          in
          (match
             UCmd.string_of_run ~trim:true ~env
               (Cmd.Name tool, [ "/c"; "echo %PATH%" ])
           with
          | Ok (out, (_, `Exited 0)) ->
              Alcotest.(check string) "child PATH" child_path out
          | Ok _ -> Alcotest.fail "unexpected status"
          | Error (`Msg msg) -> Alcotest.fail msg);
          (* An invalid first match fails without trying later entries. *)
          match
            UCmd.status_of_run ~quiet:true (Cmd.Name first, [ "/c"; "exit 0" ])
          with
          | Error (`Msg _) -> ()
          | Ok _ -> Alcotest.fail "ran a later PATH match"))

let tests =
  [
    Testo.create "quote command for bash" ?skipped test_quote_command_for_bash;
    (* The lookup uses Fpath, whose path semantics depend on the platform. *)
    Testo.create ?skipped:windows_only "Windows lookup: PATH entries"
      test_resolve_path_entries;
    Testo.create ?skipped:windows_only
      "Windows lookup: suffixes and missing programs"
      test_resolve_suffix_and_missing;
    Testo.create ?skipped:windows_only "Windows lookup: explicit paths"
      test_resolve_explicit_paths;
    Testo.create ?skipped:windows_only "Windows: no current-directory lookup"
      test_windows_no_cwd_lookup;
    Testo.create ?skipped:windows_only "Windows: PATH and explicit programs run"
      test_windows_path_and_explicit;
    Testo.create ?skipped:skipped_win32
      "string_of_run_with_timeout completes within timeout" (fun () ->
        Eio_main.run @@ fun env ->
        let eio_env = (env :> UCmd.eio_env) in
        let cmd = (Cmd.Name "sh", [ "-c"; "echo hello" ]) in
        let result, stderr =
          UCmd.string_of_run_with_timeout eio_env ~timeout_seconds:10.
            ~trim:true cmd
        in
        Alcotest.(check string) "stderr is empty" "" stderr;
        match result with
        | Ok (out, `Exited 0) ->
            Alcotest.(check string) "stdout is hello" "hello" out
        | Ok (_, status) ->
            Alcotest.failf "unexpected exit status: %s"
              (match status with
              | `Exited n -> Printf.sprintf "exited %d" n
              | `Signaled n -> Printf.sprintf "signaled %d" n)
        | Error (`Msg msg) -> Alcotest.failf "unexpected error: %s" msg
        | Error `Timeout -> Alcotest.fail "unexpected timeout");
    Testo.create ?skipped:skipped_win32
      "string_of_run_with_timeout captures non-zero exit" (fun () ->
        Eio_main.run @@ fun env ->
        let eio_env = (env :> UCmd.eio_env) in
        let cmd = (Cmd.Name "sh", [ "-c"; "echo oops; exit 42" ]) in
        let result, _stderr =
          UCmd.string_of_run_with_timeout eio_env ~timeout_seconds:10.
            ~trim:true cmd
        in
        match result with
        | Ok (out, `Exited 42) ->
            Alcotest.(check string) "stdout is oops" "oops" out
        | Ok (_, status) ->
            Alcotest.failf "unexpected exit status: %s"
              (match status with
              | `Exited n -> Printf.sprintf "exited %d" n
              | `Signaled n -> Printf.sprintf "signaled %d" n)
        | Error (`Msg msg) -> Alcotest.failf "unexpected error: %s" msg
        | Error `Timeout -> Alcotest.fail "unexpected timeout");
    Testo.create ?skipped:skipped_win32
      "string_of_run_with_timeout kills slow process" (fun () ->
        Eio_main.run @@ fun env ->
        let eio_env = (env :> UCmd.eio_env) in
        let cmd = (Cmd.Name "sh", [ "-c"; "sleep 60" ]) in
        let result, _stderr =
          UCmd.string_of_run_with_timeout eio_env ~timeout_seconds:0.1
            ~trim:true cmd
        in
        match result with
        | Error `Timeout -> ()
        | Ok (_, status) ->
            Alcotest.failf "expected timeout but got exit status: %s"
              (match status with
              | `Exited n -> Printf.sprintf "exited %d" n
              | `Signaled n -> Printf.sprintf "signaled %d" n)
        | Error (`Msg msg) -> Alcotest.failf "unexpected error: %s" msg);
    Testo.create ?skipped:skipped_win32
      "string_of_run_with_timeout captures stderr" (fun () ->
        Eio_main.run @@ fun env ->
        let eio_env = (env :> UCmd.eio_env) in
        let cmd = (Cmd.Name "sh", [ "-c"; "echo out; echo err >&2" ]) in
        let result, stderr =
          UCmd.string_of_run_with_timeout eio_env ~timeout_seconds:10.
            ~trim:true cmd
        in
        Alcotest.(check string) "stderr is err" "err" stderr;
        match result with
        | Ok (out, `Exited 0) ->
            Alcotest.(check string) "stdout is out" "out" out
        | Ok (_, status) ->
            Alcotest.failf "unexpected exit status: %s"
              (match status with
              | `Exited n -> Printf.sprintf "exited %d" n
              | `Signaled n -> Printf.sprintf "signaled %d" n)
        | Error (`Msg msg) -> Alcotest.failf "unexpected error: %s" msg
        | Error `Timeout -> Alcotest.fail "unexpected timeout");
  ]
