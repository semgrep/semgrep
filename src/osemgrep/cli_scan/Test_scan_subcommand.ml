(* Yoann Padioleau
 *
 * Copyright (C) 2024 Semgrep, Inc.
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

open Printf

let t = Testo.create

module F = Testutil_files

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Testing end-to-end (e2e) the scan subcommand.
 *
 * Note that we already have lots of e2e pytest tests for the scan command, but
 * here we add a few tests using Testo and testing just osemgrep. Indeed,
 * in the past we had osemgrep regressions that could not be catched by our
 * pytests because many of those pytests are still marked as @osemfail and
 * so do not exercise osemgrep.
 *
 * This is similar to part of cli/tests/e2e/test_output.py
 * LATER: we should port all of test_output.py to Testo in this file.
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
(* coupling: similar to cli/tests/.../rules/eqeq-basic.yaml *)
let eqeq_basic_content =
  {|
rules:
  - id: eqeq-bad
    patterns:
      - pattern: $X == $X
    message: "useless comparison"
    languages: [python]
    severity: ERROR
|}

(* coupling: similar to cli/tests/.../targets/basic/stupid.py *)
let stupid_py_content = {|
def foo(a, b):
    return a + b == a + b
|}

let dummy_app_token = "FAKETESTINGAUTHTOKEN"

(* coupling: subset of cli/tests/conftest.py ALWAYS_MASK *)
let normalize =
  [
    Testutil_logs.mask_time;
    Testutil.mask_temp_paths ();
    Testutil_git.mask_temp_git_hash;
    Testo.mask_line ~after:"Semgrep version: " ();
  ]

let without_settings f =
  Semgrep_envvars.with_envvar "SEMGREP_SETTINGS_FILE" "nosettings.yaml" f

let with_env_app_token ?(token = dummy_app_token) f =
  Semgrep_envvars.with_envvar "SEMGREP_APP_TOKEN" token f

(*****************************************************************************)
(* Tests *)
(*****************************************************************************)

let test_nosettings_no_env () =
  without_settings (fun () ->
      printf "cwd: %s\n%!" (Unix.getcwd ());
      let settings_opt = Semgrep_settings.from_file () in
      Alcotest.(check bool) "no settings from file" true (settings_opt = None);
      let settings = Semgrep_settings.load () in
      Alcotest.(check bool)
        "default settings loaded" true
        (settings = Semgrep_settings.default))

let test_nosettings_with_env () =
  without_settings (fun () ->
      with_env_app_token (fun () ->
          printf "cwd: %s\n%!" (Unix.getcwd ());
          let settings_opt = Semgrep_settings.from_file () in
          Alcotest.(check bool)
            "no settings from file" true (settings_opt = None);
          let settings = Semgrep_settings.load () in
          let default_with_app_token =
            {
              Semgrep_settings.default with
              api_token = Some (Auth.unsafe_token_of_string dummy_app_token);
            }
          in
          Alcotest.(check bool)
            "default settings loaded with app token" true
            (settings = default_with_app_token);
          let settings_with_no_include_env =
            Semgrep_settings.load ~include_env:false ()
          in
          Alcotest.(check bool)
            "default settings loaded with app token and no env" true
            (settings_with_no_include_env = Semgrep_settings.default)))

let test_basic_output (caps : Scan_subcommand.caps) : Testo.t =
  t ~checked_output:(Testo.stdxxx ()) ~normalize __FUNCTION__ (fun () ->
      Logs.app (fun m -> m "Snapshot for %s" __FUNCTION__);
      let repo_files =
        [
          F.File ("rules.yml", eqeq_basic_content);
          F.File ("stupid.py", stupid_py_content);
        ]
      in
      Testutil_git.with_git_repo ~verbose:true repo_files (fun _cwd ->
          let exit_code =
            without_settings (fun () ->
                Scan_subcommand.main caps
                  [|
                    "semgrep-scan"; "--experimental"; "--config"; "rules.yml";
                  |])
          in
          Exit_code.Check.ok exit_code))

let test_basic_verbose_output (caps : Scan_subcommand.caps) : Testo.t =
  t ~checked_output:(Testo.stdxxx ()) ~normalize __FUNCTION__ (fun () ->
      Logs.app (fun m -> m "Snapshot for %s" __FUNCTION__);
      let repo_files =
        [
          F.File ("rules.yml", eqeq_basic_content);
          F.File ("stupid.py", stupid_py_content);
        ]
      in
      Testutil_git.with_git_repo ~verbose:true repo_files (fun _cwd ->
          let exit_code =
            without_settings (fun () ->
                Scan_subcommand.main caps
                  [|
                    "semgrep-scan";
                    "--experimental";
                    "--config";
                    "rules.yml";
                    "--verbose";
                  |])
          in
          Exit_code.Check.ok exit_code))

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let tests (caps : < Scan_subcommand.caps >) =
  Testo.categorize "Osemgrep Scan (e2e)"
    [
      t "no semgrep settings file" test_nosettings_no_env;
      t "no semgrep settings file with env" test_nosettings_with_env;
      test_basic_output caps;
      test_basic_verbose_output caps;
    ]
