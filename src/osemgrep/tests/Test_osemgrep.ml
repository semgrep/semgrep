(* Yoann Padioleau
 *
 * Copyright (C) 2023 Semgrep, Inc.
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
open Common
module TL = Test_login_subcommand

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Testing combinations of multiple subcommands (e.g., login and scan).
 *
 * Many of those tests are slow because they interact for real with our
 * registry.
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*****************************************************************************)
(* Tests *)
(*****************************************************************************)

(* no need for a token to access public rules in the registry *)
let test_scan_config_registry_no_token (caps : CLI.caps) =
  Testo.create __FUNCTION__ (fun () ->
      Testutil_files.with_tempdir ~chdir:true (fun _tmp_path ->
          let exit_code =
            CLI.main caps
              [|
                "semgrep";
                "scan";
                "--experimental";
                "--debug";
                "--config";
                "r/python.lang.correctness.useless-eqeq.useless-eqeq";
              |]
          in
          assert (exit_code =*= Exit_code.ok)))

(* Remaining part of test_login.py (see also Test_login_subcommand.ml) *)
let test_scan_config_registry_with_invalid_token caps : Testo.test =
  Testo.create ~checked_output:Stderr __FUNCTION__
    ~normalize:[ Testo.mask_not_substrings [ "Saved access token" ] ]
    (TL.with_login_test_env (fun () ->
         Semgrep_envvars.with_envvar "SEMGREP_APP_TOKEN" TL.fake_token
           (fun () ->
             TL.with_fake_deployment_response TL.fake_deployment (fun () ->
                 (* log back in *)
                 (* we're not calling CLI.main() because it would also do
                  * some metrics call, so simpler to call directly
                  * Login_subcommand.
                  *)
                 let exit_code =
                   Login_subcommand.main
                     (caps :> < Cap.stdout ; Cap.network >)
                     [| "semgrep-login" |]
                 in
                 assert (exit_code =*= Exit_code.ok)));

         (* Even if we are allowed to login with a fake token (because
          * of the with_fake_deployment_response), outside of it
          * we can't use the registry with an invalid token.
          *
          * alt: call CLI.main, but that would require to intercept
          * the regular output of the program as CLI.main intercept
          * exn in CLI.safe_run and transform them in output.
          * TODO: test_login.py assert exit_code == 7
          *)
         try
           Scan_subcommand.main caps
             [|
               "semgrep-scan";
               "--experimental";
               "--config";
               "r/python.lang.correctness.useless-eqeq.useless-eqeq";
             |]
           |> ignore;
           failwith "scan should fail when the token is invalid"
         with
         | Error.Semgrep_error (msg, _) ->
             (* we got the exn as intended, good *)
             Alcotest.(check string)
               __LOC__
               {|Failed to download config from https://semgrep.dev/c/r/python.lang.correctness.useless-eqeq.useless-eqeq: HTTP GET failed: 401 Unauthorized:
{"error":"Invalid Authorization"}|}
               msg;
             ()))

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let tests (caps : CLI.caps) =
  Testo.categorize "Osemgrep (e2e)"
    [
      test_scan_config_registry_no_token caps;
      test_scan_config_registry_with_invalid_token
        (caps :> < Cap.stdout ; Cap.network ; Cap.tmp >);
    ]
