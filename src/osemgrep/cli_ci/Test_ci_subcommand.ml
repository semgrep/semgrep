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
open Common
module Out = Semgrep_output_v1_j

let t = Testo.create

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Testing end-to-end (e2e) the ci subcommand.
 *
 * This is a very partial port of cli/tests/e2e/test_ci.py
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
let sms_scan_id = "SMS_1234"

(* copy paste of Test_login_subcommand.ml (see this file for more info) *)
let fake_token = "token1234"

let fake_deployment =
  {|
  { "deployment":
    { "id": 1234,
      "name": "deployment1234"
    }
  }
|}

exception Return of string

(*****************************************************************************)
(* Tests *)
(*****************************************************************************)
(* LATER: the pysemgrep testing code is far more compact than this. Would
 * be good to improve Http_mock_client to get some of the mock features
 * of the python requests package and to have a run_semgrep() wrapper like in
 * conftest.py.
 *)
let test_sms_scan_id (caps : Ci_subcommand.caps) =
  t "sms_scan_id e2e from env to scan request field" (fun () ->
      (* the network mock *)
      let make_response_fn (req : Cohttp.Request.t) (body : Cohttp_lwt.Body.t) =
        match Uri.path (Cohttp.Request.uri req) with
        (* step1: get deployment (Semgrep_App.deployment_route) *)
        | "/api/agent/deployments/current" ->
            Http_mock_client.check_method `GET req.meth;
            let response_body = fake_deployment |> Cohttp_lwt.Body.of_string in
            Lwt.return (Http_mock_client.basic_response response_body)
        (* step2: initiate scan (Semgrep_App.start_scan_route) *)
        | "/api/cli/scans" ->
            Http_mock_client.check_method `POST req.meth;
            Logs.debug (fun m -> m "request = %s" (Dumper.dump req));
            let%lwt body = Cohttp_lwt.Body.to_string body in
            (* we abort the ci command so we can inspect the scan_request
             * further below
             *)
            raise (Return body)
        | url -> Alcotest.fail (spf "unexpected request: %s" url)
      in
      Http_mock_client.with_testing_client make_response_fn
        (fun () ->
          Semgrep_envvars.with_envvar "SEMGREP_MANAGED_SCAN_ID" sms_scan_id
            (fun () ->
              Semgrep_envvars.with_envvar "SEMGREP_APP_TOKEN" fake_token
                (fun () ->
                  Testutil_git.with_git_repo
                    [ File ("empty", "") ]
                    (fun _dir ->
                      try
                        let _res =
                          Ci_subcommand.main caps
                            [| "semgrep-ci"; "--experimental" |]
                        in
                        failwith
                          "the make_response_fn should have thrown Return"
                      with
                      | Return s -> (
                          let scan : Out.scan_request =
                            Out.scan_request_of_string s
                          in
                          (* similar to Unit_ci.ml sms_scan_id assert *)
                          match scan with
                          | { scan_metadata = { sms_scan_id = Some str; _ }; _ }
                            ->
                              Alcotest.(check string)
                                "checking sms_scan_id" sms_scan_id str
                          | _ -> failwith (spf "wrong scan request: %s" s))))))
        ())

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let tests (caps : < Ci_subcommand.caps >) =
  Testo.categorize "Osemgrep ci (e2e)" [ test_sms_scan_id caps ]
