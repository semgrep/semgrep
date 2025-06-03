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

let t = Testo.create ?skipped:Testutil.skip_on_windows

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

type _ Effect.t += Return of string

(*****************************************************************************)
(* Tests *)
(*****************************************************************************)
(* LATER: the pysemgrep testing code is far more compact than this. Would
 * be good to improve Http_mock_client to get some of the mock features
 * of the python requests package and to have a run_semgrep() wrapper like in
 * conftest.py.
 *)
(* NOTE: This test relies on the ability to perform a non-local jump out of the
   CI code back to the test, and then terminate the test afterwards. Using an
   exception here is fundamentally incompatible with the use of exceptions for
   errors in our networking library. Doing so happened to work previously
   because of a subtlety in when exceptions were raised given Lwt in the
   networking code, making the `Return` exception used by this test unhandled.
   However since now we don't wrap our single response in a stream object, we
   now need to have the pre-existing blanket handler to cover the cases a
   comment remarks on (since we don't know what exceptions we might need to
   handle due to a lack of documentation on the possibilities). This results in
   the networking components handling the exception the test defines, and
   therefore breaking the test. This changes the test to instead perform an
   effect to get this non-local jump, which works since there is no effect
   handler. Fundamentally this is prone to the same issue: we just don't have
   blanket effect handlers. Longer-term this test should be re-written in a way
   which it doesn't rely on running 1/2 of something and then making some
   non-local jump before terminating. *)
let test_sms_scan_id (caps : Ci_subcommand.caps) =
  t "sms_scan_id e2e from env to scan request field" (fun () ->
      (* the network mock *)
      let make_response_fn (req : Cohttp.Request.t) (body : Cohttp_lwt.Body.t) =
        match Uri.path (Cohttp.Request.uri req) with
        | "/api/cli/scans" ->
            Http_mock_client.check_method `POST req.meth;
            Logs.debug (fun m -> m "request = %s" (Dumper.dump req));
            let%lwt body = Cohttp_lwt.Body.to_string body in
            (* we abort the ci command so we can inspect the scan_request
             * further below
             *)
            Effect.perform (Return body)
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
                      Effect.Shallow.(
                        continue_with
                          (fiber (fun () ->
                               Ci_subcommand.main caps
                                 [| "semgrep-ci"; "--experimental" |]))
                          ()
                          {
                            retc =
                              (fun _ ->
                                failwith
                                  "the make_response_fn should have performed \
                                   Return");
                            exnc =
                              (fun _ ->
                                failwith
                                  "the make_response_fn should have perform \
                                   Return before raising");
                            effc =
                              (function
                              | Return s ->
                                  Some
                                    (fun _ ->
                                      let scan : Out.scan_request =
                                        Out.scan_request_of_string s
                                      in
                                      (* similar to Unit_ci.ml sms_scan_id assert *)
                                      match scan with
                                      | {
                                       scan_metadata =
                                         { sms_scan_id = Some str; _ };
                                       _;
                                      } ->
                                          Alcotest.(check string)
                                            "checking sms_scan_id" sms_scan_id
                                            str;
                                          ()
                                      | _ ->
                                          failwith
                                            (spf "wrong scan request: %s" s))
                              | _ -> None);
                          })))))
        ())

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let tests (caps : < Ci_subcommand.caps >) =
  Testo.categorize "Osemgrep ci (e2e)" [ test_sms_scan_id caps ]
