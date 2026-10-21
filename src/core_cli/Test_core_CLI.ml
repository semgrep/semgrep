(*
   Copyright (c) 2024-2025 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
open Common

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* End-to-end (e2e) testing of the semgrep-core CLI program
 *
 * See also tests for the semgrep-core -generate_ast_json in
 * in semgrep-interfaces/tests/test-ast run from make core-test-e2 run
 * itself from .github/workflow/test.yml
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let t = Testo.create ?skipped:Testutil.skip_on_windows

(* Mostly a copy-paste of Test_pro_core_CLI.ml *)
type exn_res = ExnExit of int

let run_main (cmd : string) : (unit, exn_res) result =
  let args = String_.split ~sep:"[ \t]+" cmd in
  try
    print_string (spf "executing: semgrep-core %s\n" cmd);
    Ok (Core_CLI.main_exn (Array.of_list ("semgrep-core" :: args)))
  with
  | Common.UnixExit (n, msg) ->
      Logs.err (fun m -> m "exn UnixExit(%d): %s" n msg);
      Error (ExnExit n)

let assert_Ok res =
  match res with
  | Ok () -> print_string "OK"
  | _ -> failwith "Not OK"

(*****************************************************************************)
(* The tests *)
(*****************************************************************************)

let semgrep_core_tests : Testo.t list =
  Testo.categorize "semgrep-core CLI (e2e)"
    [
      t "--help" (fun () ->
          match run_main "handle --help" with
          (* old: exception (Common.UnixExit 0) -> *)
          | Error (ExnExit 0) -> print_string "OK"
          | _ -> failwith "Not OK");
      (* The commit is build-dependent, so we assert against a
         reconstructed line rather than snapshot stdout, which would flap
         on every commit. *)
      t "-version reports the version and commit" (fun () ->
          let res = Testo.with_capture stdout (fun () -> run_main "-version") in
          (match fst res with
          | Error (ExnExit 0) -> ()
          | _ -> failwith "expected -version to exit with code 0");
          let expected =
            spf "semgrep-core version: %s (%s)" Version.version Git_info.commit
          in
          if String_.contains ~term:expected (snd res) then print_string "OK"
          else failwith (spf "missing %S in -version output" expected));
      t "handle -rules <rule> -l <lang> <single_file>" (fun () ->
          let cmd =
            "-rules tests/rules_v2/new_syntax.yaml -l python \
             tests/rules_v2/new_syntax.py -debug"
          in
          run_main cmd |> assert_Ok);
      t ~checked_output:(Testo.stdout ())
        "output of -rules <rule> -l <lang> <single_file>" (fun () ->
          let cmd =
            "-rules tests/semgrep-core-e2e/rules/basic.yaml -l python \
             tests/semgrep-core-e2e/targets/basic.py -debug"
          in
          run_main cmd |> assert_Ok);
      (* we could also assert that the output is actually equal to the
       * previous one
       *)
      t ~checked_output:(Testo.stdout ()) "handle -targets" (fun () ->
          let cmd =
            "-rules tests/semgrep-core-e2e/rules/basic.yaml  -targets \
             tests/semgrep-core-e2e/targets.json -debug"
          in
          run_main cmd |> assert_Ok);
    ]

let assert_backend ~curl_supported ~eio (expected : Telemetry.exporter_backend)
    =
  let actual = Telemetry.select_exporter_backend ~curl_supported ~eio in
  match (actual, expected) with
  | Curl, Curl
  | Eio, Eio
  | No_exporter, No_exporter ->
      ()
  | _ ->
      failwith
        (spf "curl_supported=%b eio=%b: expected %s, got %s" curl_supported eio
           (Telemetry.show_exporter_backend expected)
           (Telemetry.show_exporter_backend actual))

let assert_no_exporter () =
  if Opentelemetry.Sdk.present () then failwith "unexpected OTel exporter"

let telemetry_tests : Testo.t list =
  Testo.categorize "semgrep-core telemetry exporter"
    [
      Testo.create "curl exporter is only supported on Linux" (fun () ->
          let supported = Core_CLI.curl_telemetry_supported in
          assert (supported Linux);
          assert (not (supported Darwin));
          assert (not (supported Windows));
          assert (not (supported (OtherKernel "unknown"))));
      Testo.create "exporter selection" (fun () ->
          assert_backend ~curl_supported:true ~eio:false Curl;
          assert_backend ~curl_supported:false ~eio:false No_exporter;
          assert_backend ~curl_supported:true ~eio:true Eio;
          assert_backend ~curl_supported:false ~eio:true Eio);
      Testo.create "Eio exporter survives pause with curl disabled" (fun () ->
          Eio_main.run @@ fun base ->
          Eio.Switch.run @@ fun sw ->
          let endpoint = Uri.of_string "https://127.0.0.1:1/v1/traces" in
          Telemetry.configure_otel ~eio_sw_base:(sw, base) ~curl_supported:false
            "test" endpoint;
          Common.protect ~finally:Telemetry.stop_otel (fun () ->
              assert (Opentelemetry.Sdk.present ());
              Telemetry.with_otel_paused assert_no_exporter;
              assert (Opentelemetry.Sdk.present ())));
      Testo.create "unsupported curl: setup and restart create no exporter"
        (fun () ->
          assert_no_exporter ();
          let endpoint = Uri.of_string "https://127.0.0.1:1/v1/traces" in
          Telemetry.configure_otel ~curl_supported:false "test" endpoint;
          assert_no_exporter ();
          Telemetry.restart_otel ();
          assert_no_exporter ();
          Telemetry.with_otel_paused assert_no_exporter;
          assert_no_exporter ();
          Telemetry.stop_otel ();
          Telemetry.stop_otel ();
          assert_no_exporter ());
    ]

let tests () : Testo.t list = semgrep_core_tests @ telemetry_tests
