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

open Printf
open Fpath_.Operators
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
          Exit_code.Check.ok exit_code))

(* Remaining part of test_login.py (see also Test_login_subcommand.ml) *)
let test_scan_config_registry_with_invalid_token caps : Testo.t =
  Testo.create ~checked_output:(Testo.stderr ()) __FUNCTION__
    ~normalize:[ Testo.mask_not_substrings [ "Saved access token" ] ]
    (Testutil_login.with_login_test_env (fun () ->
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
                     (caps :> Login_subcommand.caps)
                     [| "semgrep-login" |]
                 in
                 Exit_code.Check.ok exit_code));

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
           Scan_subcommand.main
             (caps :> Scan_subcommand.caps)
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
               {|Failed to download config from https://semgrep.dev/c/r/python.lang.correctness.useless-eqeq.useless-eqeq, returned code 401: {"error":"Not authorized"}|}
               msg;
             ()))

let test_absolute_target_path caps =
  let func () =
    UTmp.with_temp_file ~contents:"hello\n" ~suffix:".py" (fun path ->
        assert (Fpath.is_abs path);
        (* We want 'path' to be in a folder other than the current
           folder. *)
        assert (!!(Fpath.parent path) <> Unix.getcwd ());
        Scan_subcommand.main caps
          [|
            "semgrep-scan";
            "--experimental";
            "-l";
            "python";
            "-e";
            "hello";
            !!path;
          |]
        |> Exit_code.Check.ok)
  in
  Testo.create "absolute path as target" func

let random_init = lazy (Random.self_init ())

let create_named_pipe () =
  Lazy.force random_init;
  let path =
    Filename.concat
      (Filename.get_temp_dir_name ())
      (sprintf "semgrep-test-%x.py" (Random.bits ()))
  in
  Unix.mkfifo path 0o644;
  Fpath.v path

(*
   This probably doesn't work on Windows due to the reliance on a shell
   command but could be ported (it doesn't need 'fork').
   TODO: switch to OCaml 5 and use parallelism.
*)
let with_read_from_named_pipe ~data func =
  let pipe_path = create_named_pipe () in
  Common.protect
    (fun () ->
      (* Start another process to write to the pipe in parallel *)
      UTmp.with_temp_file (fun reg_file ->
          (* We go through a regular file so as to avoid quoting issues. *)
          UFile.write_file ~file:reg_file data;
          let writer_command =
            (* Copy the data from the regular file into the named pipe *)
            sprintf "cat '%s' >> '%s'" !!reg_file !!pipe_path
          in
          (* Launch the process that feeds the pipe *)
          let writer = Unix.open_process_out writer_command in
          Common.protect
            (fun () ->
              (* This function can read the payload from the named pipe *)
              func pipe_path)
            ~finally:(fun () ->
              (* Close the helper process *)
              close_out_noerr writer)))
    ~finally:(fun () -> Sys.remove !!pipe_path)

let test_named_pipe (caps : Scan_subcommand.caps) =
  let func () =
    (* Search for pattern "hello" in a named pipe containing "hello" *)
    with_read_from_named_pipe ~data:"hello\n" (fun pipe_path ->
        Scan_subcommand.main caps
          [|
            "semgrep-scan";
            "--experimental";
            "-l";
            "python";
            "-e";
            "hello";
            !!pipe_path;
          |]
        |> Exit_code.Check.ok)
  in
  Testo.create "named pipe as target" func

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let tests (caps : CLI.caps) =
  let scan_caps = (caps :> Scan_subcommand.caps) in
  Testo.categorize "Osemgrep multi subcommands (e2e)"
    [
      test_scan_config_registry_no_token caps;
      test_scan_config_registry_with_invalid_token caps;
      test_absolute_target_path scan_caps;
      test_named_pipe scan_caps;
    ]
