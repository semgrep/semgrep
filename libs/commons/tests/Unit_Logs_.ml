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
(**
   Unit tests for our Logs_ module
*)

(*
   All these tests do is log to stderr which we capture and check against
   expectations.
*)
let t =
  Testo.create ~category:[ "Logs_" ] ~normalize:[ Testutil_logs.mask_time ]

let test_switch_color () =
  let level = Some Logs.Info in
  Logs_.with_setup ~highlight_setting:On ~level (fun () ->
      Logs.debug (fun m -> m "I am ignored color debug");
      Logs.info (fun m -> m "I am color info");
      Logs.warn (fun m -> m "I am color warning");
      Logs.err (fun m -> m "I am color error");
      Logs_.with_setup ~highlight_setting:Off ~level (fun () ->
          Logs.debug (fun m -> m "I am ignored nocolor debug");
          Logs.info (fun m -> m "I am nocolor info");
          Logs.warn (fun m -> m "I am nocolor warning");
          Logs.err (fun m -> m "I am nocolor error"));
      Logs.debug (fun m -> m "I am color debug again");
      Logs.info (fun m -> m "I am color info again");
      Logs.warn (fun m -> m "I am color warning again");
      Logs.err (fun m -> m "I am color error again"))

let test_switch_level () =
  Logs_.with_setup ~highlight_setting:Off ~level:(Some Warning) (fun () ->
      Logs.debug (fun m -> m "I am ignored debug");
      Logs.info (fun m -> m "I am ignored info");
      Logs.warn (fun m -> m "I am warning");
      Logs.err (fun m -> m "I am error");
      Logs_.with_level (Some Info) (fun () ->
          Logs.debug (fun m -> m "I am ignored debug");
          Logs.info (fun m -> m "I am info");
          Logs.warn (fun m -> m "I am warning");
          Logs.err (fun m -> m "I am error"));
      Logs.debug (fun m -> m "I am ignored debug again");
      Logs.info (fun m -> m "I am ignored info again");
      Logs.warn (fun m -> m "I am warning again");
      Logs.err (fun m -> m "I am error again"))

let test_no_logs () =
  Logs_.with_setup ~level:None (fun () ->
      Logs.debug (fun m -> m "I am ignored debug");
      Logs.info (fun m -> m "I am ignored info");
      Logs.warn (fun m -> m "I am ignored warning");
      Logs.err (fun m -> m "I am ignored error"))

let src_a = Logs.Src.create "test_a"
let src_b = Logs.Src.create "test_b"

module Log_A = (val Logs.src_log src_a : Logs.LOG)
module Log_B = (val Logs.src_log src_b : Logs.LOG)

let test_change_sources () =
  (* bug: on some platforms in CI (MacOS, Nix), the test starts as if the
     highlight_setting was Off even though it is forced to On in Test.ml
     before running the tests.
     Here, we ensure we have the same setting for all platforms. *)
  Logs_.with_setup ~highlight_setting:Off ~level:None @@ fun () ->
  (* Here's the part of the test that matters *)
  Logs_.with_level ~sources:[ "application"; "test_a" ] (Some Info) (fun () ->
      Logs.info (fun m -> m "I am application");
      Log_A.info (fun m -> m "I am A");
      Log_B.info (fun m -> m "I am hidden B");
      Logs_.with_level ~sources:[ "test_b" ] (Some Info) (fun () ->
          Logs.info (fun m -> m "I am hidden application");
          Log_A.info (fun m -> m "I am hidden A");
          Log_B.info (fun m -> m "I am B"));
      Logs.info (fun m -> m "I am application again");
      Log_A.info (fun m -> m "I am A again");
      Log_B.info (fun m -> m "I am hidden B"))

(*
   Check for the absence of the following bug:
   When switching to '~highlight_setting:Off' in some environments,
   the first log message being printed is preceded by the sequence
   ESC "[0m" which is the ansiterm sequence to clear any styling (color etc.).
   This is a big deal because it makes tests that capture and check logs
   fail randomly. Of course, it would be best if ordinary tests didn't check
   the value of logs.
*)
let test_junk_output_bug () =
  let (), capture =
    Logs_.with_setup ~highlight_setting:On ~level:(Some Warning) @@ fun () ->
    Logs.warn (fun m -> m "ignore me");
    Logs_.with_setup ~highlight_setting:Off ~level:(Some Warning) @@ fun () ->
    Testo.with_capture stderr @@ fun () -> Logs.app (fun m -> m "hello")
  in
  match capture with
  | "hello\n" -> ()
  | "\027[0mhello\n" -> Alcotest.fail "the stray ESC [0m is back!"
  | other -> Alcotest.fail (Printf.sprintf "unexpected output: %S" other)

let tests =
  [
    t "switch color" ~checked_output:(Testo.stderr ()) test_switch_color;
    t "switch level" ~checked_output:(Testo.stderr ()) test_switch_level;
    t "no logs" ~checked_output:(Testo.stderr ()) test_no_logs;
    t "change sources" ~checked_output:(Testo.stderr ()) test_change_sources;
    t "junk output bug" test_junk_output_bug;
  ]
