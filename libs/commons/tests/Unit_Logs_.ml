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
  (* For this test, we've had problems in CI where the escape sequence
     ESC "[0m" is printed on stderr at the beginning of this test in Docker
     and possibly some other environments but not all of them.
     This is the sequence that resets the style. Maybe it is inserted on
     purpose by the logger to ensure that any ongoing styling is canceled.
     This is a workaround in the hope of capturing only the output created
     here after flushing stderr. Note that stderr is flushed automatically
     by Testo.with_capture.

     This is good illustration of why in general our tests should not
     capture and check logs.
  *)
  let (), capture =
    Logs_.with_setup ~highlight_setting:Off ~level:(Some Warning) (fun () ->
        (* An unsolicited ESC "[0m" is printed before the first log message
           that is not ignored. *)
        Logs.err (fun m -> m "I am not captured");
        Testo.with_capture stderr (fun () ->
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
            Logs.err (fun m -> m "I am error again")))
  in
  Testo.with_temp_file (fun path ->
      Testo.write_file path (Testutil_logs.mask_time capture);
      Testo.stash_output_file path "results.txt")

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
  (* Here, we force color so as to get the same results on all platforms.
     We could also turn if off like it was done for 'test_switch_level'. *)
  Logs_.with_setup ~highlight_setting:On ~level:None (fun () ->
      Logs_.with_level ~sources:[ "application"; "test_a" ] (Some Info)
        (fun () ->
          Logs.info (fun m -> m "I am application");
          Log_A.info (fun m -> m "I am A");
          Log_B.info (fun m -> m "I am hidden B");
          Logs_.with_level ~sources:[ "test_b" ] (Some Info) (fun () ->
              Logs.info (fun m -> m "I am hidden application");
              Log_A.info (fun m -> m "I am hidden A");
              Log_B.info (fun m -> m "I am B"));
          Logs.info (fun m -> m "I am application again");
          Log_A.info (fun m -> m "I am A again");
          Log_B.info (fun m -> m "I am hidden B")))

let tests =
  [
    t "switch color" ~checked_output:(Testo.stderr ()) test_switch_color;
    t "switch level"
      ~checked_output_files:[ Testo.checked_output_file "results.txt" ]
      test_switch_level;
    t "no logs" ~checked_output:(Testo.stderr ()) test_no_logs;
    t "change sources" ~checked_output:(Testo.stderr ()) test_change_sources;
  ]
