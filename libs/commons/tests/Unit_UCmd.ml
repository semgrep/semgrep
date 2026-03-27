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

let tests =
  [
    Testo.create "quote command for bash" ?skipped test_quote_command_for_bash;
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
