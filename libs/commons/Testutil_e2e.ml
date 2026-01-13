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
(*
   End-to-end testing utilities

   This includes:
   - checking for the availability of external commands
   - skipping a test if some command isn't available
   - running external commands
*)

open Printf
module Log = Log_commons.Log

let check_prerequisites prerequisites : string -> bool =
  let tbl = Hashtbl.create 100 in
  List.iter
    (fun (name, check) ->
      if Hashtbl.mem tbl name then
        invalid_arg
          (sprintf "check_prerequisites: prerequisite '%s' is duplicated" name)
      else Hashtbl.add tbl name (check ()))
    prerequisites;
  let is_available name =
    match Hashtbl.find_opt tbl name with
    | Some res -> res
    | None ->
        failwith
          (sprintf
             "You must register the prerequisite '%s' by adding it to the list \
              of prerequisites with 'check_prerequisites'"
             name)
  in
  is_available

(* A command that takes a command name and print its full file path
   on stdout, followed by a line ending, or exits with a nonzero code. *)
let unix_which = if Sys.win32 then "where.exe" else "which"

let which command_name =
  let success, out =
    Testo.with_capture stdout (fun () ->
        match Sys.command (sprintf "%s %s" unix_which command_name) with
        | 0 -> true
        | _ -> false)
  in
  if success then Some (String.trim out) else None

let command_exists command_name () =
  match which command_name with
  | Some path ->
      Log.info (fun m -> m "Command '%s' is available: %s" command_name path);
      true
  | None ->
      Log.info (fun m -> m "Command '%s' is not available." command_name);
      false

(*
   Return the '?skip' argument that will mark a test as skipped if
   any of the listed command names isn't available.
*)
let skip_if_missing_prerequisites ~prerequisite_exists names =
  let missing_prerequisites =
    List.filter (fun name -> not (prerequisite_exists name)) names
  in
  match missing_prerequisites with
  | [] -> None
  | _ ->
      (* Produce an excuse for why we're skipping the test *)
      Some
        (sprintf "missing prerequisite(s): %s"
           (String.concat ", " missing_prerequisites))

(* Mark a test to be skipped if the required external commands
   are not available or if other conditions are not fulfilled.
   This results in the test suite being listed as skipped with an explanation
   rather than missing mysteriously.
*)
let skip_test_if_missing_prerequisites ~prerequisite_exists command_names
    (test : Testo.t) =
  let skipped =
    skip_if_missing_prerequisites ~prerequisite_exists command_names
  in
  match test.skipped with
  | Some _ -> test
  | None -> Testo.update ~skipped test

let skip_tests_if_missing_prerequisites ~prerequisite_exists command_names tests
    =
  List_.map
    (skip_test_if_missing_prerequisites ~prerequisite_exists command_names)
    tests

let run_command ?(expected_exit_code = 0) ?(on_error = fun () -> ()) argv =
  let cmd : Cmd.t =
    match argv with
    | [] -> (Name "", [])
    | argv0 :: args -> (Name argv0, args)
  in
  let command_string = UCmd.quote_command_for_bash argv in
  eprintf "Command: %s\n%!" command_string;
  (* nosemgrep: forbid-exec *)
  match UCmd.run_subprocess cmd with
  | Ok (`Exited n) when n = expected_exit_code ->
      eprintf "Command '%s' exited with expected code %i\n" command_string n
  | Ok (`Exited n) ->
      on_error ();
      failwith
        (sprintf "Command '%s' exited with unexpected code %i" command_string n)
  | Ok (`Signaled n) ->
      on_error ();
      failwith
        (sprintf "Command '%s' was killed with signal %i" command_string n)
  | Error (`Msg msg) ->
      on_error ();
      failwith (sprintf "Command '%s' failed to run: %s" command_string msg)
