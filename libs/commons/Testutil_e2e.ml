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

let detect_available_commands command_names : string -> bool =
  let tbl = Hashtbl.create 100 in
  List.iter
    (fun command_name ->
      (* Avoid checking twice in case of duplicates *)
      if not (Hashtbl.mem tbl command_name) then
        let avail =
          (* The printfs are a bit annoying to have always on.
              TODO: make them conditional and activable via the testo
              environment ('-e verbose=true'). *)
          match which command_name with
          | Some _path ->
              (* printf "Command '%s' is available: %s\n%!" command_name path; *)
              true
          | None ->
              (* printf
                  "Command '%s' is not available. Tests that need it will be \
                  skipped.\n\
                  %!"
                  command_name; *)
              false
        in
        Hashtbl.replace tbl command_name avail)
    command_names;
  let is_available command_name =
    match Hashtbl.find_opt tbl command_name with
    | Some res -> res
    | None ->
        failwith
          (sprintf
             "You must register the command '%s' by adding it to the list of \
              optional command names in file '%s'"
             command_name __FILE__)
  in
  is_available

(*
   Return the '?skip' argument that will mark a test as skipped if
   any of the listed command names isn't available.
*)
let skip_if_missing_commands ~is_cmd_available command_names =
  let missing_commands =
    List.filter (fun cmd -> not (is_cmd_available cmd)) command_names
  in
  match missing_commands with
  | [] -> None
  | _ ->
      (* Produce an excuse for why we're skipping the test *)
      Some
        (sprintf "missing external command(s): %s"
           (String.concat ", " missing_commands))

(* Rewrite a test suite to be skipped if the required external commands
   are not available.
   This results in the test suite being listed as skipped with an explanation
   rather than missing mysteriously.
*)
let skip_tests_if_missing_commands ~is_cmd_available command_names tests =
  let skipped = skip_if_missing_commands ~is_cmd_available command_names in
  List_.map
    (fun (test : Testo.t) ->
      match test.skipped with
      | Some _ -> test
      | None -> Testo.update ~skipped test)
    tests
