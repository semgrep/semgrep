(*
   Copyright (c) 2023-2025 Semgrep Inc.

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
(* Small wrapper around Bos.OS.Cmd
 *
 * A few functions contain a 'nosemgrep: forbid-exec' because anyway
 * those functions will/are also blacklisted in forbid-exec.jsonnet.
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* Log every external command.

   Let's not log environment variables because they may contain sensitive
   secrets.
   Note that we're using Logs.info below on purpose; this is probably
   something the user wants to know.
*)
let log_command cmd =
  (* nosemgrep: no-logs-in-library *)
  Logs.info (fun m -> m "Running external command: %s" (Cmd.to_string cmd))

(* Create a new temporary file, invoke the passed function on the temporary
   file, delete the temporary file, and return what was in the temporary file
   before it was closed. *)
let with_temp_file_out (f : Fpath.t -> 'a) : 'a * string =
  (* This is an internal implementation detail, so no need to worry about
     using CapTmp. *)
  (* nosemgrep: forbid-tmp *)
  UTmp.with_temp_file (fun path ->
      let res = f path in
      let out = UFile.read_file path in
      (res, out))

(* Create a new temporary file, invoke the passed function on the temporary
   file, delete the temporary file, and log what was in the temporary file if
   it was nonempty, at the same level as 'log_command' above. *)
let with_logging_err_temp_file (f : Fpath.t -> 'a) : 'a =
  let res, err = with_temp_file_out f in
  if err <> "" then
    (* nosemgrep: no-logs-in-library *)
    Logs.info (fun m -> m "error output: %s" err);
  res

let env_of_env (env : Cmd.env option) : Bos.OS.Env.t option =
  let* { vars; inherit_parent_env } = env in
  let start_env =
    if inherit_parent_env then
      (* alt: we could require the Cap.argv capability here *)
      match Bos.OS.Env.current () with
      | Ok start_env -> start_env
      | Error (`Msg err) -> failwith (spf "Bos.OS.Env.current failed: %s" err)
    else Astring.String.Map.empty
  in
  Some
    (vars
    |> List.fold_left
         (fun acc (k, v) -> Astring.String.Map.add k v acc)
         start_env)

(*****************************************************************************)
(* Old Common.cmd_to_list *)
(*****************************************************************************)

exception CmdError of Unix.process_status * string

let process_output_to_list ?(verbose = false) command =
  (* nosemgrep: forbid-exec *)
  let chan = Unix.open_process_in command in
  let res = ref ([] : string list) in
  let rec process_otl_aux () =
    let e = input_line chan in
    res := e :: !res;
    if verbose then
      (* nosemgrep: no-logs-in-library *)
      Logs.info (fun m -> m "%s" e);
    process_otl_aux ()
  in
  try process_otl_aux () with
  | End_of_file ->
      let stat = Unix.close_process_in chan in
      (List.rev !res, stat)

let cmd_to_list ?verbose command =
  let l, exit_status = process_output_to_list ?verbose command in
  match exit_status with
  | Unix.WEXITED 0 -> l
  | _ ->
      raise
        (CmdError
           ( exit_status,
             spf "CMD = %s, RESULT = %s" command (String.concat "\n" l) ))

(*****************************************************************************)
(* API *)
(*****************************************************************************)

(* Like status_of_run, but use this when you want to run a command without
   having it's output captured. *)
let run_subprocess ?env cmd =
  log_command cmd;
  let env = env_of_env env in
  (* nosemgrep: forbid-exec *)
  Cmd.bos_apply (Bos.OS.Cmd.run_status ?env) cmd

let string_of_run ~trim ?env cmd =
  log_command cmd;
  let env = env_of_env env in
  with_logging_err_temp_file (fun err_path ->
      (* nosemgrep: forbid-exec *)
      let out =
        Cmd.bos_apply Bos.OS.Cmd.(run_out ~err:(err_file err_path) ?env) cmd
      in
      (* nosemgrep: forbid-exec *)
      Bos.OS.Cmd.out_string ~trim out)

(* TODO: this is potentially a source of high memory usage if the captured program
 * outputs a lot of log spew. We should add a limit on the data read. *)
let string_of_run_with_stderr ~trim ?env cmd =
  log_command cmd;
  let env = env_of_env env in
  with_temp_file_out (fun err_path ->
      (* nosemgrep: forbid-exec *)
      let out =
        Cmd.bos_apply Bos.OS.Cmd.(run_out ~err:(err_file err_path) ?env) cmd
      in
      (* nosemgrep: forbid-exec *)
      Bos.OS.Cmd.out_string ~trim out)

let lines_of_run ~trim ?env cmd =
  log_command cmd;
  let env = env_of_env env in
  with_logging_err_temp_file (fun err_path ->
      (* nosemgrep: forbid-exec *)
      let out =
        Cmd.bos_apply Bos.OS.Cmd.(run_out ~err:(err_file err_path) ?env) cmd
      in
      (* nosemgrep: forbid-exec *)
      Bos.OS.Cmd.out_lines ~trim out)

(* nosemgrep: forbid-exec *)
let status_of_run ?quiet ?env cmd =
  log_command cmd;
  let env = env_of_env env in
  with_logging_err_temp_file (fun err_path ->
      (* nosemgrep: forbid-exec *)
      Cmd.bos_apply
        Bos.OS.Cmd.(run_status ?quiet ~err:(err_file err_path) ?env)
        cmd)

(*
   A superset of Bash alphanumeric keywords that don't behave like ordinary
   command names or command arguments.
   All these will be quoted because they have or may have a specific
   meaning in some contexts.

   This list also works for POSIX shells.
*)
let bash_non_arg_keywords =
  [
    "case";
    "coproc";
    "do";
    "done";
    "elif";
    "else";
    "esac";
    "fi";
    "for";
    "function";
    "if";
    "in";
    "select";
    "then";
    "until";
    "while";
  ]

let is_bash_keyword =
  let tbl = Hashtbl.create 50 in
  List.iter (fun kw -> Hashtbl.replace tbl kw ()) bash_non_arg_keywords;
  fun str -> Hashtbl.mem tbl str

let is_safe_arg str =
  String.length str > 0
  && (not (is_bash_keyword str))
  && String.for_all
       (function
         (* These are the most common known safe characters.
         It's not all of them and it's ok. *)
         | 'A' .. 'Z'
         | 'a' .. 'z'
         | '0' .. '9'
         | '_'
         | '-'
         | '.' ->
             true
         | _ -> false)
       str

let quote_arg arg = if is_safe_arg arg then arg else Filename.quote arg

(*
   A safe and pretty converter from a list of arguments to a shell command.
*)
let quote_command_for_bash args = List_.map quote_arg args |> String.concat " "
