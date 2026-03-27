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
  (* This is an internal implementation detail. *)
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
      (* alt: we could require an explicit env argument here *)
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

(* Convert Cmd.env to a string array for Eio process spawning.
   Reuses env_of_env to build the merged environment map. *)
let env_to_string_array_opt (env : Cmd.env option) : string array option =
  match env_of_env env with
  | None -> None
  | Some map ->
      Some
        (Astring.String.Map.fold (fun k v acc -> (k ^ "=" ^ v) :: acc) map []
        |> Array.of_list)

type eio_env =
  < clock : float Eio.Time.clock_ty Eio.Std.r
  ; process_mgr : Eio_unix.Process.mgr_ty Eio.Std.r >

let eio_env_of_base (base : Eio_unix.Stdenv.base) : eio_env = (base :> eio_env)

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

let string_of_run_with_timeout (eio_env : eio_env) ~timeout_seconds ~trim ?env
    cmd =
  Tracing.with_span ~__FILE__ ~__LINE__
    ~data:
      [
        ("cmd.cmd", `String (Cmd.to_string cmd));
        ("cmd.timeout_seconds", `Float timeout_seconds);
      ]
    "UCmd.string_of_run_with_timeout"
  @@ fun sp ->
  log_command cmd;
  let clock = eio_env#clock in
  let proc_mgr = eio_env#process_mgr in
  let env_arr = env_to_string_array_opt env in
  let Cmd.Name name, args = cmd in
  let arg_list = name :: args in
  let run () =
    Eio.Switch.run (fun sw ->
        let stdout_r, stdout_w = Eio_unix.pipe sw in
        let stderr_r, stderr_w = Eio_unix.pipe sw in
        (* nosemgrep: forbid-exec *)
        let proc =
          Eio.Process.spawn ~sw proc_mgr ?env:env_arr
            ~stdout:(stdout_w :> Eio.Flow.sink_ty Eio.Std.r)
            ~stderr:(stderr_w :> Eio.Flow.sink_ty Eio.Std.r)
            arg_list
        in
        (* Close the write ends in the parent so reads terminate on process exit. *)
        Eio.Resource.close stdout_w;
        Eio.Resource.close stderr_w;
        let read_all flow =
          Eio.Buf_read.parse_exn Eio.Buf_read.take_all ~max_size:max_int flow
        in
        let stdout_str, stderr_str =
          Eio.Fiber.pair
            (fun () -> read_all (stdout_r :> Eio.Flow.source_ty Eio.Std.r))
            (fun () -> read_all (stderr_r :> Eio.Flow.source_ty Eio.Std.r))
        in
        let status : Bos.OS.Cmd.status =
          match Eio.Process.await proc with
          | `Exited n -> `Exited n
          | `Signaled n -> `Signaled n
        in
        let stdout_str = if trim then String.trim stdout_str else stdout_str in
        let stderr_str = if trim then String.trim stderr_str else stderr_str in
        (Ok (stdout_str, status), stderr_str))
  in
  let result =
    match
      try Ok (Eio.Time.with_timeout_exn clock timeout_seconds run) with
      | Eio.Time.Timeout -> Error `Timeout
      | Eio.Cancel.Cancelled _ as exn -> raise exn
      | exn -> Error (`Msg (Printexc.to_string exn))
    with
    | Ok (result, stderr_str) -> (result, stderr_str)
    | Error `Timeout -> (Error `Timeout, "")
    | Error (`Msg _ as msg) -> (Error msg, "")
  in
  (match result with
  | Ok (_, status), _ ->
      let exit_code =
        match status with
        | `Exited n -> n
        | `Signaled n -> -n
      in
      Tracing.add_data_to_span sp
        [ ("cmd.exit_code", `Int exit_code); ("cmd.timed_out", `Bool false) ]
  | Error `Timeout, _ ->
      Tracing.add_data_to_span sp
        [ ("cmd.exit_code", `Int (-1)); ("cmd.timed_out", `Bool true) ]
  | Error (`Msg _), _ ->
      Tracing.add_data_to_span sp
        [ ("cmd.exit_code", `Int (-1)); ("cmd.timed_out", `Bool false) ]);
  result

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
let quote_command_for_bash args = List.map quote_arg args |> String.concat " "
