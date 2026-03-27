(* Brandon Wu
 *
 * Copyright (C) 2023 Semgrep Inc.
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
open Fpath_.Operators
module Out = Semgrep_output_v1_j

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
   Parse a semgrep-install-semgrep-pro --custom-binary=<path>
   command, execute it and exit.

   NOTE: this code only implements the the --custom-binary extention of the
   `install-semgrep-pro` command.
*)

(*****************************************************************************)
(* Types and Constants *)
(*****************************************************************************)

(* We timeout after 10s if the install fails, and we run
 * semgrep -pro_version as part of the install process.
 * TODO: add stdout, but does not even use stdout right now, it abuses
 * Logs.app but we should switch to UConsole.print
 *)
let version_stamp_filename = "pro-installed-by.txt"

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let add_semgrep_pro_version_stamp current_executable_path =
  let pro_version_stamp_path =
    Fpath.parent (Fpath.v current_executable_path) / version_stamp_filename
  in
  (* THINK: does this append or write entirely? *)
  UFile.write_file pro_version_stamp_path Version.version

(*****************************************************************************)
(* Main logic *)
(*****************************************************************************)

(* All the business logic after command-line parsing. Return the desired
   exit code. *)
let run_conf (conf : Install_semgrep_pro_CLI.conf) : Exit_code.t =
  CLI_common.with_logging ~color:Auto ~level:conf.common.logging_level
  @@ fun () ->
  let custom_binary_path =
    match conf.custom_binary with
    | Some binary -> binary
    (* The osemgrep implementation only implements the --custom-binary flag *)
    | None -> raise Pysemgrep.Fallback
  in

  Logs.debug (fun m -> m "conf = %s" (Install_semgrep_pro_CLI.show_conf conf));

  let pro_executable_name =
    Printf.sprintf "semgrep-core-proprietary%s"
      (if Platform.is_windows then ".exe" else "")
  in

  (* We want to install to basically wherever the current executable is,
     but to the name `semgrep-core-proprietary`, which is where the ultimate
     Python wrapper entry point knows to look for the pro binary.
     This is a little bit confusing because that binary might be actually the
     same as the one we're currently running, as `osemgrep-pro`.
     This should be still fine to do, though, as we have already loaded the
     binary into memory, so we can mutate the path it used to be at.
  *)
  let pro_executable_path =
    if Fpath.is_rel (Fpath.v Sys.executable_name) then
      Fpath.v (Sys.getcwd ()) / pro_executable_name
    else Fpath.parent (Fpath.v Sys.executable_name) / pro_executable_name
  in

  (* TODO This is a temporary solution to help offline users *)
  (* THINK: is this the path of the executable? *)
  Logs.app (fun m ->
      m "Semgrep Pro Engine will be installed in %s" !!pro_executable_path);

  (* This hook should only be set if this is currently the semgrep-proprietary binary *)
  (match Hook.get Core_runner.hook_mk_pro_core_run_for_osemgrep with
  | Some _ ->
      Logs.app (fun m -> m "Overwriting Semgrep Pro Engine already installed!")
  | None -> ());

  let semgrep_pro_path_tmp =
    Fpath.set_ext ~multi:true ".tmp_download" pro_executable_path
  in
  (* Copy the custom binary into the tmp path *)
  FileUtil.cp [ custom_binary_path ] !!semgrep_pro_path_tmp;
  FileUtil.chmod
    (`Symbolic
       [
         `User (`Set (`List [ `Read; `Write; `Exec ]));
         `Group (`Set (`List [ `Read; `Exec ]));
         `Other (`Set (`List [ `Read; `Exec ]));
       ])
    [ !!semgrep_pro_path_tmp ];

  (* Get Pro version, it serves as a simple check that the binary works
   * TODO: seems buggy, if passing --custom-binary ./bin/semgrep-core
   * the program returns an error (wrong -pro_version argument) but
   * the whole thing still succeed.
   *)
  let version =
    let cmd = (Cmd.Name !!semgrep_pro_path_tmp, [ "-pro_version" ]) in
    let opt =
      Time_limit.set_timeout ~name:"check pro version" 10.0 ~eio:false
        (fun () ->
          (* TODO?  Bos.OS.Cmd.run_out ~err:Bos.OS.Cmd.err_run_out *)
          let result = UCmd.string_of_run ~trim:true cmd in
          match result with
          | Ok (output, _) -> Some output
          | Error _ -> None)
      |> Option.join
    in
    match opt with
    | Some output -> output
    | None ->
        FileUtil.rm [ !!semgrep_pro_path_tmp ];
        failwith
          "Downloaded binary failed version check, try again or contact \
           support@semgrep.com"
  in

  (* Version check worked so we now install the binary *)
  FileUtil.rm [ !!pro_executable_path ];
  FileUtil.mv !!semgrep_pro_path_tmp !!pro_executable_path;
  add_semgrep_pro_version_stamp !!pro_executable_path;
  Logs.app (fun m ->
      m "\nSuccessfully installed Semgrep Pro Engine (version %s)!" version);
  Exit_code.ok ~__LOC__

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let main (argv : string array) : Exit_code.t =
  let conf = Install_semgrep_pro_CLI.parse_argv argv in
  run_conf conf
