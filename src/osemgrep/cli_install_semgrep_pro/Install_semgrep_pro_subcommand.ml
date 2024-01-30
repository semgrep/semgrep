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
module Http_helpers = Http_helpers.Make (Lwt_platform)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
   Parse a semgrep-install-semgrep-pro command, execute it and exit.

   This command installs the Semgrep Pro Engine binary via interfacing with
   Semgrep App, by making an HTTP request to an endpoint in the app with an
   authentication token.

   Translated from install.py
*)

(*****************************************************************************)
(* Types and Constants *)
(*****************************************************************************)

(* TODO: does not even use stdout right now, it abuses Logs.app *)
type caps = < Cap.network >

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

let download_semgrep_pro (caps : < Cap.network ; .. >) platform_kind dest =
  let dest = !!dest in
  match (Semgrep_settings.load ()).api_token with
  | None ->
      Logs.err (fun m ->
          m "No API token found, please run `semgrep login` first.");
      false
  | Some token -> (
      let caps = Auth.cap_token_and_network token caps in
      match Semgrep_App.fetch_pro_binary caps platform_kind with
      | Error (_, { code = 401; _ }) ->
          Logs.err (fun m ->
              m
                "API token not valid. Try to run `semgrep logout` and `semgrep \
                 login` again.");
          false
      | Error (_, { code = 403; _ }) ->
          Logs.err (fun m ->
              m
                "Logged in deployment does not have access to Semgrep Pro \
                 Engine.");
          Logs.err (fun m ->
              m
                "Visit https://semgrep.dev/products/pro-engine for more \
                 information.");
          false
      (* THINK: ??? is this raise for status? *)
      | Error _ -> false
      | Ok (body, { response; _ }) ->
          (* Make sure no such binary exists. We have had weird situations
           * when the downloaded binary was corrupted, and overwriting it did
           * not fix it, but it was necessary to `rm -f` it.
           *)
          if Sys.file_exists dest then FileUtil.rm [ dest ];

          (* TODO: does this matter if we don't have a progress bar? *)
          let _file_size =
            Cohttp.(Header.get (Response.headers response) "Content-Length")
            |> Option.map int_of_string |> Option.value ~default:0
          in

          UFile.write_file (Fpath.v dest) body;
          true)

(*****************************************************************************)
(* Main logic *)
(*****************************************************************************)

(* All the business logic after command-line parsing. Return the desired
   exit code. *)
let run_conf (caps : caps) (conf : Install_semgrep_pro_CLI.conf) : Exit_code.t =
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
      Fpath.v (Sys.getcwd ()) / "semgrep-core-proprietary"
    else Fpath.parent (Fpath.v Sys.executable_name) / "semgrep-core-proprietary"
  in

  (* TODO This is a temporary solution to help offline users *)
  (* THINK: is this the path of the executable? *)
  Logs.app (fun m ->
      m "Semgrep Pro Engine will be installed in %s" !!pro_executable_path);

  (* This hook should only be set if this is currently the semgrep-proprietary binary *)
  (match !Core_runner.hook_pro_scan_func_for_osemgrep with
  | Some _ ->
      Logs.app (fun m -> m "Overwriting Semgrep Pro Engine already installed!")
  | None -> ());

  match ((Semgrep_settings.load ()).api_token, conf.custom_binary) with
  | None, None ->
      Logs.err (fun m ->
          m "run `semgrep login` before running `semgrep install-semgrep-pro`");
      Exit_code.fatal ~__LOC__
  | _ ->
      let platform_kind =
        match (Platform.kernel (), Platform.arch ()) with
        | Darwin, Arm
        | Darwin, Arm64 ->
            Semgrep_App.Osx_arm64
        | Darwin, _ -> Semgrep_App.Osx_x86_64
        | Linux, _ -> Manylinux_x86_64
        | _ ->
            Logs.app (fun m ->
                m
                  "Running on potentially unsupported platform. Installing \
                   linux compatible binary");
            Manylinux_x86_64
      in

      (* Download the binary into a temporary location, check it, then install it.
         This should prevent bad installations.
      *)
      let semgrep_pro_path_tmp =
        Fpath.set_ext ~multi:true ".tmp_download" pro_executable_path
      in

      let download_succeeded =
        match conf.custom_binary with
        | None -> download_semgrep_pro caps platform_kind semgrep_pro_path_tmp
        | Some custom_binary_path ->
            FileUtil.cp [ custom_binary_path ] !!semgrep_pro_path_tmp;
            true
      in

      if not download_succeeded then Exit_code.fatal ~__LOC__
      else (
        (* THINK: Do we need to give exec permissions to everybody? Can this be a security risk?
           *        The binary should not have setuid or setgid rights, so letting others
           *        execute it should not be a problem.
        *)
        FileUtil.chmod
          (`Symbolic
            [ `User (`Set `Exec); `Group (`Set `Exec); `Other (`Set `Exec) ])
          [ !!semgrep_pro_path_tmp ];

        (* Get Pro version, it serves as a simple check that the binary works *)
        let version =
          let cmd = (Cmd.Name !!semgrep_pro_path_tmp, [ "-pro_version" ]) in
          let opt =
            Time_limit.set_timeout ~name:"check pro version" 10.0 (fun () ->
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
            m "\nSuccessfully installed Semgrep Pro Engine (version %s)!"
              version);
        Exit_code.ok ~__LOC__)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let main (caps : caps) (argv : string array) : Exit_code.t =
  let conf = Install_semgrep_pro_CLI.parse_argv argv in
  run_conf caps conf
