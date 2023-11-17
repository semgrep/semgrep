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
open File.Operators
module Out = Semgrep_output_v1_j
module Http_helpers = Http_helpers.Make (Lwt_platform)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
   Parse a semgrep-install-semgrep-pro command, execute it and exit.

   Translated from install.py
*)

(*****************************************************************************)
(* Constants *)
(*****************************************************************************)

let version_stamp_filename = "pro-installed-by.txt"

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let unimplemented _ = failwith "unimpl"
let platform _ = unimplemented ()
let machine _ = unimplemented ()

let add_semgrep_pro_version_stamp current_executable_path =
  let pro_version_stamp_path =
    Fpath.parent (Fpath.v current_executable_path) / version_stamp_filename
  in
  (* THINK: does this append or write entirely? *)
  File.write_file pro_version_stamp_path Version.version

(* Here, we are making the assumption that when the executable is running, it
   has been loaded into memory, and thus it is safe to manipulate the position
   that it used to be at.
   The reason why this is different than install.py is that the Pythonw as a
   wrapper around the proprietary binary. In this case, this code that is running
   might actually *be* the proprietary binary, so we have to kind of replace
   ourselves, if we're not.
*)
let download_semgrep_pro platform_kind dest =
  let dest = !!dest in
  let uri =
    Uri.(
      add_query_params'
        (with_path !Semgrep_envvars.v.semgrep_url
           (Common.spf "api/agent/deployments/deepbinary/%s" platform_kind))
        [ ("version", Version.version) ])
  in

  match Http_helpers.get uri with
  | Error (_, { code = 401; _ }) ->
      Logs.info (fun m ->
          m
            "API token not valid. Try to run `semgrep logout` and `semgrep \
             login` again.");
      false
  | Error (_, { code = 403; _ }) ->
      Logs.warn (fun m ->
          m "Logged in deployment does not have access to Semgrep Pro Engine.");
      Logs.warn (fun m ->
          m
            "Visit https://semgrep.dev/products/pro-engine for more \
             information.");
      false
  (* THINK: ??? is this raise for status? *)
  | Error _ -> false
  | Ok (body, { response; _ }) ->
      (* Make sure no such binary exists. We have had weird situations when the
       * downloaded binary was corrupted, and overwriting it did not fix it, but
       * it was necessary to `rm -f` it.
       *)
      if Sys.file_exists dest then FileUtil.rm [ dest ];

      (* TODO: does this matter if we don't have a progress bar? *)
      let _file_size =
        Cohttp.(Header.get (Response.headers response) "Content-Length")
        |> Option.map int_of_string |> Option.value ~default:0
      in

      File.write_file (Fpath.v dest) body;
      true

(*****************************************************************************)
(* Main logic *)
(*****************************************************************************)

(* All the business logic after command-line parsing. Return the desired
   exit code. *)
let run_conf (conf : Install_semgrep_pro_CLI.conf) : Exit_code.t =
  (* state.terminal.configure(verbose=False, debug=False, quiet=False, force_color=False)
  *)
  let current_executable_path = unimplemented () in

  (* TODO This is a temporary solution to help offline users *)
  (* THINK: is this the path of the executable? *)
  Logs.app (fun m ->
      m "Semgrep Pro Engine will be installed in %s" current_executable_path);

  (* This hook should only be set if this is currently the semgrep-proprietary binary *)
  (match !Scan_subcommand.hook_pro_scan_func_for_osemgrep with
  | Some _ ->
      Logs.app (fun m -> m "Overwriting Semgrep Pro Engine already installed!")
  | None -> ());

  match ((Semgrep_settings.load ()).api_token, conf.custom_binary) with
  | None, None ->
      Logs.err (fun m ->
          m "run `semgrep login` before running `semgrep install-semgrep-pro`");
      Exit_code.fatal
  | _ ->
      let platform_kind =
        match platform () with
        | s when String.starts_with ~prefix:"darwin" s ->
            (* arm64 is possible. Dunno if other arms are, so let's just check a prefix. *)
            if String.starts_with ~prefix:"arm" (machine ()) then "osx-arm64"
            else "osx-x86"
        | s when String.starts_with ~prefix:"linux" s -> "manylinux"
        | _ ->
            Logs.app (fun m ->
                m
                  "Running on potentially unsupported platform. Installing \
                   linux compatible binary");
            "manylinux"
      in

      (* Download the binary into a temporary location, check it, then install it.
         This should prevent bad installations.
      *)
      let semgrep_pro_path_tmp =
        Fpath.set_ext ~multi:true ".tmp_download" current_executable_path
      in

      let failed =
        match conf.custom_binary with
        | None -> download_semgrep_pro platform_kind semgrep_pro_path_tmp
        | Some custom_binary_path ->
            FileUtil.cp [ custom_binary_path ] !!semgrep_pro_path_tmp;
            true
      in

      if failed then Exit_code.fatal
      else (
        (* THINK: Do we need to give exec permissions to everybody? Can this be a security risk?
           *        The binary should not have setuid or setgid rights, so letting others
           *        execute it should not be a problem.
        *)

        (* nosemgrep: tests.precommit_dogfooding.python.lang.security.audit.insecure-file-permissions.insecure-file-permissions *)
        FileUtil.chmod
          (`Symbolic
            [ `User (`Set `Exec); `Group (`Set `Exec); `Other (`Set `Exec) ])
          [ !!semgrep_pro_path_tmp ];

        (* Get Pro version, it serves as a simple check that the binary works *)
        let version =
          let cmd = Bos.Cmd.(v !!semgrep_pro_path_tmp % "-pro_version") in
          let opt =
            Time_limit.set_timeout ~name:"check pro version" 10.0 (fun () ->
                let path_r =
                  Bos.OS.Cmd.run_out ~err:Bos.OS.Cmd.err_run_out cmd
                in
                let result = Bos.OS.Cmd.out_string ~trim:true path_r in
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
        FileUtil.rm [ current_executable_path ];
        FileUtil.mv !!semgrep_pro_path_tmp current_executable_path;
        add_semgrep_pro_version_stamp current_executable_path;
        Logs.app (fun m ->
            m "\nSuccessfully installed Semgrep Pro Engine (version %s)!"
              version);
        Exit_code.ok)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let main (argv : string array) : Exit_code.t =
  let conf = Install_semgrep_pro_CLI.parse_argv argv in
  run_conf conf
