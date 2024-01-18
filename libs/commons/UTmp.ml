(* Martin Jambon
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

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Operations dealing with files in /tmp (or whatever tmp directory is
 * in your OS).
 *
 * This is mainly a wrapper over UCommon similar functions but using Fpath.t
 *)

(*****************************************************************************)
(* API *)
(*****************************************************************************)

let new_temp_file prefix suffix = UCommon.new_temp_file prefix suffix |> Fpath.v
let erase_temp_files = UCommon.erase_temp_files
let erase_this_temp_file path = UCommon.erase_this_temp_file !!path

let replace_named_pipe_by_regular_file_if_needed ?(prefix = "named-pipe")
    (path : Fpath.t) : Fpath.t =
  if !Common.jsoo then path
    (* don't bother supporting exotic things like fds if running in JS *)
  else
    match (UUnix.stat !!path).st_kind with
    | Unix.S_FIFO ->
        let data = UFile.read_file path in
        let suffix = "-" ^ Fpath.basename path in
        let tmp_path, oc =
          UFilename.open_temp_file
            ~mode:[ Open_creat; Open_excl; Open_wronly; Open_binary ]
            prefix suffix
        in
        let remove () =
          if USys.file_exists tmp_path then USys.remove tmp_path
        in
        (* Try to remove temporary file when program exits. *)
        UStdlib.at_exit remove;
        Common.protect
          ~finally:(fun () -> close_out_noerr oc)
          (fun () -> output_string oc data);
        Fpath.v tmp_path
    | _ -> path
