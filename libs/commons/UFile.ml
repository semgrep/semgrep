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
open Common
open Fpath_.Operators

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
   Operations on files in the general sense (regular file, folder, etc.).

   For now, this is a thin layer on top of UCommon. Eventually, we want
   to get rid of the interface exposed by UCommon.

   related libraries:
    - Bos.OS.File, Bos.OS.Dir, Bos.OS.Path, which we should probably use
     (ex: https://erratique.ch/software/bos/doc/Bos/OS/Dir/index.html )
*)

(*****************************************************************************)
(* API *)
(*****************************************************************************)

let fullpath file = UCommon.fullpath !!file |> Fpath.v

let files_of_dirs_or_files_no_vcs_nofilter xs =
  xs |> Fpath_.to_strings |> UCommon.files_of_dir_or_files_no_vcs_nofilter
  |> Fpath_.of_strings

let cat path = UCommon.cat !!path
let cat_array file = "" :: cat file |> Array.of_list
let write_file path data = UCommon.write_file !!path data
let read_file ?max_len path = UCommon.read_file ?max_len !!path
let with_open_out path func = UCommon.with_open_outfile !!path func
let with_open_in path func = UCommon.with_open_infile !!path func
let new_temp_file prefix suffix = UCommon.new_temp_file prefix suffix |> Fpath.v
let erase_temp_files = UCommon.erase_temp_files
let erase_this_temp_file path = UCommon.erase_this_temp_file !!path

let filesize file =
  if not !Common.jsoo (* this does not work well with jsoo *) then
    (UUnix.stat !!file).st_size
    (* src: https://rosettacode.org/wiki/File_size#OCaml *)
  else
    let ic = UStdlib.open_in_bin !!file in
    let i = in_channel_length ic in
    close_in ic;
    i

let filemtime file =
  if !Common.jsoo then failwith "JSOO:filemtime"
  else (UUnix.stat !!file).st_mtime

let is_directory file = (UUnix.stat !!file).st_kind =*= Unix.S_DIR
let is_file file = (UUnix.stat !!file).st_kind =*= Unix.S_REG
let is_symlink file = (UUnix.lstat !!file).st_kind =*= Unix.S_LNK

let is_executable file =
  let stat = UUnix.stat !!file in
  let perms = stat.st_perm in
  stat.st_kind =*= Unix.S_REG && perms land 0o011 <> 0

let lfile_exists filename =
  try
    match (UUnix.lstat !!filename).st_kind with
    | Unix.S_REG
    | Unix.S_LNK ->
        true
    | _ -> false
  with
  | UUnix.Unix_error (Unix.ENOENT, _, _) -> false

(* Helps avoid the `Fatal error: exception Unix_error: No such file or directory stat` *)
let dir_exists path =
  try
    match (UUnix.lstat !!path).st_kind with
    | S_DIR -> true
    | _ -> false
  with
  | UUnix.Unix_error (Unix.ENOENT, _, _) -> false

let find_first_match_with_whole_line path ?split:(chr = '\n') =
  Bos.OS.File.with_ic path @@ fun ic term ->
  let len = in_channel_length ic in
  let res = Bytes.create len in
  really_input ic res 0 len;
  let lines = Bytes.split_on_char chr res in
  let lines = List_.map Bytes.unsafe_to_string lines in
  lines |> List.find_opt (fun str -> String_.contains ~term str)

let find_first_match_with_whole_line path ?split term =
  find_first_match_with_whole_line path ?split term
  |> Result.to_option |> Option.join

(* TODO? slow, and maybe we should cache it to avoid rereading
 * each time the same file for each match.
 * Note that the returned lines do not contain \n.
 *)
let lines_of_file (start_line, end_line) file : string list =
  let arr = cat_array file in
  let lines = List_.enum start_line end_line in
  match arr with
  (* This is the case of the empty file. *)
  | [| "" |] -> []
  | _ -> lines |> List_.map (fun i -> arr.(i))

let replace_named_pipe_by_regular_file_if_needed ?(prefix = "named-pipe")
    (path : Fpath.t) : Fpath.t =
  if !Common.jsoo then path
    (* don't bother supporting exotic things like fds if running in JS *)
  else
    match (UUnix.stat !!path).st_kind with
    | Unix.S_FIFO ->
        let data = read_file path in
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
