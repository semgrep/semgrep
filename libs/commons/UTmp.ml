(* Martin Jambon
 *
 * Copyright (C) 2023-2024 Semgrep Inc.
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

let tags = Logs_.create_tags [ __MODULE__ ]

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Operations dealing with files in /tmp (or whatever tmp directory is
 * in your OS).
 *)

(*****************************************************************************)
(* Globals *)
(*****************************************************************************)

let _temp_files_created = Hashtbl.create 101
let save_tmp_files = ref false

(* TODO: merge with _temp_files_created *)
let tmp_file_cleanup_hooks = ref []

let erase_temp_files () =
  if not !save_tmp_files then (
    _temp_files_created
    |> Hashtbl.iter (fun s () ->
           Logs.debug (fun m -> m ~tags "erasing: %s" s);
           USys.remove s);
    Hashtbl.clear _temp_files_created)

(*****************************************************************************)
(* Legacy API using 'string' for filenames *)
(*****************************************************************************)

module Legacy = struct
  (* ex: new_temp_file "cocci" ".c" will give "/tmp/cocci-3252-434465.c" *)
  let new_temp_file prefix suffix =
    let pid = if !Common.jsoo then 42 else UUnix.getpid () in
    let processid = i_to_s pid in
    let tmp_file =
      UFilename.temp_file (prefix ^ "-" ^ processid ^ "-") suffix
    in
    Hashtbl.add _temp_files_created tmp_file ();
    tmp_file

  let erase_this_temp_file f =
    if not !save_tmp_files then (
      Hashtbl.remove _temp_files_created f;
      Logs.debug (fun m -> m ~tags "erasing: %s" f);
      USys.remove f)

  let with_tmp_file ~(str : string) ~(ext : string) (f : string -> 'a) : 'a =
    let tmpfile = new_temp_file "tmp" ("." ^ ext) in
    UFile.Legacy.write_file ~file:tmpfile str;
    Common.finalize
      (fun () -> f tmpfile)
      (fun () ->
        !tmp_file_cleanup_hooks |> List.iter (fun f -> f tmpfile);
        erase_this_temp_file tmpfile)

  let register_tmp_file_cleanup_hook f = Stack_.push f tmp_file_cleanup_hooks
end

(*****************************************************************************)
(* API using Fpath.t for filenames *)
(*****************************************************************************)

let new_temp_file prefix suffix = Legacy.new_temp_file prefix suffix |> Fpath.v
let erase_this_temp_file path = Legacy.erase_this_temp_file !!path

let with_tmp_file ~str ~ext f =
  Legacy.with_tmp_file ~str ~ext (fun file -> f (Fpath.v file))

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

let get_temp_dir_name () = Fpath.v (UFilename.get_temp_dir_name ())
