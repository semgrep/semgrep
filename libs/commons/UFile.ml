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

let tags = Logs_.create_tags [ __MODULE__ ]

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
(* Globals and constants *)
(*****************************************************************************)

let follow_symlinks = ref false

let vcs_re =
  "(^((\\.hg)|(CVS)|(\\.git)|(_darcs)|(\\.svn))$)|(.*\\.git_annot$)|(.*\\.marshall$)"
  |> Re.Posix.re |> Re.compile

(*****************************************************************************)
(* Legacy API using 'string' for filenames *)
(*****************************************************************************)

module Legacy = struct
  let cat file =
    let acc = ref [] in
    let chan = UStdlib.open_in_bin file in
    try
      while true do
        acc := input_text_line chan :: !acc
      done;
      assert false
    with
    | End_of_file ->
        close_in chan;
        List.rev !acc

  (*
   This implementation works even with Linux files like /dev/fd/63
   created by bash's process substitution e.g.

     my-ocaml-program <(echo contents)

   See https://www.gnu.org/software/bash/manual/html_node/Process-Substitution.html

   In bash, '<(echo contents)' is replaced by something like
   '/dev/fd/63' which is a special file of apparent size 0 (as
   reported by `Unix.stat`) but contains data (here,
   "contents\n"). So we can't use 'Unix.stat' or 'in_channel_length'
   to obtain the length of the file contents. Instead, we read the file
   chunk by chunk until there's nothing left to read.

   Why such a function is not provided by the ocaml standard library is
   unclear.
*)
  let read_file ?(max_len = max_int) path =
    if !jsoo then (
      let ic = UStdlib.open_in_bin path in
      let s = really_input_string ic (in_channel_length ic) in
      close_in ic;
      s)
    else
      let buf_len = 4096 in
      let extbuf = Buffer.create 4096 in
      let buf = Bytes.create buf_len in
      let rec loop fd =
        match Unix.read fd buf 0 buf_len with
        | 0 -> Buffer.contents extbuf
        | num_bytes ->
            assert (num_bytes > 0);
            assert (num_bytes <= buf_len);
            Buffer.add_subbytes extbuf buf 0 num_bytes;
            if Buffer.length extbuf >= max_len then Buffer.sub extbuf 0 max_len
            else loop fd
      in
      let fd = UUnix.openfile path [ Unix.O_RDONLY ] 0 in
      Common.protect ~finally:(fun () -> Unix.close fd) (fun () -> loop fd)

  let write_file ~file s =
    let chan = UStdlib.open_out_bin file in
    output_string chan s;
    close_out chan

  (* emacs/lisp inspiration (eric cooper and yaron minsky use that too) *)
  let (with_open_outfile :
        string (* filename *) -> ((string -> unit) * out_channel -> 'a) -> 'a) =
   fun file f ->
    let chan = UStdlib.open_out_bin file in
    let xpr s = output_string chan s in
    unwind_protect
      (fun () ->
        let res = f (xpr, chan) in
        close_out chan;
        res)
      (fun _e -> close_out chan)

  let (with_open_infile : string (* filename *) -> (in_channel -> 'a) -> 'a) =
   fun file f ->
    let chan = UStdlib.open_in_bin file in
    unwind_protect
      (fun () ->
        let res = f chan in
        close_in chan;
        res)
      (fun _e ->
        (* TODO? use close_in_noerr? *)
        close_in chan)

  (* Directories *)

  (** [dir_contents] returns the paths of all regular files that are
 * contained in [dir]. Each file is a path starting with [dir].
  *)
  let dir_contents dir =
    let rec loop result = function
      | f :: fs -> (
          match f with
          | f when not (USys.file_exists f) ->
              Logs.err (fun m -> m ~tags "%s does not exist anymore" f);
              loop result fs
          | f when USys.is_directory f ->
              USys.readdir f |> Array.to_list
              |> List_.map (Filename.concat f)
              |> List.append fs |> loop result
          | f -> loop (f :: result) fs)
      | [] -> result
    in
    loop [] [ dir ]

  let files_of_dirs_or_files_no_vcs_nofilter xs =
    xs
    |> List_.map (fun x ->
           if USys.is_directory x then
             let files = dir_contents x in
             List.filter (fun x -> not (Re.execp vcs_re x)) files
           else [ x ])
    |> List_.flatten
end

(*****************************************************************************)
(* Using Fpath.t *)
(*****************************************************************************)

let files_of_dirs_or_files_no_vcs_nofilter xs =
  xs |> Fpath_.to_strings |> Legacy.files_of_dirs_or_files_no_vcs_nofilter
  |> Fpath_.of_strings

let cat path = Legacy.cat !!path
let cat_array file = "" :: cat file |> Array.of_list
let write_file ~file data = Legacy.write_file ~file:!!file data
let read_file ?max_len path = Legacy.read_file ?max_len !!path
let with_open_out path func = Legacy.with_open_outfile !!path func
let with_open_in path func = Legacy.with_open_infile !!path func

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

(* TODO? there's also USys.is_directory? *)
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
