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
module Log = Log_commons.Log

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
        acc := Common.input_text_line chan :: !acc
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
              Log.warn (fun m -> m "%s does not exist anymore" f);
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

let file_kind_to_yojson (kind : Unix.file_kind) =
  let kind_str =
    match kind with
    | S_REG -> "S_REG"
    | S_DIR -> "S_DIR"
    | S_CHR -> "S_CHR"
    | S_BLK -> "S_BLK"
    | S_LNK -> "S_LNK"
    | S_FIFO -> "S_FIFO"
    | S_SOCK -> "S_SOCK"
  in
  `String kind_str

let file_kind_of_yojson (yojson : Yojson.Safe.t) =
  match yojson with
  | `String "S_REG" -> Ok Unix.S_REG
  | `String "S_DIR" -> Ok Unix.S_DIR
  | `String "S_CHR" -> Ok Unix.S_CHR
  | `String "S_BLK" -> Ok Unix.S_BLK
  | `String "S_LNK" -> Ok Unix.S_LNK
  | `String "S_FIFO" -> Ok Unix.S_FIFO
  | `String "S_SOCK" -> Ok Unix.S_SOCK
  | json ->
      Error
        (Printf.sprintf
           "Could not convert to Unix.file_kind expected `String, received %s"
           Yojson.Safe.(to_string json))

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

let is_dir ~follow_symlinks path =
  let stat = if follow_symlinks then UUnix.stat else UUnix.lstat in
  match (stat !!path).st_kind with
  | S_DIR -> true
  | _ -> false
  | exception UUnix.Unix_error _ -> false

let is_reg ~follow_symlinks path =
  let stat = if follow_symlinks then UUnix.stat else UUnix.lstat in
  match (stat !!path).st_kind with
  | S_REG -> true
  | _ -> false
  | exception UUnix.Unix_error _ -> false

let is_dir_or_reg ~follow_symlinks path =
  let stat = if follow_symlinks then UUnix.stat else UUnix.lstat in
  match (stat !!path).st_kind with
  | S_DIR
  | S_REG ->
      true
  | _ -> false
  | exception UUnix.Unix_error _ -> false

let is_lnk path =
  match (UUnix.lstat !!path).st_kind with
  | S_LNK -> true
  | _ -> false
  | exception UUnix.Unix_error _ -> false

let is_lnk_or_reg path =
  match (UUnix.lstat !!path).st_kind with
  | S_LNK
  | S_REG ->
      true
  | _ -> false
  | exception UUnix.Unix_error _ -> false

(* This function isn't very useful but we offer it for completeness. *)
let is_dir_or_lnk path =
  match (UUnix.lstat !!path).st_kind with
  | S_LNK
  | S_DIR ->
      true
  | _ -> false
  | exception UUnix.Unix_error _ -> false

let is_dir_or_lnk_or_reg path =
  match (UUnix.lstat !!path).st_kind with
  | S_DIR
  | S_LNK
  | S_REG ->
      true
  | _ -> false
  | exception UUnix.Unix_error _ -> false

let is_executable file =
  let stat = UUnix.stat !!file in
  let perms = stat.st_perm in
  stat.st_kind =*= Unix.S_REG && perms land 0o011 <> 0

let rec make_directories dir =
  try UUnix.mkdir !!dir 0o755 with
  (* The directory already exists *)
  | UUnix.Unix_error ((EEXIST | EISDIR), _, _)
    when is_dir ~follow_symlinks:false dir ->
      ()
  (* parent doesn't exist *)
  | UUnix.Unix_error (ENOENT, _, _) ->
      let parent = Fpath.parent dir in
      make_directories parent;
      make_directories dir

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
let lines_of_file (start_line, end_line) (file : Fpath.t) :
    (string list, string) result =
  let arr = cat_array file in
  if not (start_line <= end_line) then
    Error
      (spf "lines_of_file: start line %d > end line %d for %s" start_line
         end_line !!file)
  else
    let line_idx = List_.enum start_line end_line in
    match arr with
    (* This is the case of the empty file.
     * TODO: but then we should also thrown an ex if line_idx is not null?
     *)
    | [| "" |] -> Ok []
    | _ -> (
        try
          Ok
            (line_idx
            |> List_.map (fun i ->
                   try arr.(i) with
                   | Invalid_argument s ->
                       raise (Invalid_argument (spf "%s on index %d" s i))))
        with
        | Invalid_argument s -> Error (spf "lines_of_file: %s" s))

(* alt: we could also provide the variant below but probably better to force the
 * caller to propery handle out of bounds errors
 *
 * let lines_of_file_exn (start_line, end_line) (file: Fpath.t) : string list =
 *  match lines_of_file (start_line, end_line) file with
 *  | Ok xs -> xs
 *  | Error s -> raise (Common.ErrorOnFile (spf "lines_of_file_exn(): %s" s, file))
 *)
