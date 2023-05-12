(*
   Operations on files in the general sense (regular file, folder, etc.).

   For now, this is a thin layer on top of Common. Eventually, we want
   to get rid of the interface exposed by Common.

   related libraries:
    - Bos.OS.File, Bos.OS.Dir, Bos.OS.Path, which we should probably use
     (ex: https://erratique.ch/software/bos/doc/Bos/OS/Dir/index.html )
*)

module Path = struct
  include Fpath

  let of_strings strings = Common.map Fpath.v strings
  let to_strings paths = Common.map Fpath.to_string paths
  let ( !! ) = Fpath.to_string
end

module Operators = struct
  let ( / ) = Fpath.( / )
  let ( // ) = Fpath.( // )
  let ( !! ) = Path.( !! )
end

open Operators

let fullpath file = Common.fullpath !!file |> Fpath.v
let readable ~root path = Common.readable ~root:!!root !!path |> Fpath.v

let files_of_dirs_or_files_no_vcs_nofilter xs =
  xs |> Path.to_strings |> Common.files_of_dir_or_files_no_vcs_nofilter
  |> Path.of_strings

let input_text_line = Common.input_text_line
let cat path = Common.cat !!path
let write_file path data = Common.write_file !!path data
let read_file ?max_len path = Common.read_file ?max_len !!path
let with_open_outfile path func = Common.with_open_outfile !!path func
let with_open_infile path func = Common.with_open_infile !!path func
let new_temp_file prefix suffix = Common.new_temp_file prefix suffix |> Fpath.v
let erase_temp_files = Common.erase_temp_files
let erase_this_temp_file path = Common.erase_this_temp_file !!path
let is_executable path = Common2.is_executable !!path
let filesize path = Common2.filesize !!path

let filemtime file =
  if !Common.jsoo then failwith "JSOO: File.filemtime"
  else (Unix.stat !!file).Unix.st_mtime

(* TODO? slow, and maybe we should cache it to avoid rereading
 * each time the same file for each match.
 * Note that the returned lines do not contain \n.
 *)
let lines_of_file (start_line, end_line) file : string list =
  let arr = Common2.cat_array (Fpath.to_string file) in
  let lines = Common2.enum start_line end_line in
  lines |> Common.map (fun i -> arr.(i))
