(*
   Operations on files in the general sense (regular file, folder, etc.).

   For now, this is a thin layer on top of Common. Eventually, we want
   to get rid of the interface exposed by Common.
*)

type fpath = Fpath.t

(* for ppx deriving *)
let pp_fpath fmt path = Format.pp_print_string fmt (Fpath.to_string path)
let show_fpath path = Fpath.to_string path
let equal_fpath = Fpath.equal

module Operators = struct
  let ( / ) = Fpath.( / )
  let ( // ) = Fpath.( // )
  let ( !! ) = Fpath.to_string
end

open Operators

let of_strings strings = Common.map Fpath.v strings
let to_strings paths = Common.map Fpath.to_string paths
let fullpath file = Common.fullpath !!file |> Fpath.v
let readable ~root path = Common.readable ~root:!!root !!path |> Fpath.v

let files_of_dirs_or_files_no_vcs_nofilter xs =
  xs |> to_strings |> Common.files_of_dir_or_files_no_vcs_nofilter |> of_strings

let input_text_line = Common.input_text_line
let cat path = Common.cat !!path
let write_file path data = Common.write_file !!path data
let read_file ?max_len path = Common.read_file ?max_len !!path
let with_open_outfile path func = Common.with_open_outfile !!path func
let with_open_infile path func = Common.with_open_infile !!path func
let new_temp_file prefix suffix = Common.new_temp_file prefix suffix |> Fpath.v
let erase_temp_files = Common.erase_temp_files
let erase_this_temp_file path = Common.erase_this_temp_file !!path
