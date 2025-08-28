(*
   Copyright (c) 2024 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
let with_temp_file ?contents ?persist ?prefix ?suffix ?temp_dir _caps f =
  UTmp.with_temp_file ?contents ?persist ?prefix ?suffix ?temp_dir f

let temp_dir _caps ?temp_dir ?perms prefix suffix =
  UTmp.temp_dir ?temp_dir ?perms prefix suffix

let get_temp_dir_name _caps = UTmp.get_temp_dir_name ()
let erase_temp_files _caps = UTmp.erase_temp_files ()

let new_temp_file ?prefix ?suffix ?temp_dir _caps =
  UTmp.new_temp_file ?prefix ?suffix ?temp_dir ()

let replace_named_pipe_by_regular_file_if_needed _caps =
  UTmp.replace_named_pipe_by_regular_file_if_needed

let replace_stdin_by_regular_file _caps = UTmp.replace_stdin_by_regular_file
