(* Yoann Padioleau
 *
 * Copyright (C) 2010 Facebook
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file license.txt.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * license.txt for more details.
*)

open Common

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * Many header files in the std C++ library do not have any extension,
 * e.g. /usr/include/c++/.../set
 * The goal of this module is to produce a CPP_STDLIB directory
 * that makes sense for codemap and the other tools we use to
 * analyze C++.
 *)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let rename_header_std_files dir =
  let files = Common.files_of_dir_or_files_no_vcs_nofilter [dir] in
  files |> List.iter (fun file ->
    match Common2.dbe_of_filename_safe file with
    | Common2.Left _ -> ()
    | Common2.Right (_dir, _file) ->
        let cmd = spf "mv %s %s" file (file ^ ".hpp") in
        Common2.command2_y_or_no_exit_if_no cmd
  )
