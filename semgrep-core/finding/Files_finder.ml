(*s: semgrep/finding/Files_finder.ml *)
(*s: pad/r2c copyright *)
(* Yoann Padioleau
 *
 * Copyright (C) 2019-2020 r2c
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
(*e: pad/r2c copyright *)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)
(*s: function [[Files_finder.files_of_dirs_or_files]] *)
let files_of_dirs_or_files xs =
  Common.files_of_dir_or_files_no_vcs_nofilter xs
(*e: function [[Files_finder.files_of_dirs_or_files]] *)
(*e: semgrep/finding/Files_finder.ml *)
