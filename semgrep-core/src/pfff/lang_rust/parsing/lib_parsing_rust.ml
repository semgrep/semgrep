(* Yoann Padioleau
 *
 * Copyright (C) 2014 Facebook
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

module Ast = Ast_rust
module Flag = Flag_parsing
module FT = File_type
(* module V = Visitor_rust *)

(*****************************************************************************)
(* Wrappers *)
(*****************************************************************************)

(*****************************************************************************)
(* Filemames *)
(*****************************************************************************)

let find_source_files_of_dir_or_files xs =
  Common.files_of_dir_or_files_no_vcs_nofilter xs
  |> List.filter (fun filename ->
    let ftype = File_type.file_type_of_file filename in
    match ftype with
    | FT.PL (FT.Rust) -> true
    | _ -> false
  ) |> Common.sort
