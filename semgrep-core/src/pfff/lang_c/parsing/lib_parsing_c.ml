(* Yoann Padioleau
 *
 * Copyright (C) 2012 Facebook
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

module FT = File_type
(*module V = Visitor_c*)

(*****************************************************************************)
(* Filenames *)
(*****************************************************************************)

let find_source_files_of_dir_or_files xs =
  Common.files_of_dir_or_files_no_vcs_nofilter xs
  |> List.filter (fun filename ->
    match File_type.file_type_of_file filename with
    | FT.PL (FT.C ("l" | "y")) -> false
    | FT.PL (FT.C _) ->
        (* todo: fix syncweb so don't need this! *)
        not (FT.is_syncweb_obj_file filename)
    | _ -> false
  ) |> Common.sort


(*****************************************************************************)
(* ii_of_any *)
(*****************************************************************************)
(*
let ii_of_any any =
  let globals = ref [] in
  let visitor = V.mk_visitor { V.default_visitor with
    V.kinfo = (fun (_k,_) i -> Common.push i globals)
  }
  in
  visitor any;
  List.rev !globals
*)
