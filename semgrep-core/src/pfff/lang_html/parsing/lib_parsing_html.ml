(* Yoann Padioleau
 *
 * Copyright (C) 2011 Facebook
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

open Ast_html

module V = Visitor_html

(*****************************************************************************)
(* Wrappers *)
(*****************************************************************************)

(*****************************************************************************)
(* Filemames *)
(*****************************************************************************)

let find_html_files_of_dir_or_files xs =
  Common.files_of_dir_or_files_no_vcs_nofilter xs
  |> List.filter (fun filename ->
    let ftype = File_type.file_type_of_file filename in
    match ftype with
    | File_type.PL (File_type.Web (File_type.Html)) -> true
    | _ -> false
  ) |> Common.sort

(*****************************************************************************)
(* AST helpers *)
(*****************************************************************************)

let get_data_any any =
  V.do_visit_with_ref (fun aref -> { V.default_visitor with
                                     V.khtml_tree = (fun (k, _) x ->
                                       match x with
                                       | Data (s, _info) -> Common.push s aref
                                       | _ -> k x
                                     )
                                   }) any

let html_tree_to_html _tree =
  raise Todo
