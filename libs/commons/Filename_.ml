(* Yoann Padioleau
 *
 * Copyright (C) 1998-2023 Yoann Padioleau
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
(* Filenames *)
(*****************************************************************************)

let chop_dirsymbol = function
  | s when s =~ "\\(.*\\)/$" -> Common.matched1 s
  | s -> s

(* pre: prj_path must not contain regexp symbol *)
let filename_without_leading_path prj_path s =
  let prj_path = chop_dirsymbol prj_path in
  if s = prj_path then "."
  else if
    (* Note that we should handle multiple consecutive '/' as in 'path/to//file' *)
    s =~ "^" ^ prj_path ^ "/+\\(.*\\)$"
  then Common.matched1 s
  else
    failwith (spf "cant find filename_without_project_path: %s  %s" prj_path s)

(* Deprecated: use the Ppath.ml module instead! *)
let readable ~root s =
  match root with
  | "/" -> s
  | "." -> (
      match s with
      | s when s =~ "^/" ->
          failwith (spf "file %s shouldn't start with / when root is ." s)
      | s when s =~ "^\\./\\(.*\\)" -> Common.matched1 s
      | _ -> s)
  | _ -> filename_without_leading_path root s
