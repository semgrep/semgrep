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

(* Deprecated: use the Ppath.ml module instead! *)
let readable ~root s =
  if Fpath.is_root root then s
  else if Fpath.is_current_dir root then
    if Fpath.is_abs s then
      let path = Fpath.to_string s in
      failwith (spf "file %s shouldn't be absolute when root is ." path)
    else s
  else
    match Fpath.(rem_prefix (v "./")) s with
    | Some p -> p
    | None when Fpath.equal root s -> Fpath.v "."
    | None -> (
        match Fpath.relativize ~root s with
        | Some p -> p
        | None ->
            let root = Fpath.to_string root in
            let path = Fpath.to_string s in
            failwith
              (spf "can't find 'readable' path for project root %S and path %S"
                 root path))

(*****************************************************************************)
(* dbe to filename (was in common2.ml) *)
(*****************************************************************************)

(* filename_prefix_suffix 'aaa.ext1.ext2'
 * ==> ('aaa', 'ext1.ext2')  if ~at_most_one_ext:false
 * ==> ('aaa.ext1','ext2')   if ~at_most_one_ext:true
 *)
let filename_prefix_suffix ~at_most_one_ext (s : string) : string * string =
  let open Base in
  let split_f = if at_most_one_ext then String.rsplit2 else String.lsplit2 in
  match split_f ~on:'.' s with
  | None -> (s, "")
  | Some split -> split

let dbe_of_filename file =
  let prefix, suffix =
    filename_prefix_suffix ~at_most_one_ext:true (Filename.basename file)
  in
  (Filename.dirname file, prefix, suffix)

let filename_of_dbe (dir, base, ext) =
  if ext = "" then Filename.concat dir base
  else Filename.concat dir (base ^ "." ^ ext)

let dbe_of_filename_many_ext_opt file =
  let prefix, suffix =
    filename_prefix_suffix ~at_most_one_ext:false (Filename.basename file)
  in
  if String.equal suffix "" then None
  else Some (Filename.dirname file, prefix, suffix)
