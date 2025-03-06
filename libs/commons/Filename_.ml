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
      failwith
          (spf "file %s shouldn't be absolute when root is ." path);
    else
      s
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

let (regexp_match : string -> string -> string) =
 fun s re ->
  assert (s =~ re);
  Str.matched_group 1 s

(* updated: added '-' in filesuffix because of file like foo.c-- *)
let (filesuffix : string -> string) =
 fun s ->
  try regexp_match s ".+\\.\\([a-zA-Z0-9_-]+\\)$" with
  | _ -> "NOEXT"

let (fileprefix : string -> string) =
 fun s ->
  try regexp_match s "\\(.+\\)\\.\\([a-zA-Z0-9_]+\\)?$" with
  | _ -> s

(*
let _ = example (filesuffix "toto.c" =$= "c")
let _ = example (fileprefix "toto.c" =$= "toto")
*)

(*
assert (s = fileprefix s ^ filesuffix s)

let withoutExtension s = global_replace (regexp "\\..*$") "" s
let () = example "without"
    (withoutExtension "toto.s.toto" = "toto")
*)

let db_of_filename file = (Filename.dirname file, Filename.basename file)
let filename_of_db (basedir, file) = Filename.concat basedir file

let dbe_of_filename file =
  (* raise Invalid_argument if no ext, so safe to use later the unsafe
   * fileprefix and filesuffix functions (well filesuffix is safe by default)
   *)
  ignore (Filename.chop_extension file);
  ( Filename.dirname file,
    Filename.basename file |> fileprefix,
    Filename.basename file |> filesuffix )

let filename_of_dbe (dir, base, ext) =
  if ext = "" then Filename.concat dir base
  else Filename.concat dir (base ^ "." ^ ext)

let dbe_of_filename_safe file =
  try Either.Left (dbe_of_filename file) with
  | Invalid_argument _ ->
      Either.Right (Filename.dirname file, Filename.basename file)

let dbe_of_filename_nodot file =
  let d, b, e = dbe_of_filename file in
  let d = if d = "." then "" else d in
  (d, b, e)

(* old:
 * let re_be = Str.regexp "\\([^.]*\\)\\.\\(.*\\)"
 * let dbe_of_filename_noext_ok file =
 *   ...
 *   if Str.string_match re_be base 0
 *   then
 *     let (b, e) = matched2 base in
 *     (dir, b, e)
 *
 *  That way files like foo.md5sum.c would not be considered .c
 *  but .md5sum.c, but then it has too many disadvantages because
 *  then regular files like qemu.root.c would not be considered
 *  .c files, so it's better instead to fix syncweb to not generate
 *  .md5sum.c but .md5sum_c files!
 *)

let dbe_of_filename_noext_ok file =
  let dir = Filename.dirname file in
  let base = Filename.basename file in
  (dir, fileprefix base, filesuffix base)

let dbe_of_filename_many_ext_opt file =
  let re_be = Str.regexp "\\([^.]*\\)\\.\\(.*\\)" in
  let dir = Filename.dirname file in
  let base = Filename.basename file in
  if Str.string_match re_be base 0 then
    let b, e = matched2 base in
    Some (dir, b, e)
  else None
