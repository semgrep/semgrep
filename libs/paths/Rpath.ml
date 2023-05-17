(* Brandon Wu, Yoann Padioleau
 *
 * Copyright (C) 2022-2023 Semgrep Inc.
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

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Real paths.

   This module governs a path type, which can only produce values which
   are for normalized paths to existent files or directories.
   These paths are always canonically kept at their absolute forms, in order to
   maintain a canonicity property. Due to the `Path` constructor being
   private, we can enforce this invariant at the type level.

   history: this file was originally called `Path.ml`, but this conflicts with
   the OCaml compiler libraries. Then it was called FPath.ml but this
   was too close to the Fpath library by Daniel Buenzli. Then it was
   called FilePath.ml, but ultimately got renamed to Rpath.ml, so we would
   have Fpath.ml, Ppath.ml,and finally Rpath.ml. We could also have Dpath.ml
   for directories and that would put us closer to the nice filenames library
   in Scala with better types paths:
   https://www.lihaoyi.com/post/HowtoworkwithFilesinScala.html
*)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* TODO? use Path of Fpath.t instead? *)
type t = Path of string [@@deriving show, eq]

(*****************************************************************************)
(* Main functions *)
(*****************************************************************************)

(* TODO: we should use Unix.realpath but it's available only in 4.13
 * and there is no unixcompat like we have stdcompat.
 * alt: use Common.fullpath, but not as good as Unix.realpath
 *)
let of_fpath p = Path (Realpath.realpath p |> Fpath.to_string)
let of_string s = Path (Realpath.realpath_str s)
let to_fpath (Path s) = Fpath.v s
let to_string (Path s) = s
let canonical s = to_string (of_string s)
let ( / ) (Path s1) s2 = of_string (Filename.concat s1 s2)
let concat = ( / )
let apply ~f (Path s) = f s
let file_exists (Path s) = Sys.file_exists s

(* TODO: probably better to direct people to the File module, and remove those
 * functions. People just have to use 'rpath |> Rpath.to_fpath |> xxx'
 *)
let cat = apply ~f:Common.cat
let read_file ?max_len = apply ~f:(Common.read_file ?max_len)
let write_file ~file:(Path s1) = Common.write_file ~file:s1
let is_directory = apply ~f:Sys.is_directory
let basename (Path s) = Filename.basename s
let dirname (Path s) = Filename.dirname s |> of_string
let extension (Path s) = Filename.extension s
