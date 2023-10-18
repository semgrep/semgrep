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
   maintain a canonicity property. Due to the `Rpath` constructor being
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

(* TODO? use of Fpath.t instead? or too confusing? *)
type t = Rpath of string [@@deriving show, eq]

(*****************************************************************************)
(* Main functions *)
(*****************************************************************************)

let of_fpath p = Rpath (Fpath.to_string p |> Unix.realpath)
let of_string s = Rpath (Unix.realpath s)
let to_fpath (Rpath s) = Fpath.v s
let to_string (Rpath s) = s
let canonical s = to_string (of_string s)
let ( / ) (Rpath s1) s2 = of_string (Filename.concat s1 s2)
let concat = ( / )
let apply ~f (Rpath s) = f s
let basename (Rpath s) = Filename.basename s
let dirname (Rpath s) = Filename.dirname s |> of_string
let extension (Rpath s) = Filename.extension s

(* TODO: probably better to direct people to the File module, and remove those
 * functions. People just have to use 'rpath |> Rpath.to_fpath |> xxx'
 *)
let is_directory = apply ~f:Sys.is_directory
let file_exists = apply ~f:Sys.file_exists
