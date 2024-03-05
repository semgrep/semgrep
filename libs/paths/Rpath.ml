(* Brandon Wu, Yoann Padioleau
 *
 * Copyright (C) 2022-2024 Semgrep Inc.
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

type t = Rpath of Fpath.t [@@deriving show, eq]

(*****************************************************************************)
(* Main functions *)
(*****************************************************************************)

let of_fpath p = Rpath (Fpath.to_string p |> Unix.realpath |> Fpath.v)
let to_fpath (Rpath x) = x
let canonical s = to_fpath (of_fpath s)

(* deprecated *)
let of_string s = Rpath (Unix.realpath s |> Fpath.v)
let to_string (Rpath x) = Fpath.to_string x
let canonical_string s = to_string (of_string s)
