(*
   Copyright (c) 2024 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
(*
   A dedicated type for Semgrep scanning roots to avoid confusion with
   discovered target files.
*)

(*
   The type of a scanning root.

   A scanning root is a file or a folder that exists. It may be absolute
   or relative to the current working directory. It may be a symbolic
   link, in which case it will be dereferenced when scanning for target files.

   Note that resolving symlinks may be done only after figuring out the
   project root, otherwise the project root would be inferred incorrectly.

   NOTE: This is distinguished type from the below
*)
type t = private Fpath.t [@@deriving show]

(* Conversions from/to fpaths are no-ops. *)
val of_fpath : Fpath.t -> t
val to_fpath : t -> Fpath.t
val of_string : string -> t
val to_string : t -> string
