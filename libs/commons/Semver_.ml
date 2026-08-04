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
type t = Semver.t

(* we don't have access to Semver code so can't add the deriving there
 * so we need to roll our own boilerplate imitating what
 * a simple [@@deriving show, ord] would do above.
 *)
let pp fmt x = Format.pp_print_string fmt (Semver.to_string x)
let compare = Semver.compare
let show = Semver.to_string
let equal a b = Semver.compare a b = 0
