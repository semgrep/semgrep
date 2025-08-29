(* Yoann Padioleau
 *
 * Copyright (c) 2025, Semgrep Inc.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * LICENSE for more details.
 *)
open Common

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* A few private type for packages to avoid using 'string' everywhere.

   Unfortunately, having private types forces us to have an mli file that's
   almost identical to the ml file.

   See the mli file for details.
*)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* see mli *)
type name = string [@@deriving eq, ord, show, yojson]

let name x = x
let names xs = xs

module Name_set = Set.Make (struct
  type t = name

  let compare = compare_name
end)

module Name_map = Map.Make (struct
  type t = name

  let compare = compare_name
end)

(* see mli *)
type version = string [@@deriving eq, ord, show, yojson]

let version x = x

(* see mli *)
type version_constraint = string [@@deriving show, eq, yojson]

let version_constraint x = x
let name_and_version x = x

(* see mli *)
type t = { name : name; version : version } [@@deriving eq, ord, show]

let of_strings ~name ~version = { name; version }
let to_string (pkg : t) : string = spf "%s@%s" pkg.name pkg.version
