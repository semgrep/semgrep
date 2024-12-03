(* Cooper Pierce
 *
 * Copyright (c) 2024, Semgrep Inc.
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

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Lockfile kind and path (e.g., package-lock.json in the NPM ecosystem).
 *
 * This module is just to designate a lockfile. The actual parsed content
 * of a lockfile is defined in SCA_dependency.ml (and Lockfile_xtarget.ml)
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)
(* TODO? add a manifest option in it? to know the origin of the lockfile?
 * old: used to be path : Target.path but no need complex origin for manifest
 *)
type t = Semgrep_output_v1_t.lockfile [@@deriving show]

(*****************************************************************************)
(* API *)
(*****************************************************************************)

let mk_lockfile kind (path : Fpath.t) : t = { path; kind }
