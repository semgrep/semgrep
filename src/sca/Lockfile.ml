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
(* Lockfile (e.g., package-lock.json in the NPM ecosystem)
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)
(* TODO: factorize with semgrep_output_v1.atd lockfile type
 * TODO? add a manifest option in it? to know the origin of the lockfile?
 * old: used to be path : Target.path but no need complex origin for manifest
 *)
type t = { path : Fpath_.t; kind : Lockfile_kind.t } [@@deriving show, yojson]

(*****************************************************************************)
(* API *)
(*****************************************************************************)

let mk_lockfile kind (path : Fpath.t) : t = { path; kind }
