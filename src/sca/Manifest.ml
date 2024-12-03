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
(* SCA manifest file (e.g., a package.json for the NPM ecosystem), which is
 * different from a lock file (e.g., a package-lock.json for NPM).
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* TODO: factorize with Out.manifest
 * old: used to be path : Target.path but no need complex origin for manifest
 *)
type t = { path : Fpath_.t; kind : Manifest_kind.t } [@@deriving show]
(** A manifest file to be used during matching. See also
    {!Lockfile_xtarget.manifest}, which also has the contents. *)

(*****************************************************************************)
(* API *)
(*****************************************************************************)

let mk_manifest (kind : Manifest_kind.t) (path : Fpath.t) : t = { path; kind }
