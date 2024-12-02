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
module Out = Semgrep_output_v1_t

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* ?? *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* TODO: factorize with semgrep_output_v1.atd *)
type t =
  | ManifestOnly of Manifest.t
  | LockfileOnly of Lockfile.t
  | ManifestAndLockfile of Manifest.t * Lockfile.t
      (** A source to resolve dependencies from. Can be either a lockfile or a manifest, or both. *)

(*****************************************************************************)
(* Converters *)
(*****************************************************************************)

let manifest_of_semgrep_output ({ path; kind } : Out.manifest) : Manifest.t =
  Manifest.mk_manifest kind path

let lockfile_of_semgrep_output ({ path; kind } : Out.lockfile) : Lockfile.t =
  Lockfile.mk_lockfile kind path

let dependency_source_of_semgrep_output (output_source : Out.dependency_source)
    : t =
  match output_source with
  | ManifestOnlyDependencySource manifest ->
      ManifestOnly (manifest_of_semgrep_output manifest)
  | LockfileOnlyDependencySource lockfile ->
      LockfileOnly (lockfile_of_semgrep_output lockfile)
  | ManifestLockfileDependencySource (manifest, lockfile) ->
      ManifestAndLockfile
        ( manifest_of_semgrep_output manifest,
          lockfile_of_semgrep_output lockfile )
