(* Ben Kettle
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
(* The source of dependency information (usually a lockfile path but can be
 * a lockfile and manifest or sometimes just a manifest).
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type t = Out.dependency_source =
  | ManifestOnly of Manifest.t
  | LockfileOnly of Lockfile.t
  | ManifestLockfile of (Manifest.t * Lockfile.t)
  (* The dependency_source should be LockfileOnly or ManifestLockfile,
   * but not ManifestOnly.
   * Right now this variant is only used by pysemgrep; it is
   * deconstructed in multiple LockfileXxx when calling the dynamic resolver.
   * Note that this variant introduces a series of problems in the Python code
   * because atdpy generates a List[DependencySource] and List are
   * not hashable in Python. We had to define a special hash function
   * for Subproject to avoid hashing the dependency_source.
   * TODO? add a <python repr="tuple"> so the list is converted instead in a
   * Tuple[DependencySource, ...] which is hashable.
   *)
  | MultiLockfile of t list
[@@deriving show]

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(** List all the source files included in the dependency source. *)
let rec source_files (dep_src : t) : Fpath.t list =
  match dep_src with
  | Out.LockfileOnly lockfile -> [ lockfile.path ]
  | Out.ManifestOnly manifest -> [ manifest.path ]
  | Out.ManifestLockfile (manifest, lockfile) ->
      [ manifest.path; lockfile.path ]
  | Out.MultiLockfile sources -> List.concat_map source_files sources
