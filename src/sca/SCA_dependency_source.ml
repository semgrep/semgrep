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

type t = Out.dependency_source [@@deriving show]

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(** List all the source files included in the dependency source. *)
let rec source_files (dep_src : t) : Fpath.t list =
  match dep_src with
  | Out.LockfileOnlyDependencySource lockfile -> [ lockfile.path ]
  | Out.ManifestOnlyDependencySource manifest -> [ manifest.path ]
  | Out.ManifestLockfileDependencySource (manifest, lockfile) ->
      [ manifest.path; lockfile.path ]
  | Out.MultiLockfileDependencySource sources ->
      List.concat_map source_files sources
