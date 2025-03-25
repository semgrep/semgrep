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

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* A few type aliases for packages to avoid using 'string' everywhere *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* package name (e.g., "lodash")
 * alt: could use a newtype with 'PackageName of string'
 *)
type name = string [@@deriving show, eq]

(* package version (e.g., "1.1.0")
 * see also SCA_version.t which is its parsed form.
 * Those strings usually appear in lockfiles.
 *)
type version = string [@@deriving show, eq]

(* ex: "^1.1.0" or "~1.1.0" in yarn.lock and package.json
 * This can also be a single version as in "1.1.0". Those strings
 * usually appear in manifests.
 *)
type version_constraint = string [@@deriving show, eq]

(* See also SCA_dependency.t which specifies the ecosystem, URI,
 * and location in a lockfile.
 * This is mostly the same type that dependency_child in semgrep_output_v1.atd
 *)
type t = { name : name; version : version } [@@deriving show, eq]
