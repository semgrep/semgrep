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
(* Lockfile kind and path (e.g., package-lock.json in the NPM ecosystem).
 *
 * This module is just to designate a lockfile. The actual parsed content of a
 * lockfile is defined in SCA_dependency.ml (and Dependency_source_xtarget.ml)
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type kind = Out.sbom_kind = CycloneDXJson [@@deriving ord, eq, show]

type t = Out.sbom = { kind : kind; is_ephemeral : bool; path : Fpath.t }
[@@deriving ord, show]

(*****************************************************************************)
(* API *)
(*****************************************************************************)

let mk_sbom kind (path : Fpath.t) is_ephemeral : t =
  { kind; is_ephemeral; path }
