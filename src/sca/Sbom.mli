(*
   Copyright (c) 2024-2025 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
type kind = Semgrep_output_v1_t.sbom_kind = CycloneDXJson
[@@deriving ord, eq, show]

type t = Semgrep_output_v1_t.sbom = {
  kind : kind;
  is_ephemeral : bool;
  path : Fpath.t;
}
[@@deriving ord, show]

val mk_sbom : kind -> Fpath.t -> bool -> t
