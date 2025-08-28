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
open Common
module Out = Semgrep_output_v1_t

type kind = Out.sca_match_kind [@@deriving ord]

let pp_kind fmt _kd = Format.fprintf fmt "SCA_match.kind: TODO"
let equal_kind kd1 kd2 = kd1 =*= kd2

type t = {
  (* the actual dependency in the lockfile *)
  dep : Dependency.t;
  (* the version constraint on a package and its ecosystem *)
  pat : SCA_pattern.t;
  kind : kind;
}
[@@deriving eq, ord, show]
