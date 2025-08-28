(*
   Copyright (c) 2023-2025 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
type t = Semgrep_output_v1_t.product [@@deriving ord, show]

(* currently [`SAST; `SCA; `Secrets] *)
val all : t list

(* use the metadata.product field to derive the product of a match *)
val of_cli_match : Semgrep_output_v1_t.cli_match -> t
