(*
   Copyright (c) 2023-2024 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
(* This module is used mostly to decide between pysemgrep and osemgrep.
 * It could be used for different things later.
 *)

type t =
  (* mix of pysemgrep and osemgrep, depending on CLI arguments (see CLI.ml) *)
  | Default
  (* for forcing pysemgrep *)
  | Legacy
  (* for forcing osemgrep *)
  | Experimental
  (* Leaving on the edge, using osemgrep with osemgrep-only features enabled *)
  | Develop
[@@deriving show]

(* --experimental/--legacy/--develop CLI processing *)
val o_maturity : t Cmdliner.Term.t
