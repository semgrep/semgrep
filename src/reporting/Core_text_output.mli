(*
   Copyright (c) 2019-2024 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
(* This file is mostly deprecated. You should use osemgrep text output instead *)

(* this can also display metavars and taint traces *)
val print_match : Semgrep_output_v1_j.core_match -> unit

(* used also in Metavar_replacement.ml *)
val join_with_space_if_needed : string list -> string
