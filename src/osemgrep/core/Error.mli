(*
   Copyright (c) 2022-2024 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
(* See also Core_error.ml and semgrep_output_v1.atd error_type *)

exception Semgrep_error of string * Exit_code.t option

(* Please avoid the name 'Exit' since it's already a standard exception. *)
exception Exit_code of Exit_code.t

(* shortcut *)
val abort : string -> 'a
val exit_code_exn : Exit_code.t -> 'a

(* used for CLI text output and for the metrics payload.errors.errors *)
val string_of_error_type : Semgrep_output_v1_t.error_type -> string
