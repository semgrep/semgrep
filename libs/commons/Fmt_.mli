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
val with_buffer_to_string : (Format.formatter -> unit) -> string

(* Make a pp function "show-compliant" (equivalent to Fmt.to_to_string) *)
val to_show : 'a Fmt.t -> 'a -> string

(* Make a show function "pp-compliant" (equivalent to Fmt.of_to_string) *)
val of_show : ('a -> string) -> 'a Fmt.t
