(*
   Copyright (c) 2024 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
(* !! UNSAFE !!
 *
 * at least add a version number in 'a and marshall a pair (version, data)
 * in write_value(), so at least after get_value() you can double
 * check that the data can still be read.
 *)
val get_value : Fpath.t -> 'a
val write_value : 'a -> Fpath.t -> unit
