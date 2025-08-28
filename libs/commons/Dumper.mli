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
(* Dump an OCaml value into a printable string.
 * By Richard W.M. Jones (rich@annexia.org).
 * Dumper.mli 1.1 2005/02/03 23:07:47 rich Exp
 *)

(* Dump any OCaml data-structure in a string. dump() relies on the Obj module
 * internally so it is limited (e.g., it just dumps numbers for constructors).
 * You should use instead 'deriving show' which correctly handle
 * constructors, fields, etc. However, if you can't use 'deriving show',
 * then this function helps.
 *)
val dump : 'a -> string
