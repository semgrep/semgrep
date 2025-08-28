(*
   Copyright (c) 2025 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
(* Pretty print a sequence separated by commas:
 *
 * let pp_brackets = pp "[" "]" in
 * pp pp_elt fmt seq
 *
 * -> "[1, 2, 3]"
 *)
val pp :
  string ->
  string ->
  (Format.formatter -> 'elt -> unit) ->
  Format.formatter ->
  'elt Seq.t ->
  unit
