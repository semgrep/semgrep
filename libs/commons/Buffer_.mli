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
(* Small extension to the official Buffer.ml module.
 * See also Immutable_buffer.ml
 *)

(* [with_buffer_to_string f] will call [f] with a new buffer and once
 * its computation is done it will return the content of this buffer.
 * You can use this function with Printf.bprintf as in:
 *
 *   Buffer_.with_buffer_to_string (fun buf ->
 *     let prf fmt = Printf.bprintf buf fmt in
 *     prf "%d" i;
 *     prf "%s" str;
 *     ...
 *   )
 *)
val with_buffer_to_string : (Buffer.t -> unit) -> string
