(* Nat Mote, Yoann Padioleau
 *
 * Copyright (C) 2024 Semgrep Inc.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * LICENSE for more details.
 *)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Small wrappers around Marshal to have a more disciplined way to
 * store data in memory or on the disk.
 *)

(*****************************************************************************)
(* Code *)
(*****************************************************************************)

(* What is the use of InMemory marshalling? Sometimes some data structures
 * are using a tons of pointers (e.g., ASTs) and they put a big stress on the
 * GC. Indeed, the GC must traverse all those pointers; if the data is not
 * often used, it's worth it to temporarily marshal the data into a long
 * string (considered a single pointer by the OCaml runtime) and garbage
 * collect the original data. This is a classic OCaml perf trick.
 *)
module InMemory = struct
  type 'a t = string

  let marshal x = Marshal.to_string x []
  let unmarshal x = UMarshal.from_string x 0
end

(* Just a typesafe way to access and remember where the data was stored on
 * the disk.
 *)
module OnDisk = struct
  type 'a t = Fpath.t

  let marshal path x =
    UMarshal_.write_value x path;
    path

  let unmarshal x = UMarshal_.get_value x
  let get_path x = x
end
