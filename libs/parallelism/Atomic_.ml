(* Nathan Taylor
 *
 * Copyright (C) 2025 Semgrep Inc.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * LICENSE for more details.
 *)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Utilities for common atomic and lock-free idioms. *)

(*****************************************************************************)
(* API *)
(*****************************************************************************)

let rec cons x al =
  let l = Atomic.get al in
  let l' = x :: l in
  if not (Atomic.compare_and_set al l l') then
    cons x al (* TODO: backoff strategy? *)
