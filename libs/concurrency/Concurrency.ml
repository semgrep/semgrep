(* Yoann Padioleau
 *
 * Copyright (C) 2023 Semgrep Inc.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file license.txt.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * license.txt for more details.
 *)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Small set of utilities to deal with Concurrency.
 *
 * related libraries:
 *  - standard Mutex.mli, Condition.mli, Thread.mli, Event.mli
 *  - Base?
 *  - Lwt, Oslo, etc.
 *)

(*****************************************************************************)
(* Mutex *)
(*****************************************************************************)

(* See also Lock_protected.ml *)
let with_lock f lock =
  Mutex.lock lock;
  Common.protect ~finally:(fun () -> Mutex.unlock lock) f
