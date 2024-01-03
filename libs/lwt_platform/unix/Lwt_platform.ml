(* Austin Theriault
 *
 * Copyright (C) 2019-2023 Semgrep, Inc.
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

(* Commentary *)
(* Unix specific LWT primitives *)
(* See lwt_platform top level dune for further explanation *)
(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Code *)
(*****************************************************************************)
let run = Lwt_main.run
let detach = Lwt_preemptive.detach
let init_preemptive = Lwt_preemptive.init

let set_engine () =
  if Sys.unix then Lwt_engine.set (new Lwt_engine.libev ())
  else Lwt_engine.set (new Lwt_engine.select)

let sleep = Lwt_unix.sleep
