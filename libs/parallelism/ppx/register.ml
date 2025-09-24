(* Austin Theriault
 *
 * Copyright (C) Semgrep, Inc.
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

let auto = Mapper.auto

open Ppxlib

let () =
  Driver.add_arg "--auto" (Set auto) ~doc:"measure all top-level functions.";
  let impl str = (Mapper.toplevel_mapper !auto)#structure str in
  Ppxlib.Driver.register_transformation "parallelism" ~impl
