(* Nat Mote
 *
 * Copyright (C) 2020-2025 Semgrep Inc.
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
(* A safer ref to setup "hooks".
 *
 * A hook is usually a place in a function of a module that can be used to
 * add special behavior that can't be anticipated when the module was designed.
 * This is common practice in Emacs modes for example.
 *)

open Common

(*****************************************************************************)
(* Types *)
(*****************************************************************************)
type 'a t = 'a Domain.DLS.key

(*****************************************************************************)
(* API *)
(*****************************************************************************)

let create v = Domain.DLS.new_key ~split_from_parent:Fun.id (Fun.const v)

let with_hook_set hook v f =
  let old = Domain.DLS.get hook in
  Domain.DLS.set hook v;
  finalize f (fun _ -> Domain.DLS.set hook old)

let get = Domain.DLS.get

(* small helper so we can combine calls to [with_] with [@@]
 * alt: 'let f () = Hook.with_hook_set ... f in let f () = ...'
 *)
let with_ hook v f () = with_hook_set hook v f
