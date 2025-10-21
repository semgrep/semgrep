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
type 'a t = 'a Lazy.t * 'a Eio.Lazy.t

(* Protect is closest to normal Lazy behavior *)
let default_cancel_kind = `Protect
let from_val v = (Lazy.from_val v, Eio.Lazy.from_val v)

let from_fun ?(cancel = default_cancel_kind) f =
  (Lazy.from_fun f, Eio.Lazy.from_fun ~cancel f)

(* From https://github.com/ocaml-multicore/eio/issues/800:

    One potential problem with using Fiber.check (or Fiber.get) in application
    code for this is that if OCaml gets thread-local storage then Eio might stop
    using an effect for this.
*)
let is_eio_context () =
  try
    let _ = Eio.Fiber.check () in
    true
  with
  | Stdlib.Effect.Unhandled _ -> false

let force ((l, el) : 'a t) : 'a =
  if is_eio_context () then Eio.Lazy.force el else Lazy.force l

let map ?(cancel = default_cancel_kind) f x =
  let l, el = x in
  (Lazy.map f l, Eio.Lazy.from_fun ~cancel (fun () -> f (Eio.Lazy.force el)))

let is_val ((l, _el) : 'a t) =
  Lazy.is_val l || false (* Eio.Lazy.is_val el does not exist *)
