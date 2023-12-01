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
(* Javascript specific LWT primitives *)
(* See lwt_platform top level dune for further explanation *)
(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Code *)
(*****************************************************************************)
(* This Lwt_main.run dropin assumes that some other process ran the promise *)
(* For example in Js_of_ocaml, Javascript's event loop will do this *)
let run t =
  match Lwt.state t with
  | Lwt.Return x -> x
  (* nosem *)
  | Lwt.Fail e -> raise e
  | Lwt.Sleep -> failwith "Lwt_main.run: thread didn't return"

(* TODO: preemptive threads w/node workers *)
let detach f x = f x |> Lwt.return
let init_preemptive _ _ _ = ()
let set_engine () = ()
let (sleep : float -> 'a Lwt.t) = Js_of_ocaml_lwt.Lwt_js.sleep
