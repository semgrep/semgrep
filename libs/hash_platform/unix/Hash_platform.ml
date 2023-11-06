(* Brandon Wu
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
(* This Lwt_main.run dropin assumes that some other process ran the promise *)
(* For example in Js_of_ocaml, Javascript's event loop will do this *)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Code *)
(*****************************************************************************)

let hash s =
  (* Digestif.BLAKE2B.digest_string s |> Digestif.BLAKE2B.to_hex
  *)
  Common.spf "hashing %s as unix" s
