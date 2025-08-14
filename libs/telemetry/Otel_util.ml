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
(* Otel_util has some misc helper functions shared between modules *)
(*****************************************************************************)
(* Code *)
(*****************************************************************************)

module Otel = Opentelemetry

(* We copy these conv value functions from the otel library (opentelemetry.ml)
   so we can wrap the metrics emit functions in a nicer way. When we upstream
   this we could just make the types nicer there *)
let _conv_value =
  let open Otel.Proto.Common in
  function
  | `Int i -> Some (Int_value (Int64.of_int i))
  | `String s -> Some (String_value s)
  | `Bool b -> Some (Bool_value b)
  | `Float f -> Some (Double_value f)
  | `None -> None

let _conv_key_value (k, v) =
  let open Otel.Proto.Common in
  let value = _conv_value v in
  default_key_value ~key:k ~value ()

let _value_conv =
  let open Otel.Proto.Common in
  function
  | Some (Int_value i) -> `Int (Int64.to_int i)
  | Some (String_value s) -> `String s
  | Some (Bool_value b) -> `Bool b
  | Some (Double_value f) -> `Float f
  | Some (Array_value _)
  | Some (Bytes_value _)
  | Some (Kvlist_value _)
  | None ->
      `None

let _key_value_conv kv =
  let open Otel.Proto.Common in
  (kv.key, _value_conv kv.value)
