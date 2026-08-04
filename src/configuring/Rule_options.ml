(*
   Copyright (c) 2023-2024 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
(* alt: we could move also this file under src/rule/ now *)

type t = Rule_options_t.t

let default = Rule_options_j.t_of_string "{}"

let pp fmt t =
  let s = Rule_options_j.string_of_t t in
  Format.fprintf fmt "%s" s

(* Equality via JSON serialization — sufficient for rule-parsing diff
   harnesses, since two options with the same effect serialize identically. *)
let equal a b =
  String.equal (Rule_options_j.string_of_t a) (Rule_options_j.string_of_t b)
