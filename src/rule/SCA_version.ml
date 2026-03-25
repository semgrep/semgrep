(*
   Copyright (c) 2024-2025 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
(* The "core of a version": a dot separated list of numbers, like 4.1.6.2.7
 * alt: we could inline it in V of { ... } which is allowed in modern OCaml
 * but Match_SCA_mode.ml has also functions operating just on this type
 * so simpler to have a separate type with a proper name.
 *)
type core = { major : int; minor : int; incrementals : int list }
[@@deriving eq, ord, show]

(* Used in Rule.ml for sca_dependency_pattern and in SCA_dependency.ml *)
type t =
  | V of core
  (* Versions are sometimes listed as arbitrary strings, like a github URL *)
  | Other of string
[@@deriving eq, ord, show { with_path = false }]

(* pretty printer *)
let to_string (v : t) : string =
  match v with
  | Other s -> s
  | V { major; minor; incrementals } ->
      major :: minor :: incrementals
      |> List.map string_of_int |> String.concat "."
