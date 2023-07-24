(* Martin Jambon
 *
 * Copyright (C) 2023 Semgrep Inc.
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
(* Rule identifiers (rule IDs), e.g., "ocaml.lang.list-no-map".
 *
 * History: this used to be defined in Rule.ml as a simple type alias,
 * but better to provide a precise API to manipulate Rule IDs.
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type t = string [@@deriving show, eq]

exception Malformed_rule_ID of string

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

let to_string x = x

let validate =
  let rex = SPcre.regexp "^[a-zA-Z0-9._-]*$" in
  fun str -> SPcre.pmatch_noerr ~rex str

let sanitize_string str =
  let buf = Buffer.create (String.length str) in
  String.iter
    (function
      | ('a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '.' | '_' | '-') as c ->
          Buffer.add_char buf c
      | _junk -> ())
    str;
  Buffer.contents buf

let of_string x = if not (validate x) then raise (Malformed_rule_ID x) else x
let of_string_opt x = if validate x then Some x else None
let to_string_list x = x
let of_string_list x = x
let compare = String.compare

let ends_with r ~suffix:inc_or_exc_rule =
  r = inc_or_exc_rule
  || Stdcompat.String.ends_with ~suffix:("." ^ inc_or_exc_rule) r
