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

   TODO: distinguish the bare rule name 'foo' from the path 'path.to.foo'
 *)
open Sexplib.Std

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type t = string [@@deriving show, eq, sexp]

exception Malformed_rule_ID of string

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

let to_string x = x

let validate =
  let rex = Pcre2_.regexp "^[a-zA-Z0-9._-]*$" in
  fun str -> Pcre2_.pmatch_noerr ~rex str

let sanitize_string str =
  let buf = Buffer.create (String.length str) in
  String.iter
    (function
      | ('a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '.' | '_' | '-') as c ->
          Buffer.add_char buf c
      | _junk -> ())
    str;
  Buffer.contents buf

let of_string_exn x =
  if not (validate x) then raise (Malformed_rule_ID x) else x

let of_string_opt x = if validate x then Some x else None
let to_string_list x = x
let of_string_list x = x
let compare = String.compare

let last_elt_opt x =
  (* ocaml.best-practice.use-obj-magic
   * -> ['ocaml';'best-practice';'use-obj-magic']
   *)
  let rule_name_seperated = String.split_on_char '.' x in
  List_.last_opt rule_name_seperated

let ends_with r ~suffix:inc_or_exc_rule =
  r = inc_or_exc_rule || String.ends_with ~suffix:("." ^ inc_or_exc_rule) r

(* For ATD 'string wrap' in semgrep_output_v1.atd *)
let unwrap = to_string
let wrap = of_string_exn
let dash_e = of_string_exn "-"
