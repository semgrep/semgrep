(*
 * Copyright (c) 2025 Semgrep Inc.
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
(* Utilities for parsing float literals from source text.
 *
 * Many languages allow type suffixes on float literals that need to be
 * stripped before parsing. For example:
 * - Java: 1.0f, 2.0F (float), 3.0d, 4.0D (double)
 * - C/C++: 1.0f, 2.0F (float), 3.0l, 4.0L (long double)
 * - Rust: 1.0f32, 2.0f64 (though Rust has more complex suffix handling)
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type t = float option * Tok.t

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let suffixes_to_strip =
  [
    (* Rust: f32/f64 (check 3-char suffixes first) *)
    "f32";
    "f64";
    (* Java: f/F for float, d/D for double *)
    (* C/C++: f/F for float, l/L for long double *)
    "f";
    "F";
    "d";
    "D";
    "l";
    "L";
  ]

(* Strip common float literal type suffixes from various languages *)
let strip_suffix s =
  let matching_suffix =
    List.find_opt (fun suffix -> String.ends_with ~suffix s) suffixes_to_strip
  in
  match matching_suffix with
  | None ->
      (* No need to strip anything *)
      s
  | Some suffix -> String_.safe_sub s 0 (String.length s - String.length suffix)

(*****************************************************************************)
(* API *)
(*****************************************************************************)

let parse (s, t) =
  let normalized = strip_suffix s in
  (float_of_string_opt normalized, t)
