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
(* like Uri.of_string but instead of silently returning an
 * empty uri in case of error, we return None here.
 *)
val of_string_opt : string -> Uri.t option

(* rely on Uri.pp *)
val show : Uri.t -> string
val of_fpath : Fpath.t -> Uri.t

(* Checks if the string starts with 'http://' or 'https://'
 * Returns true only for valid HTTP(S) URL prefixes.
 * Note: This only validates the scheme prefix, not the full URL structure.
 *)
val is_url : string -> bool
