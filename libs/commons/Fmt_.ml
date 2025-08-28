(*
   Copyright (c) 2023-2025 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* A few helpers for the Fmt library
 *
 * Note that you should try to avoid using the Fmt and Format libraries.
 * Fmt and Format are a bit complicated to use and are needed only
 * when doing complex box-based pretty printing. Otherwise just
 * use sprintf or Console.ml helpers.
 *)

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

let with_buffer_to_string f =
  let buf = Buffer.create 100 in
  let (ppf : Format.formatter) = Format.formatter_of_buffer buf in
  f ppf;
  Format.pp_print_flush ppf ();
  Buffer.contents buf

(* Make a pp function "show-compliant". Equivalent to Fmt.to_to_string.
 * alt: with_buffer_to_string (fun ppf -> pp ppf x)
 *)
let to_show = Fmt.to_to_string

(* Make a show function "pp-compliant" *)
let of_show = Fmt.of_to_string

let () =
  Testo.test "Fmt_.with_buffer_to_string" (fun () ->
      assert (
        with_buffer_to_string (fun ppf -> Format.fprintf ppf "foo") = "foo"))
