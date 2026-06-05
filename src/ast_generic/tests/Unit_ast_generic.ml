(*
   Copyright (c) 2026 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
(*
   Unit tests for {!AST_generic.normalize_windows_float_exponent}.
*)

let t = Testo.create

let check input expected () =
  Alcotest.(check string)
    (Printf.sprintf "%S" input)
    expected
    (AST_generic.normalize_windows_float_exponent input)

let test_windows_three_digit_exponent () =
  (* Windows [%F] / [string_of_float] padding (see [Unit_external] and numbers.yml). *)
  check "1.23e-010" "1.23e-10" ();
  check "1.23e+034" "1.23e+34" ();
  check "1.5e-010" "1.5e-10" ();
  check "1.0e+013" "1.0e+13" ()

let test_unchanged () =
  check "42." "42." ();
  check "1.5" "1.5" ();
  check "1.5e-10" "1.5e-10" ();
  check "1.5e+10" "1.5e+10" ();
  check "infinity" "infinity" ();
  check "neg_infinity" "neg_infinity" ();
  check "nan" "nan" ()

let test_three_digit_exponent_preserved () =
  (* Unix [%F] already uses three exponent digits when needed (no leading-zero pad).
     These must not be shortened by the Windows normalizer. *)
  check "1e+100" "1e+100" ();
  check "1e-100" "1e-100" ();
  check "1.23e+34" "1.23e+34" ();
  check "1.23e-34" "1.23e-34" ();
  check "1e+123" "1e+123" ()

let test_windows_padding_to_three_digit_exponent () =
  (* Windows may left-pad the exponent with an extra [0] even when the value needs
     three digits (e.g. [e+0100] for exponent 100). Strip only the padding zero. *)
  check "1e+0100" "1e+100" ();
  check "1e-0100" "1e-100" ();
  check "1.23e+0100" "1.23e+100" ();
  check "1.0e+0123" "1.0e+123" ();
  (* Two-digit exponent with Windows padding: only the extra [0] is removed. *)
  check "1.0e+099" "1.0e+99" ()

let tests =
  Testo.categorize "AST_generic.normalize_windows_float_exponent"
    [
      t "windows three-digit exponent" test_windows_three_digit_exponent;
      t "unchanged strings" test_unchanged;
      t "three-digit exponent preserved" test_three_digit_exponent_preserved;
      t "windows padding to three-digit exponent"
        test_windows_padding_to_three_digit_exponent;
    ]
