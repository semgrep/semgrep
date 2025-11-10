(*
   Copyright (c) 2024 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
(*
   Unit tests for Regex
*)

let t = Testo.create

let test_match_limit_ok () =
  let rex = Pcre2_.regexp "(a+)+$" in
  match Pcre2_.pmatch ~rex "aaaaaaaaaaaaaaaaa!" with
  | Ok _ -> ()
  | Error Pcre2.MatchLimit ->
      Alcotest.fail "should not have failed with error MatchLimit"
  | Error _ -> Alcotest.fail "unexpected error"

(* IGNORE THIS *)

let tests =
  Testo.categorize "pcre2 settings"
    [
      t "match limit ok" test_match_limit_ok;
    ]
