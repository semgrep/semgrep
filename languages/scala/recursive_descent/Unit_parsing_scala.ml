(*
   Copyright (c) 2022-2025 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
open Fpath_.Operators

let t = Testo.create

(*****************************************************************************)
(* Unit tests *)
(*****************************************************************************)

(* ran from the root of the semgrep repository *)
let tests_path = Fpath.v "tests"

let tests =
  Testo.categorize "parsing_scala"
    [
      t "regression files" (fun () ->
          let dir = tests_path / "scala" / "parsing" in
          let files = Common2.glob (dir / "*.scala") in
          files
          |> List.iter (fun file ->
                 try
                   let _ = Parse_scala.parse file in
                   ()
                 with
                 | exn ->
                     Alcotest.failf "it should correctly parse %a (exn = %s)"
                       Fpath.pp file (Common.exn_to_s exn)));
      t "rejecting bad code" (fun () ->
          let dir = tests_path / "scala" / "parsing_errors" in
          let files = Common2.glob (dir / "*.scala") in
          files
          |> List.iter (fun file ->
                 try
                   let _ast = Parse_scala.parse file in
                   Alcotest.failf "it should have thrown a Parse_error %a"
                     Fpath.pp file
                 with
                 | Parsing_error.Syntax_error _ -> ()
                 | exn ->
                     Alcotest.failf "throwing wrong exn %s on %a"
                       (Common.exn_to_s exn) Fpath.pp file));
    ]
