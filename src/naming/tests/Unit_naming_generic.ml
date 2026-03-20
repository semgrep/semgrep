(*
   Copyright (c) 2021-2025 Semgrep Inc.

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

let tests parse_program =
  Testo.categorize "naming generic"
    [
      t "regression files" (fun () ->
          let dir = Fpath.(tests_path / "naming" / "python") in
          let files1 = Common2.glob (dir / "*.py") in
          let dir = Fpath.(tests_path / "naming" / "go") in
          let files2 = Common2.glob (dir / "*.go") in
          let dir = Fpath.(tests_path / "naming" / "js") in
          let files3 = Common2.glob (dir / "*.js") in
          let dir = Fpath.(tests_path / "naming" / "java") in
          let files4 = Common2.glob (dir / "*.java") in

          files1 @ files2 @ files3 @ files4
          |> List.iter (fun file ->
                 try
                   (* at least we can assert we don't thrown an exn or go
                      into infinite loops *)
                   let ast = parse_program file in
                   let lang = Lang.lang_of_filename_exn file in
                   Naming_AST.resolve lang ast;
                   (* this used to loop forever if you were not handling correctly
                      possible cycles with id_type *)
                   let _v = AST_generic.show_any (AST_generic.Pr ast) in
                   ()
                 with
                 | Parsing_error.Syntax_error _ ->
                     Alcotest.failf "it should correctly parse %s" !!file));
    ]
