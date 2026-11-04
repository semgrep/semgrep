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

let generic_program source =
  Hook.with_hook_set Flag_parsing.error_recovery false (fun () ->
      Parse_python.program_of_string source |> Python_to_generic.program)

let lazy_imports () =
  let source =
    "lazy import json, os.path as path\n\
     lazy from .tools import (load as read, save,)\n"
  in
  let stmts = generic_program source in
  Alcotest.(check int) "import count" 3 (List.length stmts);
  List.iter
    (fun stmt ->
      match stmt.AST_generic.s with
      | AST_generic.DirectiveStmt { d_attrs = [ KeywordAttr (Lazy, tok) ]; _ }
        ->
          Alcotest.(check string) "lazy token" "lazy" (Tok.content_of_tok tok)
      | _ -> Alcotest.fail "expected a lazy import directive")
    stmts

let comprehension_kinds () =
  List.iter
    (fun (source, kind) ->
      match generic_program source with
      | [
       { AST_generic.s = ExprStmt ({ e = Comprehension (actual, _); _ }, _); _ };
      ] ->
          Alcotest.(check bool) source true (actual = kind)
      | _ -> Alcotest.fail ("expected comprehension: " ^ source))
    AST_generic.
      [
        ("[*xs for xs in groups]", List);
        ("[*(1, x) for x in xs]", List);
        ("{*xs for xs in groups}", Set);
        ("{*(1, x) for x in xs}", Set);
        ("{**d for d in mappings}", Dict);
        ("(*xs for xs in groups)", Tuple);
        ("(*(1, x) for x in xs)", Tuple);
        ("{x for x in xs}", Set);
        ("{k: v for k, v in pairs}", Dict);
      ]

let tests =
  Testo.categorize "parsing_python"
    [
      t "Python 3.15 lazy imports" lazy_imports;
      t "Python 3.15 unpacking comprehension kinds" comprehension_kinds;
      t "lazy remains an identifier" (fun () ->
          let stmts =
            generic_program
              "lazy = 1\n\
               lazy(lazy)\n\
               obj.lazy\n\
               import lazy as lazy\n\
               from lazy import lazy\n"
          in
          Alcotest.(check int) "statement count" 5 (List.length stmts));
      t "invalid lazy import prefixes" (fun () ->
          List.iter
            (fun source ->
              let rejected =
                try
                  generic_program source |> ignore;
                  false
                with
                | Parsing_error.Syntax_error _ -> true
              in
              Alcotest.(check bool) source true rejected)
            [
              "other import json";
              "other from json import loads";
              "lazy from json import *";
              "lazy from __future__ import annotations";
            ]);
      t "regression files" (fun () ->
          let dir = tests_path / "python" / "parsing" in
          let files = Common2.glob (dir / "*.py") in
          files
          |> List.iter (fun file ->
              Testutil.run !!file (fun () ->
                  Parse_python.parse_program file |> ignore)));
    ]
