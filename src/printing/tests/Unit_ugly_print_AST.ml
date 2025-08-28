(*
   Copyright (c) 2022-2024 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
open Common

let t = Testo.create

let test_python_printer () =
  let printer = new Ugly_print_AST.python_printer in
  let check (source, expected) =
    (* Parse as a pattern to allow us to test printing snippets of code *)
    let ast = Parse_pattern.parse_pattern Lang.Python source |> Result.get_ok in
    match printer#print_any ast with
    | Error e -> failwith (spf "Couldn't print `%s`:\n%s" source e)
    | Ok actual ->
        let actual = Immutable_buffer.to_string actual in
        Alcotest.(check string) source expected actual
  in
  List.iter check [ ("foo", "foo"); ("foo()", "foo()") ]

let tests =
  Testo.categorize "ugly printer"
    [ t "test python printer" test_python_printer ]
