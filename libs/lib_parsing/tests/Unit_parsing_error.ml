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
   Unit tests for Parsing_error.

   The exception printers registered by Parsing_error feed logs and traces
   that leave the machine, so they must print a token's location but never
   its text.
*)

let t = Testo.create
let source = "API_KEY = 'hunter2'"

let tok : Tok.t =
  OriginTok
    {
      str = source;
      pos = { bytepos = 0; line = 3; column = 4; file = Fpath.v "secret.py" };
    }

let test_printer_omits_token_text () =
  [
    Parsing_error.Lexical_error ("bad char", tok);
    Parsing_error.Syntax_error tok;
    Parsing_error.Ast_builder_error ("bad ast", tok);
    Parsing_error.Other_error ("other", tok);
  ]
  |> List.iter (fun exn ->
      let s = Printexc.to_string exn in
      if String_.contains ~term:"hunter2" s then
        Alcotest.failf "printer leaked token text: %s" s;
      if not (String_.contains ~term:"secret.py:3:4" s) then
        Alcotest.failf "printer dropped token location: %s" s)

let tests =
  Testo.categorize "Parsing_error"
    [ t "printer omits token text" test_printer_omits_token_text ]
