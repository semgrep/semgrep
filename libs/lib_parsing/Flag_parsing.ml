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
(* TODO: we should probably get rid of this file.
 * You can switch to Logs src if you want logging for your parser.
 *)

let verbose_lexing = Hook.create false
let verbose_parsing = Hook.create true

(* see Parse_info.lexical_error helper and Lexical_error exn *)
(* nosemgrep: no-ref-declarations-at-top-scope *)
let exn_when_lexical_error = ref true

(* Do not raise an exn when a parse error but use NotParsedCorrectly.
 * If the parser is quite complete, it's better to set
 * error_recovery to false by default and raise a true ParseError exn.
 * This can be used also in testing code, to parse a big set of files and
 * get statistics (e.g., -parse_java) and not stop at the first parse error.
 *)
let error_recovery = Hook.create false
let debug_lexer = Hook.create false
let debug_parser = Hook.create false

(* TODO: definitely switch to Logs src for that *)
let show_parsing_error = Hook.create true

(* will lexer $X and '...' tokens, and allow certain grammar extension
 * see sgrep_guard() below.
 *)
let sgrep_mode = Hook.create false

let cmdline_flags_verbose () =
  [
    ("-no_verbose_parsing", Hook.Arg.clear verbose_parsing, "  ");
    ("-no_verbose_lexing", Hook.Arg.clear verbose_lexing, "  ");
  ]

let cmdline_flags_debugging () =
  [
    ("-debug_lexer", Hook.Arg.set debug_lexer, " ");
    ("-debug_parser", Hook.Arg.set debug_parser, " ");
  ]

let sgrep_guard v = if Hook.get sgrep_mode then v else raise Parsing.Parse_error
