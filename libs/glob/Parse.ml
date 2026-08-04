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
(*
   Parse a glob pattern
*)

(* This normalization is for compatibility with Semgrepignore v1.
   It's not supported by Gitignore and should not be supported
   by Semgrepignore v2 in the long term.

   If the pattern starts with './', we remove the '.'.
   Deeper normalization is not attempted for simplicity and because users
   don't have '..' or other occurrences of '.' in their patterns.

   TODO: emit a deprecation warning
   TODO: remove support after a deprecation period
*)
let normalize_pattern (pat : Pattern.t) =
  match pat with
  | [ Segment [ Char '.' ] ] as pat -> pat
  | Segment [ Char '.' ] :: pat -> Segment [] :: pat
  | pat -> pat

let parse_string ?(deprecated_absolute_dotslash = false) str =
  let lexbuf = Lexing.from_string str in
  let pat = Parser.segments Lexer.token lexbuf in
  if deprecated_absolute_dotslash then normalize_pattern pat else pat
