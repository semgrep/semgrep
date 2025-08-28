(*
   Copyright (c) 2023-2024 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
{
module M = Pattern
open Parser

exception Syntax_error of string

let syntax_error msg =
  raise (Syntax_error msg)
}

let neg = ['^' '!']

rule token = parse
| '/'+     { SLASH }
| "**"     { (* only special if it occupies a whole path segment. This
                is dealt with later. *)
             STARSTAR }
| '*'      { STAR }
| '?'      { QUESTION }
| '[' (neg? as negation)
      ('['? as literal_bracket)
           {
             let range_acc =
               if literal_bracket <> "" then
                 [M.Class_char '[']
               else
                 []
             in
             let ranges = char_class range_acc lexbuf in
             let complement = (negation <> "") in
             CHAR_CLASS { complement; ranges }
           }
| '\\' (_ as c)
           { CHAR c }
| [^ '/' '*' '?' '[' '\\'] as c
           { CHAR c }
| eof      { EOF }

and char_class acc = parse
| ']'      { List.rev acc }
| ([^']'] as a) '-' ([^']'] as b)
           { char_class (Range (a, b) :: acc) lexbuf }
| [^']'] as c
           { char_class (Class_char c :: acc) lexbuf }
| eof      { syntax_error "malformed glob pattern: missing ']'" }
