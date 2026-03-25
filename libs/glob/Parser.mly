/*
   Copyright (c) 2023-2024 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*/
%{
open Pattern

type tmp =
| Fragment of segment_fragment
| Starstar

let convert_ellipses (fragments : tmp list) =
  match fragments with
  | [Starstar] -> Any_subpath
  | xs ->
     Segment (List.map (function Fragment x -> x | Starstar -> Star) xs)
%}
%token SLASH QUESTION STAR STARSTAR EOF
%token <char> CHAR
%token <Pattern.char_class> CHAR_CLASS

%start <Pattern.segment list> segments
%%

segments:
| frags=list(fragment) SLASH comps=segments
     { convert_ellipses frags :: comps }
| frags=list(fragment) EOF
     { [convert_ellipses frags] }

fragment:
| c=CHAR
     { Fragment (Char c) }
| cc=CHAR_CLASS
     { Fragment (Char_class cc) }
| QUESTION
     { Fragment Question }
| STAR
     { Fragment Star }
| STARSTAR
     { Starstar }
