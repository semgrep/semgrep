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
(*
   Fast check that tells whether a pattern has any chance of matching the
   document with 'Match.search'.

   This checks that all the required literals in the pattern exist somewhere
   in the document.
*)

(*
   If this returns false, it's guaranteed that 'Match.search' would return
   no matches.
*)
val may_match : case_sensitive:bool -> Pattern_AST.t -> Doc_AST.t -> bool
