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
(*
   This module is a hack for evaluating string literals.
*)

(*
   Evaluate unquoted, escaped string literal contents.

   abc\\\'\"def  ->  abc\'"def

   Assumes:

   \\ -> \
   \' -> '
   \" -> "

   Other escape sequences are left untouched.

   HACK!

   Different languages use different escaping rules for their string
   literals. This is incorrect but can be useful until each language
   parser exposes unescaped strings.
*)
val approximate_unescape : string -> string
