(*
   Copyright (c) 2024 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
type t = AST_generic.any [@@deriving show, eq]

(* a few helpers used mostly in Analyze_pattern.ml *)
val is_special_identifier : ?lang:Lang.t -> string -> bool
val is_special_string_literal : string -> bool
val regexp_regexp_string : string
val is_regexp_string : string -> bool
