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
module String_set : module type of Sets.String_set
module MvarSet : module type of Sets.String_set

type strings = String_set.t
type mvars = MvarSet.t

(*
   Extract strings and metavariables that occur in the pattern
   (for prefiltering purpose, see Analyze_rule.ml)
*)
val extract_strings_and_mvars :
  ?lang:Lang.t -> interfile:bool -> Pattern.t -> strings * mvars

val extract_specific_strings :
  ?lang:Lang.t -> interfile:bool -> Pattern.t -> strings

(*
   Extract metavariables that occur in an "id position" so that, if we
   encounter a `metavariable-regex` operator on any of those metavariables,
   we can use the corresponding `regex` for pre-filtering.
*)
val extract_mvars_in_id_position :
  ?lang:Lang.t -> interfile:bool -> Pattern.t -> mvars
