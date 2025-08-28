(*
   Copyright (c) 2024-2025 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
(*
   Print the matched lines to stdout in a human-readable format.
*)
val print :
  ?highlight:bool ->
  ?print_optional_separator:(unit -> unit) ->
  Src_file.t ->
  Match.match_ list ->
  unit

(*
   Print the results of matching multiple patterns against multiple documents.
*)
val print_nested_results :
  ?with_time:bool ->
  ?highlight:bool ->
  ?print_optional_separator:(unit -> unit) ->
  (Src_file.t
  * (Match.pattern_id * Match.match_ list * float) list
  * float
  * float)
  list ->
  (Src_file.t * Parse_pattern.error) list ->
  unit
