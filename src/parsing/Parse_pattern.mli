(*
   Copyright (c) 2020-2025 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
val parse_pattern :
  ?rule_options:Rule_options_t.t ->
  Lang.t ->
  string ->
  (Pattern.t, string) Result.t

val dump_tree_sitter_pattern_cst : Lang.t -> Fpath.t -> unit
