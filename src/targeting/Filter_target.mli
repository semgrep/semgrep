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
val filter_target_for_analyzer : Analyzer.t -> Fpath.t -> bool
(** Select the file if it belongs to the language using Guess_lang.ml *)

val filter_paths : Rule.path_filter -> Fppath.t -> bool
(** Determine whether a target file passes the path filters specified
   in a Semgrep rule (paths.exclude, paths.include).
   If the target file doesn't have a proper ppath, the filters can't be applied
   and the result is true.
 *)
