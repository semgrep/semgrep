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
(* This function's role is to simply see if a metavariable, in the
   context of a range with metavariables, can satisfy some regular
   expression condition. If so, then we return a list of the bindings
   that are produced via the capture groups present in the regular
   expression, such as in the regex
   foo(?<X>[A-Z]+)bar
   which produces a capture group metavariable $X.
   The file is needed so these matches have locations properly localized
   to the originating file, rather than the match.
*)
val get_metavar_regex_capture_bindings :
  Eval_generic.env ->
  file:Fpath.t ->
  Range_with_metavars.t ->
  (* mvar, regex string *)
  Metavariable.mvar * string ->
  Metavariable.bindings list option
