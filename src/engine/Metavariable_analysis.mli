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
   Analyze the contents of a string literal bound to a metavariable.
   The analyzer operates of the strings contents after best-effort
   unescaping.
   Return false if the bound value isn't a string literal.
   The predicate can be Entropy.has_high_score.
   The environment is only used for error management, to be able
   to call Match_env.error
   alt: pass an error function instead of the environment.
*)
val analyze_string_metavar :
  Match_env.env ->
  Metavariable.bindings ->
  Metavariable.mvar ->
  (string -> bool) ->
  bool
