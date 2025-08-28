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
(*
   Similar to Gitignore_filter but select paths to be kept rather than ignored.

   Negated patterns are not supported as they wouldn't make much sense here.
   Exclusions can be made via --exclude or gitignore/semgrepignore.

   Example: --include a --include '*.ml'
     /a/b.c  --> selected because /a matches a pattern
     /x/a    --> selected because a matches a pattern (in folder /x)
     x.ml    --> selected because x.ml matches a pattern
     x.c     --> excluded because x.c doesn't match a pattern

   The logic is different from gitignore/semgrepignore and --exclude.
   It is applied before all of these filters.

   In semgrep, this is used in two contexts:
   - pre-filtering of all target files based on the --include command-line
     options
   - per-rule filtering, which is an extra filtering step requested
     by specific semgrep rules
*)

type t [@@deriving show]

(*
   Create a filter from a nonempty list of patterns.
   An empty list of patterns would result in all paths being ignored.
*)
val create : project_root:Fpath.t -> string list -> t

(*
   Run a filter against the path of a file within a project.
   The result indicates whether the path should be selected or ignored
   and provides details for debugging purposes. See the Gitignore module
   for details.
*)
val select : t -> Ppath.t -> Gitignore.status * Gitignore.selection_event list
