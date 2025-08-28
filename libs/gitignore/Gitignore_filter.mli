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
   Create a gitignore filter meant to be reused to filter many target paths.

   gitignore_filenames: set this option to consult other files than
                        '.gitignore'.

*)
val create :
  ?gitignore_filenames:Gitignore.gitignore_filename list ->
  ?higher_priority_levels:Gitignore.level list ->
  ?lower_priority_levels:Gitignore.level list ->
  project_root:Fpath.t ->
  unit ->
  Gitignore.filter

(*
   Examine a single absolute[1] path[2] and determine whether it is selected
   by the gitignore mechanism, i.e. ignored for git purposes.

   [1] The path must be absolute within the git project. For example,
   if the git project root is at /home/bob/fooproj, then
   the path to the file /home/bob/fooproj/bar
   must be given as /bar (hence the use of Ppath.t below).

   [2] Paths to folders must have a trailing slash.

   Return the status and the list of selection/deselection events that the path
   went through, in reverse order. The first element of the list, if any,
   determines whether the file is selected.
*)
val select :
  Gitignore.filter ->
  Ppath.t ->
  Gitignore.status * Gitignore.selection_event list
