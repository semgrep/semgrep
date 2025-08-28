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
(* Initialize the cache for a project defined by the project root folder.
   See the doc in Gitignore.ml about gitignore_filenames for more information
   on the ?gitignore_filenames parameter below.
*)
val create :
  ?gitignore_filenames:Gitignore.gitignore_filename list ->
  project_root:Fpath.t ->
  unit ->
  Gitignore.gitignores_cache

(*
   Load (or get it back from the cache) the .gitignore files applicable to
   target files in the given folder.
*)
val load :
  Gitignore.gitignores_cache ->
  Ppath.t (* directory *) ->
  Gitignore.level option
