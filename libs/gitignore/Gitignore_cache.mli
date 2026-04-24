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

type t
(**
   A project usually contains a single toplevel .gitignore, but each subdir
   of this project can also contain a "refining" .gitignore that
   takes precedence. This cache loads and memoizes the .gitignore files
   of any folder that was consulted during a scan. It also supports
   non-standard ignore files (e.g., .semgrepignore).
*)

(* Initialize the cache for a project defined by the project root folder.
   See the doc in Gitignore.ml about gitignore_filenames for more information
   on the ?gitignore_filenames parameter below.
*)
val create :
  ?gitignore_filenames:Gitignore.gitignore_filename list ->
  project_root:Fpath.t ->
  unit ->
  t

(*
   Load (or get it back from the cache) the .gitignore files applicable to
   target files in the given folder.
*)
val load : t -> Ppath.t (* directory *) -> Gitignore_level_index.t option
