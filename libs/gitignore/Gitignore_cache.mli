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

type t
(**
   A project usually contains a single toplevel .gitignore, but each subdir
   of this project can also contain a "refining" .gitignore that
   takes precedence. This cache loads the .gitignore files of every folder
   under the project root eagerly during [create], and then exposes a
   read-only lookup interface via [find]. Because the cache is sealed
   (read-only) after [create] returns, it is safe to share across OCaml 5
   domains in parallel target-filtering code.

   It also supports non-standard ignore files (e.g., .semgrepignore).
*)

val create :
  ?gitignore_filenames:Gitignore.gitignore_filename list ->
  project_root:Fpath.t ->
  unit ->
  t
(** Initialize the cache for a project defined by the project root folder.
    Eagerly walks the filesystem under [project_root] (skipping directories
    named [.git] at any depth, and not following symlinks) and pre-populates
    the cache so that subsequent [find] calls are read-only.

    Cost is proportional to the number of directories under [project_root] (one
    readdir + per-entry lstat). Construct once per project; avoid calling this
    in tight loops.  Transient I/O errors are logged and that file's level is
    omitted from the cache.

    See the doc in Gitignore.ml about gitignore_filenames for more
    information on the ?gitignore_filenames parameter below. *)

val find : t -> Ppath.t (* directory *) -> Gitignore_level_index.t option
(** Look up the .gitignore level applicable to target files in the given
    directory. *)
