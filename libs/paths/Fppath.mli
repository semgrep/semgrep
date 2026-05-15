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
(*
   Path information for a file in a project, which includes:
   - a path in the file system (fpath)
   - the path relative to the project root (ppath)

   Both are useful in different context (e.g., ppath is usually better
   in error messages while fpath for actual file content access).
*)

type t = { fpath : Fpath.t; ppath : Ppath.t } [@@deriving show, eq]
(** A file system path that is expected to be valid accompanied with
    its ppath, the normalized path of the file relative to its project root.

    TODO: make the ppath optional so as to prevent the target file
    from being filtered out with path filters. For now, we use a hack
    based on special file path that indicates our desire to not filter.
*)

val to_fpath : t -> Fpath.t

val compare : t -> t -> int
(** Compare based on the original fpath *)

val add_seg : t -> string -> t
(** Append a single path segment (e.g. a directory or file name from a
    [readdir]) to both the fpath and ppath in lockstep. *)

val walk_dirs :
  ?should_recurse:(Ppath.t -> bool) -> t -> (Ppath.t -> unit) -> unit
(** Recursively walk every subdirectory of [root]. [f] is invoked on
    [root]'s Ppath, then on each subdirectory's Ppath in turn. [f] is
    invoked only on directories (not on regular files, symlinks, or other
    entry types).

    [?should_recurse]: predicate to control which subdirectories to enter.
    Defaults to "always recurse". The Ppath passed to the predicate is the
    Ppath of the candidate child directory; the caller is responsible for
    deciding (e.g. skip [.git] by checking [Ppath.last_segment]).

    Symlinks are not followed: lstat is used to distinguish real directories
    from symlinks-to-directories. Read-directory and lstat errors are
    logged at warn level and the walk continues.

    [Bos.OS.Dir.fold_contents] in principle gives us exactly the mechanism
    we want here.  Here's why we're not making use of it:
    - Bos follows symlinks by default ([Sys.is_directory] under the hood).
      We need [lstat] semantics to match the legacy target-discovery walker
      and maintain the invariant that we ever traverse outside the root of
      the project root.
    - Bos yields [Fpath.t], except that some callers (such as Gitignore_filter)
      operate on [Ppath.t]s, so callers needing a Ppath would have to call
      [Fpath.relativize] per directory. [Fpath.relativize ~root:p p] for a
      relative root can return [Some "../<basename>"] rather than [Some "."],
      which crashes [Ppath.of_relative_fpath_exn]. Maintaining the Fppath
      pair natively (via [add_seg]) avoids the conversion entirely.
    - Bos's [?traverse] predicate fires at every level. Our skip rules are
      simple enough that expressing them via [should_recurse] is cheaper. *)

val append_relative_fpath : t -> Fpath.t -> t
(** Append a relative fpath to an existing root path.
    Raise Invalid_argument is the provided fpath isn't relative. *)

val of_relative_fpath_exn : Fpath.t -> t
(** Create an fppath from a relative fpath.
    Raises Invalid_argument if the input is not a relative path. *)

val of_file_basename : Fpath.t -> t
(** Assumes that the project root is the folder containing this file.
    For example, the ppath of /a/b/c is /c and the ppath of a/b/c
    is also /c. This function is useful to represent single target files that
    are not associated with a particular project.
    This function is intended for paths representing files, not folders,
    but doesn't check the file type.
*)

val fake_from_fpath_DEPRECATED : Fpath.t -> t
(** Safe alternative to [of_relative_fpath]. Using this function is a sign
    that something's modeled incorrectly!
    Correct fppaths are returned by target discovery as done by
    the [Find_targets] module.
*)

val unfilterable_DEPRECATED : Fpath.t -> t
(** Deprecated: rely on the Target module if you need to bypass path filtering.

    Leave the ppath unset to ensure this path won't be filtered out
    with path filters.
    TODO: get rid of this hack. See Target.mli for an alternative.
*)

val is_filterable_DEPRECATED : t -> bool
(** Is the ppath set correctly?
    TODO: make ppath optional rather than requiring this function. *)
