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
