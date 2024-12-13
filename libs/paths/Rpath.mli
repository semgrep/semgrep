(* "Real paths" aka "physical paths"
    Absolute and unique filesystem paths as returned by 'Unix.realpath'

   Here is some terminology:
   - relative path: a path relative to a directory
     ex: foo/bar, ./foo/bar, ../foo/../bar
   - absolute path: a path relative to a root (In Unix there is only one root).
     ex: /foo/bar
   - project path: a path relative to a project root (See Ppath.mli)
     ex: also /foo/bar, but this must be interpreted in a project context
   - normalized path: a path that doesn't contain "." or ".." as one of its
     segments (see Fpath documentation for more information on segments)
     ex: foo/bar, or /foo/bar
   - real path: the normalized, absolute path of a file that doesn't have a
     symbolic link as one of its prefixes. This means a real path is a
     canonical value.
     ex: /home/pad/foo/bar, /mnt/volumes/tmp/foo/bar

   Importantly, due to this reliance on `realpath()`, this means
   that all values of this type must be valid paths to *existent* files
   or directories. You cannot use this for something which does not
   yet exist. Use the Fpath library for that instead.

   Also note that even though it's tempting to use Rpath.t inside your
   program, because they provide a canonical representation of a path,
   you should prefer in general to use Fpath.t because users want
   error messages, findings, etc. that contain paths derived from
   what was passed on the command line, or Ppath.t if users want
   findings relative to the root of their project (and not real paths like
   /home/pad/my/long/project/foo/bar). Rfpath.t encapsulates both.

   The name of the module imitates Fpath.ml, and Ppath.ml, but use Rpath.ml
   for Real path.
*)

(* Note that the type below uses a 'private' variant constructor (see
   https://v2.ocaml.org/manual/privatetypes.html#ss%3Aprivate-types-variant)
   which behaves like a regular constructor, but cannot be freely used to
   construct values of the given type. This means that [t] can only be
   constructed via `of_string()` (below), but it can be freely destructed
   via pattern-matching without needing to call `to_string()`. This
   ensures that all values of the type `t` must go through
   `of_string`, and in particular, ensures that they all must be
   validated by `realpath()`.
*)
type t = private Rpath of Fpath.t [@@unboxed] [@@deriving show, eq]

(*
   Resolve a path into physical path i.e. a path that's free of symbolic links.
   This requires the path to exist!

   'Error msg' is returned if the physical path can't be determined.
   This is the case if the file doesn't exist at all, its resolution
   leads a broken symlink, or insufficient permissions.
*)
val of_fpath : Fpath.t -> (t, string) Result.t
val of_string : string -> (t, string) Result.t

(* For tests only. This raises an unspecified exception if the path can't
   be resolved. *)
val of_fpath_exn : Fpath.t -> t
val of_string_exn : string -> t

(* converters *)
val to_fpath : t -> Fpath.t
val to_string : t -> string

(* <=> to_fpath (of_fpath s) *)
val canonical_exn : Fpath.t -> Fpath.t

(*
   Get the current working directory from the system.
   It raises an exception with a slightly better error message than
   'Sys.getcwd' or 'Unix.getcwd' if the current working directory
   no longer exists or is inaccessible for some other reason.
*)
val getcwd : unit -> t

(*
   Return a realpath's parent (without having to consult the filesystem).
   Unlike Fpath.parent, this returns None if the path has no parent rather
   than returning itself.
*)
val parent : t -> t option

(*
   Return whether a path is a prefix of another. Since they're real paths,
   the corresponding files or folders are assumed to exist.

   For example, '/a' contains '/a', '/a/b', and '/a/b/c' but does not
   contain '/', '/c', or '/aa'.
*)
val contains : t -> t -> bool
