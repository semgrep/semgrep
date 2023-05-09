(* Real paths.

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
   you should prefer in general to use Ppath.t because users want
   error messages, findings, etc. that refer to paths relative to the
   root of their project (and not /home/pad/my/long/project/foo/bar).

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
type t = private Path of string [@@deriving show, eq]

(* only way to build an Rpath *)
val of_string : string -> t
val to_string : t -> string

(* <=> to_string (of_string s) *)
val canonical : string -> string

(* Similar to Fpath.(/) *)
val ( / ) : t -> string -> t
val concat : t -> string -> t
val apply : f:(string -> 'a) -> t -> 'a

(* Similar to functions in Filename.mli *)
val basename : t -> string
val dirname : t -> t
val extension : t -> string

(* Similar to functions in File.mli *)
val cat : t -> string list
val write_file : file:t -> string -> unit
val read_file : ?max_len:int -> t -> string
val file_exists : t -> bool
val is_directory : t -> bool
