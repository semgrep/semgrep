(*
   Utilities for creating, scanning, and deleting a hierarchy
   of test files.
*)

(*
   We don't deal with other file types than those three.
   File names must be normal names, i.e. not ".", "..", or "". They may not
   contain slashes or backslashes either.
*)
type t =
  | Dir of string * t list
  | File of string * string
  | Symlink of string * string

(*
   Sort the files in a reasonable order. Useful for comparison purposes.
*)
val sort : t list -> t list

(*
   Return one path per file or symlink the tree.
   'include_dirs=true' will return the path to the folders as well.
*)
val flatten : ?root:Fpath.t -> ?include_dirs:bool -> t list -> Fpath.t list

(*
   Read files from a root. Don't follow symlinks.
   Fail with an exception if we can't read the files or if they're of an
   exotic kind.
*)
val read : Fpath.t -> t list

(*
   Write a file hierarchy into the given root folder,
   which must already exist.
*)
val write : Fpath.t -> t list -> unit

(* Recursive removal (rm -r) *)
val remove : Fpath.t -> unit

val is_dir : Fpath.t -> bool
val is_file : Fpath.t -> bool
val is_symlink : Fpath.t -> bool

(*
   Create a folder specified as a string. This path is parsed. If relative,
   it created relative to the root, which must exist and defaults to
   the process' current directory. If absolute, that path is used
   and all the parent directories up to the root are created if necessary.
*)
val mkdir : ?root:Fpath.t -> Fpath.t -> unit

(* Run a function in a directory, then return to the original directory *)
val with_chdir : Fpath.t -> (unit -> 'a) -> 'a

(*
   Create a temporary directory and pass its path to the function to call.
   The folder is automatically deleted upon exit unless 'persist'
   is true. If 'chdir' is set to true, the current directory is
   set temporarily to the temporary directory.
 *)
val with_tempdir :
  ?persist:bool ->
  ?chdir:bool ->
  (Fpath.t -> 'a) -> 'a
