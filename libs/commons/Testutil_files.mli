(*
   Utilities for creating, scanning, and deleting a hierarchy
   of test files.

   It is recommended for running tests that operate on a file hierarchy.
   This file hierarchy is defined inline alongside the test code, which
   is more convenient than having to manually create actual files and folders
   for each test case. Another advantage is that we can test the structure
   of git repositories without interfering with our project's real
   repository (e.g. the tests can create a '.git' folder and '.gitignore'
   files without restrictions). During the execution of a test with
   'with_tempfiles', the file hierarchy is created in a temporary folder.

   See Unit_semgrepignore.ml for sample usage.
*)

(*
   File tree. A file tree can be easily inspected in OCaml, and can be written,
   read, or deleted in a single function all.

   We don't deal with other file types than those three.
   File names must be normal names, i.e. not ".", "..", or "". They may not
   contain slashes or backslashes either.

   Sample file tree:
     [
       File (".gitignore", "*.c");
       Dir ("src", [
         File ("hello.ml", "print_endline {|hello|}");
         Dir ("tmp", []);
         Symlink ("hello", "hello.ml")
       ])
     ]

   represents this:
     .
     ├── .gitignore
     └── src/
         ├── hello -> hello.ml
         ├── hello.ml
         └── tmp/
*)
type t =
  | Dir of string (* name *) * t list
  | File of string (* name *) * string (* contents *)
  | Symlink of string (* name *) * string (* destination path *)

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
   Read the file tree starting from a root folder. Don't follow symlinks.
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
val with_tempdir : ?persist:bool -> ?chdir:bool -> (Fpath.t -> 'a) -> 'a

(*
   Create files under a temporary root and execute a function in this
   context. See 'with_tempdir' for the meaning of the options.
*)
val with_tempfiles :
  ?persist:bool -> ?chdir:bool -> t list -> (Fpath.t -> 'a) -> 'a
