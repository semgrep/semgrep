(*
   Lazy file tree used for path filtering.

   Design principles:
   - This module reads from but doesn't write to the file system. This allows
     us to ignore certain file types without conflicts.
   - This module does not read the contents of files.
   - The file tree and the current directory are assumed to not change
     for the lifetime of the data structures created by this module.
     If this happens, the behavior of this module is undefined.

   The goal of the laziness is to save resources (time, memory) by not
   listing subtrees that we don't need. This is useful when whole subtrees
   are ignored via gitignore files or similar.

   The following kinds of files are supported:
   - folders (directories)
   - regular files
   - symbolic links: supported but not dereferenced. Their destination isn't
     checked, including symlinks pointing to missing files ("dangling") and
     symlinks pointing outside the allowed tree (e.g. git project). Symlinks
     are therefore just another kind of regular files for the sake of this
     module.

   Other file kinds are treated as if they didn't exist.

   Navigation:
   - finding the parent directory:
     * of an absolute path: if the path is a directory, then we remove
       the last component. If the path is not a directory (nonexistent,
       regular file, symlink, other file type), we fail.
     * of a relative path: we append it to the result of getcwd() first.
     This mechanism allows symlinks in the path as long as we don't try
     to access their parents.
   - symlinks, even those pointing to valid directories, are not followed.
   - parent directories denoted '..'
*)

(*
   Encapsulates:
   - the current working directory (physical, no symlinks)
   - a cache of file metadata
*)
type t
type file_kind = Dir | Reg | Lnk

(* A file that exists *)
type file = private {
  (* File path. Could be absolute or relative. *)
  path : Fpath.t;
  (* Absolute path to the file but not necessarily the physical path. *)
  abs_path : Fpath.t;
  kind : file_kind;
  parent : file option Lazy.t;
  children : file list Lazy.t;
}

(*
   Create the caches and other data shared by the whole tree.
   The optional 'cwd' argument specifies the current work directory used
   to resolve relative paths. It is either absolute or relative to
   the process' cwd value. It is converted to the physical path to the
   folder.
*)
val create : ?cwd:Fpath.t -> unit -> t

(* Return the physical path used to resolve relative paths. *)
val cwd : t -> Fpath.t

(* Check the existence of a file from its path *)
val resolve : t -> Fpath.t -> file option

(* Return the parent directory of a file, if any. *)
val parent : file -> file option

(* List the files in the given directory, returning an empty list
   if it's a symlink or a regular file. *)
val children : file -> file list

(* List all the children recursively. Do not follow symlinks. *)
val descendants : file -> file list
