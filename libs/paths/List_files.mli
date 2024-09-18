(*
   List files recursively in a safe, efficient, and portable manner
   (should work on Linux, macOS, and also Windows).

   Replaces the functions in libs/commons/ that use external UNIX commands
   such as 'find'.
*)

(*
   List all files recursively. Exclude folders/directories.
   For further filtering based on file type, use 'list_with_stat'.
*)
val list : Fpath.t -> Fpath.t list

(*
   List all regular files recursively. This excludes symlinks, among other.

   If 'keep_root' is true, the root path is included in the result list
   even if it's not a regular file, as long as it's not a folder.
   This is useful to force the use of files that are explicitly specified
   and are not folders, such as named pipes created by bash
   via process substitution (e.g. /dev/fd/63 created by 'echo <(echo hello)'
   or symbolic links which are acceptable only when specified directly
   by the user.

   This does not raise exceptions if path does not exist.
   Moreover while traversing dirs, list_regular_files ignores all
   Unix.Unix_error exceptions raised by Unix.lstat.
*)
val list_regular_files : ?keep_root:bool -> Fpath.t -> Fpath.t list

(*
   List all files recursively. Exclude folders/directories.
   Use List_.map_filter to exclude more file types.
*)
val list_with_stat : Fpath.t -> (Fpath.t * Unix.stats) list

(*
   Iterate over files recursively. Exclude folders/directories.
*)
val fold_left :
  ('acc -> Fpath.t -> Unix.stats -> 'acc) -> 'acc -> Fpath.t -> 'acc

val iter : (Fpath.t -> Unix.stats -> unit) -> Fpath.t -> unit

(* internals *)

(* Read the names found in a directory, excluding "." and "..". *)
val read_dir_entries : Fpath.t -> string list

(* same than read_dir_entries but return single segment Fpath.t *)
val read_dir_entries_fpath : Fpath.t -> Fpath.t list
