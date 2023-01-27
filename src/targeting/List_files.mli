(*
   List files recursively in a safe, efficient, and portable manner.

   Replaces the functions in libs/commons/ that use external UNIX commands
   such as 'find'.
*)

type path = string

(*
   List all files recursively. Exclude folders/directories.
   For further filtering based on file type, use 'list_with_stat'.
*)
val list : path -> path list

(*
   List all regular files recursively. This excludes symlinks, among other.

   If 'keep_root' is true, the root path is included in the result list
   even if it's not a regular file, as long as it's not a folder.
   This is useful to force the use of files that are explicitly specified
   and are not folders, such as named pipes created by bash
   via process substitution (e.g. /dev/fd/63 created by 'echo <(echo hello)'
   or symbolic links which are acceptable only when specified directly
   by the user.
*)
val list_regular_files : ?keep_root:bool -> path -> path list

(*
   List all files recursively. Exclude folders/directories.
   Use List.filter_map to exclude more file types.
*)
val list_with_stat : path -> (path * Unix.stats) list

(*
   Iterate over files recursively. Exclude folders/directories.
*)
val fold_left : ('acc -> path -> Unix.stats -> 'acc) -> 'acc -> path -> 'acc
val iter : (path -> Unix.stats -> unit) -> path -> unit
