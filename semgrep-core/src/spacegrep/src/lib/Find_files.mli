(**
   Iterate over a file tree.
*)

val fold :
  ?excluded_paths:string list ->
  ?accept_file_name:(string -> bool) ->
  ?accept_dir_name:(string -> bool) ->
  ('acc -> string -> 'acc) ->
  'acc ->
  string list ->
  'acc
(**
   [fold f acc roots] scans the files or directories [roots] and their
   children, applying [f] on each path corresponding to a regular file
   or a link to a regular file. The accumulator [acc] is used to carry
   results along, like the standard [List.fold_left].

   @param excluded_paths is a list of files or directories to not scan.
   @param accept_file_name is an optional filter applied on a
                           file's base name, not its full path. Only applies
                           to regular files or symbolic links to regular files.
   @param accept_dir_name is an optional filter applied on a directory's base
                          name, not its full path. Only applies
                           to directories or symbolic links to directories.
*)

val iter :
  ?excluded_paths:string list ->
  ?accept_file_name:(string -> bool) ->
  ?accept_dir_name:(string -> bool) ->
  (string -> unit) ->
  string list ->
  unit
(** Simplified interface to [fold], operating by side effects. *)

val list :
  ?excluded_paths:string list ->
  ?accept_file_name:(string -> bool) ->
  ?accept_dir_name:(string -> bool) ->
  string list ->
  string list
(** Simplified interface to [fold], which lists all the matching paths. *)
