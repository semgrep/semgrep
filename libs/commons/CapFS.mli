val readdir : Cap.FS.readdir -> Unix.dir_handle -> string

(* Read the names found in a directory, excluding "." and "..". *)
val read_dir_entries : < Cap.readdir ; .. > -> Fpath.t -> string list

(* Note that this calls internally Sys.readdir but does not require
 * the capability because in the end none of the entries are returned
 *)
val is_empty_dir : Fpath.t -> bool
