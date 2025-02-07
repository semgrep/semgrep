val readdir : Cap.FS.readdir -> Unix.dir_handle -> string

(* Read the names found in a directory, excluding "." and "..". *)
val read_dir_entries : < Cap.readdir ; .. > -> Fpath.t -> string list
