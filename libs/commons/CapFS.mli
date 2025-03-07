val readdir : Cap.FS.readdir -> Unix.dir_handle -> Fpath.t

(* Read the names found in a directory, excluding "." and "..". *)
val read_dir_entries : < Cap.readdir ; .. > -> Fpath.t -> Fpath.t list

(* Note that this calls internally Sys.readdir but does not require
 * the capability because in the end none of the entries are returned
 *)
val is_empty_dir : Fpath.t -> bool

(* [with_chdir caps dir f] will temporarily change the pwd
 * to [dir] and execute [f] in this context and then restore the pwd to
 * its old value. This internally calls Common.protect so is
 * safe to use even if f throw exceptions
 *)
val with_chdir : < Cap.chdir ; .. > -> Fpath.t -> (unit -> 'a) -> 'a
