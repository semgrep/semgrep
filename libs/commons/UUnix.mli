val stat : Fpath.t -> (Unix.stats, Unix.error * string * string) result
val lstat : Fpath.t -> (Unix.stats, Unix.error * string * string) result
val fstat : Unix.file_descr -> (Unix.stats, Unix.error * string * string) result
