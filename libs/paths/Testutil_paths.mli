type file_tree = Dir of string * file_tree list | File of string * file_kind
and file_kind = Regular of string | Symlink of string

val with_file_tree : file_tree -> (Fpath.t -> unit) -> unit
val with_file_trees : file_tree list -> (Fpath.t -> unit) -> unit
