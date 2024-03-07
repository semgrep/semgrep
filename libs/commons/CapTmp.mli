(* Capability aware wrappers to UTmp.ml *)

val with_tmp_file :
  Cap.FS.tmp -> str:string -> ext:string -> (Fpath.t -> 'a) -> 'a

val get_temp_dir_name : Cap.FS.tmp -> Fpath.t

val new_temp_file :
  ?temp_dir:Fpath.t -> Cap.FS.tmp -> string -> string -> Fpath.t

val replace_named_pipe_by_regular_file_if_needed :
  Cap.FS.tmp -> ?prefix:string -> Fpath.t -> Fpath.t option

val replace_stdin_by_regular_file :
  Cap.FS.tmp -> ?prefix:string -> unit -> Fpath.t

val erase_temp_files : Cap.FS.tmp -> unit
