(* Capability aware wrappers to UTmp.ml *)

val with_tmp_file :
  Cap.FS.tmp -> str:string -> ext:string -> (Fpath.t -> 'a) -> 'a
