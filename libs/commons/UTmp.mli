(* creation of /tmp files, a la gcc
 * ex: new_temp_file "cocci" ".c" will give "/tmp/cocci-3252-434465.c"
 *)
val new_temp_file : string (* prefix *) -> string (* suffix *) -> Fpath.t
val erase_temp_files : unit -> unit
val erase_this_temp_file : Fpath.t -> unit

(* If the file is a named pipe (e.g., created with <(echo 'foo')), copy it
   into a temporary regular file (with prefix [prefix]) and return the path
   of that temporary file. This allows multiple reads on the file and
   avoids illegal seeks when reporting match results or parsing errors.
   The temporary file is deleted at_exit.
*)
val replace_named_pipe_by_regular_file_if_needed :
  ?prefix:string -> Fpath.t -> Fpath.t
