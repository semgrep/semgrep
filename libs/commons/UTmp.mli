(* creation of /tmp files, a la gcc
 * ex: new_temp_file "cocci" ".c" will give "/tmp/cocci-3252-434465.c"
 *)
val new_temp_file :
  ?temp_dir:Fpath.t -> string (* prefix *) -> string (* suffix *) -> Fpath.t

val erase_temp_files : unit -> unit
val erase_this_temp_file : Fpath.t -> unit
val with_tmp_file : str:string -> ext:string -> (Fpath.t -> 'a) -> 'a

(* If the file is a named pipe (e.g., created with <(echo 'foo')), copy it
   into a temporary regular file (with prefix [prefix]) and return the path
   of that temporary file. This allows multiple reads on the file and
   avoids illegal seeks when reporting match results or parsing errors.
   The temporary file is deleted at_exit.
*)
val replace_named_pipe_by_regular_file_if_needed :
  ?prefix:string -> Fpath.t -> Fpath.t

(* To not erase tmp files after they have been used (can be useful to
 * help debug failures). Usually set via a -keep_tmp_files CLI flag) *)
val save_tmp_files : bool ref

(* fpath wrapper to Filename.get_temp_dir_name() *)
val get_temp_dir_name : unit -> Fpath.t

(* Deprecated! *)
module Legacy : sig
  val new_temp_file :
    ?temp_dir:string (* filename *) ->
    string (* prefix *) ->
    string (* suffix *) ->
    string (* filename *)

  val erase_this_temp_file : string (* filename *) -> unit

  val with_tmp_file :
    str:string -> ext:string -> (string (* filename*) -> 'a) -> 'a

  (* NOT IN MAIN API *)

  (* Runs just before a tmp file is deleted. Multiple hooks can be added,
   * but the order in which they are called is unspecified.
   *)
  val register_tmp_file_cleanup_hook : (string (* filename *) -> unit) -> unit
end
