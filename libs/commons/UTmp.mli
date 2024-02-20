(* Creation of /tmp files, a la gcc
 * ex: new_temp_file "cocci" ".c" will create a new tmp file and return
 * its name (e.g., "/tmp/cocci-3252-434465.c").
 *
 * Note: the set of tmp files created are saved in a global and
 * you can call erase_temp_files() before exiting your program to
 * clean things up.
 *
 * Note: You should use with_tmp_file() instead in most cases.
 *)
val new_temp_file :
  ?temp_dir:Fpath.t -> string (* prefix *) -> string (* suffix *) -> Fpath.t

(* Erase all the temporary files created by new_temp_file().
 * Usually called before exiting the program to clean things up.
 *)
val erase_temp_files : unit -> unit

(* To not erase tmp files after they have been used (can be useful to
 * help debug failures). Usually set via a -keep_tmp_files CLI flag. *)
val save_tmp_files : bool ref

(* Erase the tmp file created by new_temp_file() and remove it from
 * the global, so erase_temp_files() will not try to delete an already
 * deleted file.
 *)
val erase_this_temp_file : Fpath.t -> unit

(* Create a new temporary file (using new_temp_file() above), invoke
 * the passed function on the temporary file, and erase the temporary
 * file once done (using erase_this_temp_file()).
 * You can also setup cleanup hooks, see below.
 *)
val with_tmp_file : str:string -> ext:string -> (Fpath.t -> 'a) -> 'a

(* The hooks below are run just before a tmp file created by with_tmp_file()
 * is deleted. Multiple hooks can be added, but the order in which they are
 * called is unspecified.
 *
 * This is useful for cache invalidation when we cache information about
 * files. For example, when scanning a repository with a large number of
 * regex rules, Semgrep creates numerous tmp files. For each one, we create
 * a lookup table to go from offset -> line/col, and cache it keyed on
 * filename. If this cache does not get properly invalidated when Semgrep
 * is done with the tmp file, even though the tmp file does get removed,
 * the next time a tmp file is requested, there is a chance that the
 * randomly-generated filename will collide with a previous one. Semgrep would
 * rely on the cached lookup table for the previous file with that filename,
 * and chaos ensues.
 *
 * See https://github.com/returntocorp/semgrep/issues/5277 for more info.
 *)
val register_tmp_file_cleanup_hook : (Fpath.t -> unit) -> unit

(* If the file is a named pipe (e.g., created with <(echo 'foo')), copy it
   into a temporary regular file (with prefix [prefix]) and return the path
   of that temporary file. This allows multiple reads on the file and
   avoids illegal seeks when reporting match results or parsing errors.
   The temporary file is deleted at_exit.
*)
val replace_named_pipe_by_regular_file_if_needed :
  ?prefix:string -> Fpath.t -> Fpath.t

(* fpath wrapper to Filename.get_temp_dir_name() *)
val get_temp_dir_name : unit -> Fpath.t
