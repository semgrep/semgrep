(* Creation of /tmp files, a la gcc
   ex: new_temp_file "cocci" ".c" will create a new tmp file and return
   its name (e.g., "/tmp/cocci-3252-434465.c").

   Note: the set of tmp files created are saved in a global and
   you can call erase_temp_files() before exiting your program to
   clean things up.

   Note: You should use with_temp_file() instead in most cases.

   Options: see 'with_temp_file'.
*)
val new_temp_file :
  ?prefix:string -> ?suffix:string -> ?temp_dir:Fpath.t -> unit -> Fpath.t

(* Erase all the temporary files created by new_temp_file().
 * Usually called before exiting the program to clean things up.
 *)
val erase_temp_files : unit -> unit

(* To not erase tmp files after they have been used (can be useful to
 * help debug failures). Usually set via a -keep_tmp_files CLI flag. *)
val save_temp_files : bool ref

(* Erase the tmp file created by new_temp_file() and remove it from
 * the global, so erase_temp_files() will not try to delete an already
 * deleted file.
 *)
val erase_this_temp_file : Fpath.t -> unit

(* Create a new temporary file (using new_temp_file() above), invoke
   the passed function on the temporary file, and erase the temporary
   file once done (using erase_this_temp_file()).
   You can also setup cleanup hooks, see below.

   Options:
   - contents: optional data to write into the file. Default: none.
   - persist: keep the file instead of deleting it when done. This can
              be useful for debugging.
   - prefix: a prefix for the file name. Default: derived from argv[0].
   - suffix: an optional suffix for the file name e.g. '.py'. Default: empty.
   - temp_dir: folder containing the temporary file. Defaults to the
               system-defined temporary folder e.g. '/tmp'.
*)
val with_temp_file :
  ?contents:string ->
  ?persist:bool ->
  ?prefix:string ->
  ?suffix:string ->
  ?temp_dir:Fpath.t ->
  (Fpath.t -> 'a) ->
  'a

(* The hooks below are run just before a tmp file created by with_temp_file()
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
 * See https://github.com/semgrep/semgrep/issues/5277 for more info.
 *)
val register_temp_file_cleanup_hook : (Fpath.t -> unit) -> unit

(* If the file is a named pipe (e.g., created with <(echo 'foo')), copy it
   into a temporary regular file (with prefix [prefix]) and return the path
   of that temporary file. This allows multiple reads on the file and
   avoids illegal seeks when reporting match results or parsing errors.
   The temporary file is deleted at_exit.

   We return an option because it's useful to know whether the path
   was replaced.
*)
val replace_named_pipe_by_regular_file_if_needed :
  ?prefix:string -> Fpath.t -> Fpath.t option

(* Create a temporary file holding the contents of stdin. Works like
   'replace_named_pipe_by_regular_file_if_needed' above.
*)
val replace_stdin_by_regular_file : ?prefix:string -> unit -> Fpath.t

(* fpath wrapper to Filename.get_temp_dir_name() *)
val get_temp_dir_name : unit -> Fpath.t
