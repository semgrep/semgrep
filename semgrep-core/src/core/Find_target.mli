(*
   Find target files suitable to be analyzed by semgrep, assuming a given
   language.
*)

(*
   Scan a list of folders or files recursively and return a list of files
   in the requested language. This takes care of ignoring undesirable
   files, which are returned in the semgrep-core response format.

   Reasons for skipping a file include currently:
   - the file looks like it's the wrong language.
   - a 'skip_list.txt' file was found at a conventional location (see
     skip_list.ml in pfff).
   - files over a certain size.

   If keep_root_files is true (the default), regular files passed directly
   to this function are considered ok and bypass the other filters.

   This is a replacement for Lang.files_of_dirs_or_files.
*)
val files_of_dirs_or_files :
  ?keep_root_files:bool ->
  Lang.t ->
  Common.path list ->
  Common.filename list * Semgrep_core_response_t.skipped_target list
