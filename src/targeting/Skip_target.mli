val is_minified :
  Fpath.t -> (Fpath.t, Semgrep_output_v1_t.skipped_target) result
(** [is_minified path] returns [Ok path] if the file is not minified, and
    [Error skipped_target] if it is. This is based on whitespace and line
    density *)

val is_big :
  int ->
  Semgrep_output_v1_t.fpath ->
  (Semgrep_output_v1_t.fpath, Semgrep_output_v1_t.skipped_target) result
(** [is_big max_target_bytes path] returns [Ok path] if the file is less than or
    equal to [max_target_bytes] or [max_target_bytes = -1], and [Error
    skipped_target] if it is larger. *)

val exclude_big_files :
  int -> Fpath.t list -> Fpath.t list * Semgrep_output_v1_t.skipped_target list
(** [exclude_big_files max_target_bytes paths] will exclude files larger that
    [max_target_bytes]. No files are excluded if [max_target_bytes = -1]*)

(* Detecting and filtering minified files (for Javascript) *)
val exclude_minified_files :
  Fpath.t list -> Fpath.t list * Semgrep_output_v1_t.skipped_target list

(*************************************************************************)
(* Access permission filtering *)
(*************************************************************************)
(*
   Filter out folders and files that don't have sufficient access permissions.
*)

val filter_dir_access_permissions :
  Fpath.t -> (Fpath.t, Semgrep_output_v1_t.skipped_target) result

val filter_file_access_permissions :
  Fpath.t -> (Fpath.t, Semgrep_output_v1_t.skipped_target) result

val exclude_inaccessible_files :
  Fpath.t list -> Fpath.t list * Semgrep_output_v1_t.skipped_target list
