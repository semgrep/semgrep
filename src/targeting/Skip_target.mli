(* This is using the skip_list.txt of pfff Skip_code.ml *)
val exclude_files_in_skip_lists :
  Fpath.t list -> Fpath.t list * Output_from_core_t.skipped_target list

(* This is using Flag_semgrep.max_target_bytes *)
val exclude_big_files :
  Fpath.t list -> Fpath.t list * Output_from_core_t.skipped_target list

(* Detecting and filtering minified files (for Javascript) *)
val exclude_minified_files :
  Fpath.t list -> Fpath.t list * Output_from_core_t.skipped_target list
