(* This is using the skip_list.txt of pfff Skip_code.ml *)
val exclude_files_in_skip_lists :
  Common.filename list ->
  Common.filename list * Semgrep_core_response_t.skipped_target list

(* This is using Flag_semgrep.max_target_bytes *)
val exclude_big_files :
  Common.filename list ->
  Common.filename list * Semgrep_core_response_t.skipped_target list

(* Detecting and filtering minified files (for Javascript) *)
val exclude_minified_files :
  Common.filename list ->
  Common.filename list * Semgrep_core_response_t.skipped_target list

(* Stats used internally to detect minified files *)
type whitespace_stat = { sample_size : int; ws_freq : float; line_freq : float }

(*
   Return file size and frequency of whitespace bytes.
   This is intended for calibrating the heuristic used to detect
   minified files.
*)
val whitespace_stat_of_file : Common.filename -> whitespace_stat
