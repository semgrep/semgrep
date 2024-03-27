val is_large_machine_optimized :
  Fpath.t -> (Fpath.t, Semgrep_output_v1_t.skipped_target) result

(* This is using Flag_semgrep.max_target_bytes *)
val exclude_big_files :
  Fpath.t list -> Fpath.t list * Semgrep_output_v1_t.skipped_target list

(* Detecting and filtering minified files (for Javascript) *)
val exclude_minified_files :
  Fpath.t list -> Fpath.t list * Semgrep_output_v1_t.skipped_target list
