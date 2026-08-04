(*
   Copyright (c) 2021-2025 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
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

val is_binary : Fpath.t -> (Fpath.t, Semgrep_output_v1_t.skipped_target) result
(** [is_binary path] returns [Ok path] if the file is not binary, and [Error
    skipped_target] if it is. Source-like files (per [File_type]) are never
    considered binary; other files are classified by matching magic numbers at
    the start of the file. *)

val exclude_big_files :
  int -> Fpath.t list -> Fpath.t list * Semgrep_output_v1_t.skipped_target list
(** [exclude_big_files max_target_bytes paths] will exclude files larger that
    [max_target_bytes]. No files are excluded if [max_target_bytes = -1]*)

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
