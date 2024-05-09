(* !!This module is deprecated!! You should prefer the gitignore library
 * to skip files.
 *)

type skip =
  (* mostly to avoid parsing errors messages *)
  | Dir of Fpath.t
  | File of Fpath.t
  | DirElement of Fpath.t
  | SkipErrorsDir of Fpath.t

val load : Fpath.t -> skip list

(*
   Return selected paths (unchanged) and excluded files (real paths including
   the root).
*)
val filter_files :
  skip list ->
  root:Fpath.t (* root *) ->
  Fpath.t list ->
  Fpath.t list * Fpath.t list

(*
   TODO: explain why 'root' is a list and the implementation only uses its
   head.

   This function searches for files named 'skip_list.txt' in conventional
   locations and uses them to exclude target files based on patterns they
   contain.

   The result is the filtered list of paths (relative to the root)
   and the list of excluded paths (real paths including the root).
*)
val filter_files_if_skip_list :
  root:Fpath.t list -> Fpath.t list -> Fpath.t list * Fpath.t list

val reorder_files_skip_errors_last :
  skip list -> Fpath.t (* root *) -> Fpath.t list -> Fpath.t list

(* returns true if we should skip the file for errors *)
val build_filter_errors_file : skip list -> Fpath.t (* readable *) -> bool
