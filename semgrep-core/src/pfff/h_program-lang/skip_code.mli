
type skip =
  (* mostly to avoid parsing errors messages *)
  | Dir of Common.dirname
  | File of Common.filename
  | DirElement of Common.dirname
  | SkipErrorsDir of Common.dirname

val load: Common.filename -> skip list

(*
   Return selected paths (unchanged) and excluded files (real paths including
   the root).
*)
val filter_files:
  skip list -> root:Common.dirname (* root *) -> Common.filename list ->
  Common.filename list * Common.filename list

(*
   TODO: explain why 'root' is a list and the implementation only uses its
   head.

   This function searches for files named 'skip_list.txt' in conventional
   locations and uses them to exclude target files based on patterns they
   contain.

   The result is the filtered list of paths (relative to the root)
   and the list of excluded paths (real paths including the root).
*)
val filter_files_if_skip_list:
  root:Common.dirname list -> Common.filename list ->
  Common.filename list * Common.filename list

val reorder_files_skip_errors_last:
  skip list -> Common.dirname (* root *) -> Common.filename list ->
  Common.filename list

(* returns true if we should skip the file for errors *)
val build_filter_errors_file:
  skip list -> (Common.filename (* readable *) -> bool)
