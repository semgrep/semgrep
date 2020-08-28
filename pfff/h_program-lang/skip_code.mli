
type skip =
  (* mostly to avoid parsing errors messages *)
  | Dir of Common.dirname
  | File of Common.filename
  | DirElement of Common.dirname
  | SkipErrorsDir of Common.dirname

val load: Common.filename -> skip list

val filter_files: 
  skip list -> Common.dirname (* root *) -> Common.filename list -> 
  Common.filename list

(* assumes given full paths *)
val filter_files_if_skip_list:
  root:Common.dirname list -> Common.filename list -> Common.filename list

val reorder_files_skip_errors_last:
  skip list -> Common.dirname (* root *) -> Common.filename list -> 
  Common.filename list

(* returns true if we should skip the file for errors *)
val build_filter_errors_file:
  skip list -> (Common.filename (* readable *) -> bool)

