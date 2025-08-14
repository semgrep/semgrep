val filter_target_for_analyzer : Analyzer.t -> Fpath.t -> bool
(** Select the file if it belongs to the language using Guess_lang.ml *)

val filter_paths : Rule.path_filter -> Fppath.t -> bool
(** Determine whether a target file passes the path filters specified
   in a Semgrep rule (paths.exclude, paths.include).
   If the target file doesn't have a proper ppath, the filters can't be applied
   and the result is true.
 *)
