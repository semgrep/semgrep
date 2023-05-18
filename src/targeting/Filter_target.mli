(* Select the file if it belongs to the language using Guess_lang.ml *)
val filter_target_for_xlang : Xlang.t -> Fpath.t -> bool

(* Select the file if it satisfies the include: exclude: constraints
 * in a rule paths: field *)
val filter_paths : Rule.paths -> Fpath.t -> bool
