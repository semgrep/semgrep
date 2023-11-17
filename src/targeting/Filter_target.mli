(* Select the file if it belongs to the language using Guess_lang.ml *)
val filter_target_for_xlang : Xlang.t -> Fpath.t -> bool

(* Select the file if it satisfies the include: exclude: constraints
 * in a rule paths: field *)
val filter_paths : Rule.paths -> Fpath.t -> bool

(* Return true if the target has a known extension *)
val filter_target_has_known_extension : Fpath.t -> bool
