(* Similar to Analyze_pattern.ml but for spacegrep *)
val extract_strings_and_mvars_spacegrep :
  Spacegrep.Pattern_AST.t -> string list * Metavariable.mvar list
