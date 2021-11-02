val extract_specific_strings : ?lang:Lang.t -> Pattern.t -> string list

(*
   Extract strings and metavariables that occur in the source code.
*)
val extract_strings_and_mvars :
  ?lang:Lang.t -> Pattern.t -> string list * Metavariable.mvar list
