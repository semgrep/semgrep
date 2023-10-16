module MvarSet = Common2.StringSet

type mvars = MvarSet.t

val extract_specific_strings : ?lang:Lang.t -> Pattern.t -> string list

(*
   Extract strings and metavariables that occur in the source code.
*)
val extract_strings_and_mvars :
  ?lang:Lang.t -> Pattern.t -> string list * Metavariable.mvar list

(*
   Extract metavariables that occur in an "id position" so that, if we
   encounter a `metavariable-regex` operator on any of those metavariables,
   we can use the corresponding `regex` for pre-filtering.
*)
val extract_mvars_in_id_position : ?lang:Lang.t -> Pattern.t -> mvars
