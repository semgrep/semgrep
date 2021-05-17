(* this is memoized *)
val regexp_prefilter_of_rule :
  Rule.t -> (string (* for debugging *) * (string -> bool)) option

(* internal, do not use directly, not memoized *)
val regexp_prefilter_of_formula :
  Rule.formula -> (string (* for debugging *) * (string -> bool)) option
