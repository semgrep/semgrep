
val regexp_prefilter_of_formula:
  Rule.formula -> (string (* for debugging *) * (string -> bool)) option


val regexp_prefilter_of_rule:
  Rule.t -> (string (* for debugging *) * (string -> bool)) option
