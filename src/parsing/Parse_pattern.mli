val parse_pattern :
  ?print_errors:bool ->
  ?rule_options:Rule_options_t.t option ->
  Lang.t ->
  string ->
  Pattern.t

val parse_pattern_ref :
  (bool -> Rule_options_t.t option -> Lang.t -> string -> Pattern.t) ref
