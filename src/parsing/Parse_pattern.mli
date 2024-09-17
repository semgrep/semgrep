val parse_pattern :
  ?rule_options:Rule_options_t.t ->
  Lang.t ->
  string ->
  (Pattern.t, string) Result.t

val parse_pattern_ref :
  (Rule_options_t.t option -> Lang.t -> string -> Pattern.t) ref
