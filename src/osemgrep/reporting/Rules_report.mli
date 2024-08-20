val pp_rules :
  too_many_entries:int ->
  Format.formatter ->
  Rules_source.t * Rule.t list ->
  unit
