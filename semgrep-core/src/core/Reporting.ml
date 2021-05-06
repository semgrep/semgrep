type profiling = {
  file: Common.filename;
  parse_time: float;
  match_time: float;
  run_time: float;
}

type match_result = {
  matches: Pattern_match.t list;
  errors: Error_code.error list;
  profiling: profiling;
}
