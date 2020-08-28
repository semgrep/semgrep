type expected_argument = PercentD | PercentS

val start_state:
  expected_argument list -> char list -> expected_argument list

val try_auto_fix:
  Error_php.error -> unit
