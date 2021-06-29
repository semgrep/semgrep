(*
   Interface with semgrep wrapper.
*)

(*
   Print the matches in the json format that semgrep understands.
   (See Semgrep.atd)
*)
val print_semgrep_json :
  with_time:bool ->
  ( Src_file.t
  * (Match.pattern_id * Match.match_ list * float) list
  * float
  * float )
  list ->
  (Src_file.t * Parse_pattern.error) list ->
  unit
