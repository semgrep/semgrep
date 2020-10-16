(*
   Interface with semgrep wrapper.
*)

(*
   Print the matches in the json format that semgrep understands.
   (See Semgrep.atd)
*)
val print_semgrep_json :
  (Src_file.t * (Match.pattern_id * Match.match_ list) list) list -> unit
