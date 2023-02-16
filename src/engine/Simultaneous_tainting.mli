val run_simultaneous_taint :
  match_hook:(string -> Pattern_match.t -> unit) ->
  Rule.taint_mode Rule.rule_info list ->
  Match_env.xconfig ->
  Xtarget.t ->
  (* in reality, only ever returns taint rules... *)
  Rule.mode Rule.rule_info list
