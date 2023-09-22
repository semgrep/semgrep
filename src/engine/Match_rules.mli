(* this can be raised when timeout_threshold is set *)
exception File_timeout

(*
   Return matches, errors, match time.

   This will run the search-mode and taint-mode rules.
   This can raise File_timeout.
*)
val check :
  match_hook:(string -> Pattern_match.t -> unit) ->
  timeout:float ->
  timeout_threshold:int ->
  Match_env.xconfig ->
  Rule.rules ->
  Xtarget.t ->
  Core_profiling.partial_profiling Core_result.match_result

(* for osemgrep interactive *)
val is_relevant_rule_for_xtarget :
  Analyze_rule.prefilter_cache option ->
  Rule.t ->
  Match_env.xconfig ->
  Xtarget.t ->
  bool
