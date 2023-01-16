(*
   Analyze the contents of a string literal bound to a metavariable.
   The analyzer operates of the strings contents after best-effort
   unescaping.
   Return false if the bound value isn't a string literal.
   The predicate can be Entropy.has_high_score.
   The environment is only used for error management, to be able
   to call Match_env.error
   alt: pass an error function instead of the environment.
*)
val analyze_string_metavar :
  Match_env.env ->
  Metavariable.bindings ->
  Metavariable.mvar ->
  (string -> bool) ->
  bool
