(*s: semgrep/engine/Semgrep.mli *)

val check:
  (*with_caching:*)bool ->
  (Metavariable.bindings -> Parse_info.t list Lazy.t -> unit) (* hook *) ->
  Rule.rules ->
  (Common.filename * Rule.xlang * (Target.t * Error_code.error list) Lazy.t) ->
  Rule_match.t list * Error_code.error list

(*e: semgrep/engine/Semgrep.mli *)
