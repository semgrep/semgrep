(*s: semgrep/engine/Semgrep.mli *)

val check:
  (*with_caching:*)bool ->
  (Metavariable.bindings -> Parse_info.t list Lazy.t -> unit) (* hook *) ->
  Rule.rules ->
  (Common.filename * Lang.t * Target.t) ->
  Pattern_match.t list

(*e: semgrep/engine/Semgrep.mli *)
