(*s: semgrep/engine/Semgrep.mli *)

val check:
  (Metavariable.bindings -> Parse_info.t list Lazy.t -> unit) (* hook *) ->
  Rule.rules ->
  (Common.filename * Lang.t * Target.t) ->
  Rule_match.t list

(*e: semgrep/engine/Semgrep.mli *)
