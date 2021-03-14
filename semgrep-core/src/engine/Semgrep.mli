(*s: semgrep/engine/Semgrep.mli *)

(*
   Return matches, errors, match time.
*)
val check:
  (string -> Metavariable.bindings -> Parse_info.t list Lazy.t -> unit) ->
  Config_semgrep.t ->
  Rule.rules ->
  (Common.filename * Rule.xlang * (Target.t * Error_code.error list) Lazy.t) ->
  Rule_match.t list * Error_code.error list * float

(*e: semgrep/engine/Semgrep.mli *)
