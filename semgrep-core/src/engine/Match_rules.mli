(* this can be raised when timeout_threshold is set *)
exception File_timeout

(*
   Return matches, errors, match time.

   This will run the search-mode and taint-mode rules.
   This can raise File_timeout.
*)
val check :
  match_hook:
    (string -> Metavariable.bindings -> Parse_info.t list Lazy.t -> unit) ->
  timeout:float ->
  timeout_threshold:int ->
  Config_semgrep.t * Equivalence.equivalences ->
  Rule.rules ->
  Xtarget.t ->
  Report.partial_profiling Report.match_result
