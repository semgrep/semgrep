(* this can be raised if timeout_threshold is set *)
exception File_timeout

(* TODO: this should not bubble up outside check:, but this is currently
 * catched in Parse_with_caching.ml so needs to be in the .mli
 *)
exception Rule_timeout

(*
   Return matches, errors, match time.

   This will run the search-mode and taint-mode rules.
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
