(* should only be extract rules *)
val extract_nested_lang :
  match_hook:
    (string -> Metavariable.bindings -> Parse_info.t list Lazy.t -> unit) ->
  timeout:float ->
  timeout_threshold:int ->
  Rule.extract_rule list ->
  Xtarget.t ->
  string list ->
  (* function maps results from the returned target(s) to results for the argument target *)
  (Input_to_core_t.target
  * (Report.partial_profiling Report.match_result ->
    Report.partial_profiling Report.match_result))
  list

(* alt?
   targets
    * (target,
       Report.file_profiling Report.match_result
        -> Report.file_profiling Report.match_result)
        Hashtbl.t
*)
