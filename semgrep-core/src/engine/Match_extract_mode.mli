(*
   Generates a list of targets corresponding to extract mode rule matches in
   the provided target file. The resulting target will be configured to run
   the rules whose ids are passed. Each generated target will also have a
   function which can transform match results in the generated target/file to
   match results corresponding to the file from which that extraction occured.
*)
val extract_nested_lang :
  match_hook:(string -> Pattern_match.t -> unit) ->
  timeout:float ->
  timeout_threshold:int ->
  Rule.extract_rule list ->
  Xtarget.t ->
  Rule.rule_id list ->
  (Input_to_core_t.target
   (* function maps results from the returned target(s) to results for the
      argument target. Could instead be
          (a -> a) -> a Report.match_result -> a Report.match_result
      although this is a bit less ergonomic for the caller.
   *)
  * (Report.partial_profiling Report.match_result ->
    Report.partial_profiling Report.match_result))
  list
