(* A function which maps a match result from the *extracted* target
 * (e.g., '/tmp/extract-foo.rb') to a match result to the
 * *original* target (e.g., 'src/foo.erb').
 *)

type match_result_location_adjuster =
  Report.partial_profiling Report.match_result ->
  Report.partial_profiling Report.match_result

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
  all_rules:Rule.t list ->
  Rule.extract_rule list ->
  Xtarget.t ->
  (Input_to_core_t.target * match_result_location_adjuster) list
