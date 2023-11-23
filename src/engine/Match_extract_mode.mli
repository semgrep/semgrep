(*
   Generates a list of targets corresponding to extract mode rule matches in
   the provided target file. Each generated target will also have a
   function which can transform match results in the generated target/file to
   match results corresponding to the file from which that extraction occured.
*)
val extract :
  match_hook:(string -> Pattern_match.t -> unit) ->
  timeout:float ->
  timeout_threshold:int ->
  Rule.extract_rule list ->
  Xtarget.t ->
  Extract.extracted_target_and_adjuster list
