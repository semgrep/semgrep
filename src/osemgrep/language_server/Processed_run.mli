val of_matches :
  ?skipped_fingerprints:string list ->
  ?only_git_dirty:bool ->
  Core_runner.result ->
  Semgrep_output_v1_t.cli_match list
(**  [of_matches ~only_git_dirty result] returns the list of cli matches from the
     result of a semgrep run. If [only_git_dirty] is [true], only the matches
     that are in files + lines that are git dirty are returned. *)
