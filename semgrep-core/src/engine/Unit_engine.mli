(*
   The test suites are defined from files found in the file system relative
   to the current location. Having them created on demand allows running
   'dune utop' from any location.
*)
val tests : unit -> Testutil.test list

(* Can be used from other test code to concisely run Semgrep *)
val match_pattern :
  lang:Lang.t ->
  hook:(Pattern_match.t -> unit) ->
  file:string ->
  pattern:string ->
  fix_pattern:string option ->
  Pattern_match.t list
