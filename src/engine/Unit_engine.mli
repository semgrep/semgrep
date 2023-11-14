(*
   The test suites are defined from files found in the file system relative
   to the current location. Having them created on demand allows running
   'dune utop' from any location.
*)
val tests : unit -> Testutil.test list

type fix_type =
  | Fix of string
  | FixRegex of
      (* regex *) string * (* count *) int option * (* replacement *) string
  | NoFix

(* Can be used from other test code to concisely run Semgrep *)
val match_pattern :
  lang:Lang.t ->
  hook:(Pattern_match.t -> unit) ->
  file:Fpath.t ->
  pattern:string ->
  fix:fix_type ->
  Pattern_match.t list
