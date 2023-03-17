(* TODO: rename this module since it's used directly in the semgrep command
   line and doesn't run any tests? *)

val expr_at_range : string -> Fpath.t -> unit
val synthesize_patterns : string -> Fpath.t -> unit

(* this takes something like a list of strings of strings representing
   ranges except for the last element that's a file path.
   TODO: please expose a well-typed signature; command-line parsing should
   be done elsewhere.
*)
val generate_pattern_choices : string list -> unit

(* See function of same name in Synthesizer.mli *)
val locate_patched_functions : Fpath.t -> unit
