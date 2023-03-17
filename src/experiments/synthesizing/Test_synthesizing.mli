val expr_at_range : string -> Fpath.t -> unit
val synthesize_patterns : string -> Fpath.t -> unit
val generate_pattern_choices : string list -> unit

(* See function of same name in Synthesizer.mli *)
val locate_patched_functions : Fpath.t -> unit
