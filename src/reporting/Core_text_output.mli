(* This file is mostly deprecated. You should use osemgrep text output instead *)

(* this can also display metavars and taint traces *)
val print_match : < Cap.stdout > -> Semgrep_output_v1_j.core_match -> unit

(* used also in Metavar_replacement.ml *)
val join_with_space_if_needed : string list -> string
