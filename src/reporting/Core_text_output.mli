(* This file is mostly deprecated. You should use osemgrep text output instead *)

(* this can also display metavars and taint traces *)
val print_match :
  < Cap.stdout > -> Pattern_match.t -> Metavariable.mvar list -> unit

(* used also in Metavar_replacement.ml *)
val join_with_space_if_needed : string list -> string
