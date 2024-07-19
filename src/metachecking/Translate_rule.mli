(* TODO: what's this strange signature? *)
val translate_files :
  (Fpath.t -> (Rule.t list, Rule_error.t) result) -> Fpath.t list -> unit
