(* TODO: what's this strange signature? *)
val translate_files :
  (Fpath.t -> (Rule.t list, Rule.error) Result.t) -> Fpath.t list -> unit
