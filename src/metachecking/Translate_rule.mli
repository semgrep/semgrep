(* TODO: what's this strange signature? *)
val translate_files :
  (Fpath.t -> (Rule.t list, Rule.Error.t) Result.t) -> Fpath.t list -> unit
