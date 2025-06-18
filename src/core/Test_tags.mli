(*
   Tags used to filter tests.
*)

(* A test that sometimes fails for unknown reasons *)
val flaky : Testo.Tag.t

(* This is used to exclude all the tests involving this or that language. *)
val tags_of_lang : Lang.t -> Testo.Tag.t list
val tags_of_langs : Lang.t list -> Testo.Tag.t list
