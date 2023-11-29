(*
   Tags used to filter tests.
*)

val todo_js : string

(* This is used to exclude all the tests involving this or that language. *)
val tags_of_lang : Lang.t -> string list
val tags_of_langs : Lang.t list -> string list
