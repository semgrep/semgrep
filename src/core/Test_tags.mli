(*
   Tags used to filter tests.
*)

val todo_js : Alcotest_ext.Tag.t

(* This is used to exclude all the tests involving this or that language. *)
val tags_of_lang : Lang.t -> Alcotest_ext.Tag.t list
val tags_of_langs : Lang.t list -> Alcotest_ext.Tag.t list
