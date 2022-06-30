type match_format =
  (* ex: tests/misc/foo4.php:3
   *  foo(
   *   1,
   *   2);
   *)
  | Normal
  (* ex: tests/misc/foo4.php:3: foo( *)
  | Emacs
  (* ex: tests/misc/foo4.php:3: foo(1,2) *)
  | OneLine

val print_match :
  ?format:match_format ->
  ?str:string ->
  ?spaces:int ->
  Parse_info.t list ->
  unit

val join_with_space_if_needed : string list -> string
