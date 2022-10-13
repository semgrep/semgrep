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

(* [lines_of_file (start_line, end_line) file] returns
 * the list of lines from start_line to end_line included.
 *
 * Note that the returned lines do not contain \n.
 *
 * This function is slow, you should not use it!
 *)
val lines_of_file : int * int -> Common.filename -> string list
