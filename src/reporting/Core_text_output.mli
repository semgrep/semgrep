(* This file is deprecated. You should use osemgrep text output *)

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
[@@deriving show]

val print_match :
  ?format:match_format -> ?str:string -> ?spaces:int -> Tok.t list -> unit

val join_with_space_if_needed : string list -> string
