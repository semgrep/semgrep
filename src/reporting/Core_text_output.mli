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

(* this can also display metavars and taint traces *)
val print_match :
  < Cap.stdout > ->
  match_format ->
  Pattern_match.t ->
  Metavariable.mvar list ->
  (Metavariable.mvalue -> Tok.t list) ->
  unit

(* used also in Metavar_replacement.ml *)
val join_with_space_if_needed : string list -> string
