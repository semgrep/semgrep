(*s: semgrep/reporting/Matching_report.mli *)
(*s: type [[Matching_report.match_format]] *)
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

(*e: type [[Matching_report.match_format]] *)

(*s: signature [[Matching_report.print_match]] *)
val print_match :
  ?format:match_format -> ?str:string -> Parse_info.t list -> unit

(*e: signature [[Matching_report.print_match]] *)

(*s: signature [[Matching_report.join_with_space_if_needed]] *)
val join_with_space_if_needed : string list -> string

(*e: signature [[Matching_report.join_with_space_if_needed]] *)
(*e: semgrep/reporting/Matching_report.mli *)
