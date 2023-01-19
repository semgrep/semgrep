(*
   Exit codes for the semgrep executable.

   Partially translated from error.py
*)

type t = int

(* this is useful because of the private declaration of t in the .mli *)
let to_int x = x
let of_int x = x

(*
   Exit codes of the semgrep command.
   Commented out definitions show the codes that are no longer in use.
*)
let ok = 0
let findings = 1
let fatal = 2
let invalid_code = 3
let invalid_pattern = 4
let unparseable_yaml = 5

(* let need_arbitrary_code_exec = 6 *)
let missing_config = 7
let invalid_language = 8

(* let match_timeout = 9 *)
(* let match_max_memory = 10 *)
(* let lexical_error = 11 *)
(* let too_many_matches = 12 *)
let invalid_api_key = 13
let scan_fail = 14

(* Temporary until either osemgrep dies or replaces semgrep. *)
let not_implemented_in_osemgrep = 99
