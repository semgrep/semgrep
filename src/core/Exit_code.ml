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

(* a.k.a. target_parse_failure *)
let invalid_code = 3

(* a.k.a rule_parse_failure *)
let invalid_pattern = 4
let unparseable_yaml = 5

(* old: let need_arbitrary_code_exec = 6 *)
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

let to_message code =
  match code with
  | 0 -> "OK"
  | 1 -> "some findings"
  | 2 -> "fatal error"
  | 3 -> "invalid target code"
  | 4 -> "invalid pattern"
  | 5 -> "unparseable YAML"
  | 6 -> "need arbitrary code execution"
  | 7 -> "missing configuration"
  | 8 -> "invalid language"
  | 9 -> "match timeout"
  | 10 -> "match memory limit exceeded"
  | 11 -> "lexical error"
  | 12 -> "too many matches"
  | 13 -> "invalid API key"
  | 14 -> "scan failure"
  | 99 -> "not implemented in osemgrep"
  | n -> Printf.sprintf "<unknown exit code %i>" n
