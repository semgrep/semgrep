(*
   Translated from error.py
*)

(*
   Exit codes of the semgrep command.
   Commented out definitions show the codes that are no longer in use.
*)
let ok_exit_code = 0
let findings_exit_code = 1
let fatal_exit_code = 2

(* let invalid_code_exit_code = 3 *)
let invalid_pattern_exit_code = 4
let unparseable_yaml_exit_code = 5

(* let need_arbitrary_code_exec_exit_code = 6 *)
let missing_config_exit_code = 7
let invalid_language_exit_code = 8

(* let match_timeout_exit_code = 9 *)
(* let match_max_memory_exit_code = 10 *)
(* let lexical_error_exit_code = 11 *)
(* let too_many_matches_exit_code = 12 *)
let invalid_api_key_exit_code = 13
let scan_fail_exit_code = 14

(* Temporary until either mlgrep dies or replaces semgrep. *)
let not_implemented_in_mlgrep = 99

type level =
  | Warn (* = 3; Always an error *)
  | Error (* = 4; Only an error if "strict" is set *)
