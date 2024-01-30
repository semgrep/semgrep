(*
   Exit codes for the semgrep executable.

   Partially translated from error.py
*)

type t = int

(*
   This systematically logs where the exit code was generated.
   Otherwise, it's like looking for a needle in haystack.
*)
let with_log ~__LOC__:loc x =
  Logs.info (fun m -> m "Producing exit code %i at: %s" x loc);
  x

(* this is useful because of the private declaration of t in the .mli *)
let to_int x = x
let of_int ~__LOC__:loc x = with_log loc x

(* This is for comparing exit codes against expectations without logging
   the exit code. See mli. *)
module Int = struct
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
  let not_implemented_in_osemgrep = 99
end

(*
   Exit codes of the semgrep command.
   Commented out definitions show the codes that are no longer in use.
*)
let ok ~__LOC__:loc = with_log loc Int.ok
let findings ~__LOC__:loc = with_log loc Int.findings
let fatal ~__LOC__:loc = with_log loc Int.fatal

(* a.k.a. target_parse_failure *)
let invalid_code ~__LOC__:loc = with_log loc Int.invalid_code

(* a.k.a rule_parse_failure *)
let invalid_pattern ~__LOC__:loc = with_log loc Int.invalid_pattern
let unparseable_yaml ~__LOC__:loc = with_log loc Int.unparseable_yaml
let missing_config ~__LOC__:loc = with_log loc Int.missing_config
let invalid_language ~__LOC__:loc = with_log loc Int.invalid_language
let invalid_api_key ~__LOC__:loc = with_log loc Int.invalid_api_key
let scan_fail ~__LOC__:loc = with_log loc Int.scan_fail

(* Temporary until either osemgrep dies or replaces semgrep. *)
let not_implemented_in_osemgrep ~__LOC__:loc =
  with_log loc Int.not_implemented_in_osemgrep

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

module Check = struct
  let ok = Alcotest.(check int) "exit code" Int.ok
  let findings = Alcotest.(check int) "exit code" Int.findings
  let fatal = Alcotest.(check int) "exit code" Int.fatal
  let invalid_code = Alcotest.(check int) "exit code" Int.invalid_code
  let invalid_pattern = Alcotest.(check int) "exit code" Int.invalid_pattern
  let unparseable_yaml = Alcotest.(check int) "exit code" Int.unparseable_yaml
  let missing_config = Alcotest.(check int) "exit code" Int.missing_config
  let invalid_language = Alcotest.(check int) "exit code" Int.invalid_language
  let invalid_api_key = Alcotest.(check int) "exit code" Int.invalid_api_key
  let scan_fail = Alcotest.(check int) "exit code" Int.scan_fail

  let not_implemented_in_osemgrep =
    Alcotest.(check int) "exit code" Int.not_implemented_in_osemgrep
end
