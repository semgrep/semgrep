(*
   Exit codes for the semgrep executable.

   Partially translated from error.py
*)

type t = { code : int; description : string }

let create code description = { code; description }

(*
   This systematically logs where the exit code was generated.
   Otherwise, it's like looking for a needle in haystack.
*)
let with_log ~__LOC__:loc x =
  Logs.debug (fun m ->
      m "Producing exit code %i (%s) at: %s" x.code x.description loc);
  x

(* this is useful because of the private declaration of t in the .mli *)
let to_int x = x.code
let of_int ~__LOC__:loc ~code ~description = with_log loc { code; description }

(*
   Legal values. This does not include those created via 'of_int' which
   are deprecated. This module is private so as to force location logging.
*)
module Value = struct
  let ok = create 0 "OK"
  let findings = create 1 "some findings"
  let fatal = create 2 "fatal error"
  let invalid_code = create 3 "invalid target code"
  let invalid_pattern = create 4 "invalid pattern"
  let unparseable_yaml = create 5 "unparseable YAML"

  (* let need_arbitrary_code_exec = 6 "need arbitrary code execution" *)
  let missing_config = create 7 "missing configuration"
  let invalid_language = create 8 "invalid language"

  (* let match_timeout = 9 "match timeout" *)
  (* let match_max_memory = 10 "match memory limit exceeded" *)
  (* let lexical_error = 11 "lexical error" *)
  (* let too_many_matches = 12 "too many matches" *)
  let invalid_api_key = create 13 "invalid API key"
  let scan_fail = create 14 "scan failure"
  let not_implemented_in_osemgrep = create 99 "not implemented in osemgrep"
end

(*
   Exit codes of the semgrep command.
   Commented out definitions show the codes that are no longer in use.

   This mechanism guarantees that creating an exit code results in the location
   being logged.

   Alternatively, we could store this location and log it at the time
   of calling 'exit'.
*)
let ok ~__LOC__:loc = with_log loc Value.ok
let findings ~__LOC__:loc = with_log loc Value.findings
let fatal ~__LOC__:loc = with_log loc Value.fatal

(* a.k.a. target_parse_failure *)
let invalid_code ~__LOC__:loc = with_log loc Value.invalid_code

(* a.k.a rule_parse_failure *)
let invalid_pattern ~__LOC__:loc = with_log loc Value.invalid_pattern
let unparseable_yaml ~__LOC__:loc = with_log loc Value.unparseable_yaml
let missing_config ~__LOC__:loc = with_log loc Value.missing_config
let invalid_language ~__LOC__:loc = with_log loc Value.invalid_language
let invalid_api_key ~__LOC__:loc = with_log loc Value.invalid_api_key
let scan_fail ~__LOC__:loc = with_log loc Value.scan_fail

(* Temporary until either osemgrep dies or replaces semgrep. *)
let not_implemented_in_osemgrep ~__LOC__:loc =
  with_log loc Value.not_implemented_in_osemgrep

let equal a b = Int.equal a.code b.code

module Equal = struct
  let ok = equal Value.ok
  let findings = equal Value.findings
  let fatal = equal Value.fatal
  let invalid_code = equal Value.invalid_code
  let invalid_pattern = equal Value.invalid_pattern
  let unparseable_yaml = equal Value.unparseable_yaml
  let missing_config = equal Value.missing_config
  let invalid_language = equal Value.invalid_language
  let invalid_api_key = equal Value.invalid_api_key
  let scan_fail = equal Value.scan_fail
  let not_implemented_in_osemgrep = equal Value.not_implemented_in_osemgrep
end

let testable : t Alcotest.testable =
  let print fmt x = Format.fprintf fmt "%i (%s)" x.code x.description in
  Alcotest.testable print equal

let check expected actual = Alcotest.check testable "exit code" expected actual

module Check = struct
  let ok = check Value.ok
  let findings = check Value.findings
  let fatal = check Value.fatal
  let invalid_code = check Value.invalid_code
  let invalid_pattern = check Value.invalid_pattern
  let unparseable_yaml = check Value.unparseable_yaml
  let missing_config = check Value.missing_config
  let invalid_language = check Value.invalid_language
  let invalid_api_key = check Value.invalid_api_key
  let scan_fail = check Value.scan_fail
  let not_implemented_in_osemgrep = check Value.not_implemented_in_osemgrep
end
