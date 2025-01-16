(*
   'semgrep ci' command-line parsing.
*)

(*
   The result of parsing a 'semgrep ci' command.
*)
type conf = {
  audit_on : string list;
  dry_run : bool;
  suppress_errors : bool;
  (* --code/--sca/--secrets/ *)
  products : Semgrep_output_v1_t.product list;
  (* for monorepos *)
  subdir : Fpath.t option;
  (* BIG ONE: 'semgrep ci' shares many flags with 'semgrep scan' *)
  scan_conf : Scan_CLI.conf;
  (* internal only *)
  x_distributed_scan_conf : Distributed_scan_stub.conf;
  (* osemgrep-only options *)
  (* path to fake responses for testing purpose (see tests/ci/fake_backend/) *)
  fake_backend : Fpath.t option;
  (* path to log dir to save all comms with backend for debugging purpose *)
  log_backend : Fpath.t option;
}
[@@deriving show]

(*
   Usage: parse_argv [| "semgrep-ci"; <args> |]

   Turn argv into a conf structure.

   This function may raise an exn in case of an error parsing argv
   but this should be caught by CLI.safe_run.
*)
val parse_argv : string array -> conf
