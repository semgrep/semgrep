(*
  'semgrep validate' command-line parsing
*)

(* The result of parsing a 'semgrep validate' command. This is also used in
 * Scan_CLI.ml to transform legacy commands such as 'semgrep --validate <dir>'
 * into the new 'semgrep validate <dir>'
 *)
type conf = {
  rules_source : Rules_source.t;
  pro : bool;
  core_runner_conf : Core_runner.conf;
  common : CLI_common.conf;
}
[@@deriving show]

val parse_argv : string array -> conf
