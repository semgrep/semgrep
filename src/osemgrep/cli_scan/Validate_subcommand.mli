(* There is currently no 'semgrep validate' subcommand. Rule validations
 * are run via 'semgrep scan --validate ...' but internally it's quite
 * similar to a subcommand.
 *)

type conf = {
  rules_source : Rules_source.t;
  core_runner_conf : Core_runner.conf;
  common : CLI_common.conf;
}
[@@deriving show]

val run : conf -> Exit_code.t
