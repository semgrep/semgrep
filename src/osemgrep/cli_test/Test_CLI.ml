module Arg = Cmdliner.Arg
module Term = Cmdliner.Term
module Cmd = Cmdliner.Cmd
module H = Cmdliner_

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
   'semgrep test' command-line arguments processing.
*)

(*****************************************************************************)
(* Types and constants *)
(*****************************************************************************)

(*
   The result of parsing a 'semgrep test' command.
   This is also used in Scan_CLI.ml to transform legacy
   commands such as 'semgrep scan --tests <dir>' into the
   new 'semgrep test <dir>'
*)
type conf = {
  target : target_kind;
  ignore_todo : bool;
  (* TODO? do we need those options? people use the JSON output?
   * the playground? and the optimizations and strict?
   *)
  json : bool;
  (* take the whole core_runner_conf? like for validate? *)
  optimizations : bool;
  strict : bool;
  common : CLI_common.conf;
}

(* alt: we could accept multiple dirs, and multiple files
 * TODO? should we restrict the config_str to File or Dir?
 *)
and target_kind =
  | Dir of Fpath.t * Rules_config.config_string option (* optional --config *)
  | File of Fpath.t * Rules_config.config_string (* mandatory --config *)
[@@deriving show]

(*************************************************************************)
(* Command-line flags *)
(*************************************************************************)

(* ------------------------------------------------------------------ *)
(* Flags *)
(* ------------------------------------------------------------------ *)
let o_test_ignore_todo : bool Term.t =
  H.negatable_flag [ "test-ignore-todo" ] ~neg_options:[ "no-test-ignore-todo" ]
    ~default:false
    ~doc:
      {|If --test-ignore-todo, ignores rules marked as '#todoruleid:' in
test files.
|}

let o_json : bool Term.t =
  let info = Arg.info [ "json" ] ~doc:{|Output results in JSON format.|} in
  Arg.value (Arg.flag info)

(* coupling: similar to Scan_CLI.o_strict? *)
let o_strict : bool Term.t =
  let info = Arg.info [ "strict" ] ~doc:{|???.|} in
  Arg.value (Arg.flag info)

(* coupling: Scan_CLI.o_config *)
let o_config : string list Term.t =
  let info =
    Arg.info [ "c"; "f"; "config" ]
      ~env:(Cmd.Env.info "SEMGREP_RULES")
      ~doc:
        {|YAML configuration file, directory of YAML files ending in
.yml|.yaml, URL of a configuration file, or Semgrep registry entry name.

Use --config auto to automatically obtain rules tailored to this project;
your project URL will be used to log in to the Semgrep registry.

To run multiple rule files simultaneously, use --config before every YAML,
URL, or Semgrep registry entry name.
For example `semgrep --config p/python --config myrules/myrule.yaml`

See https://semgrep.dev/docs/writing-rules/rule-syntax for information on
configuration file format.
|}
  in
  Arg.value (Arg.opt_all Arg.string [] info)

(* ------------------------------------------------------------------ *)
(* Positional arguments *)
(* ------------------------------------------------------------------ *)

let o_args : string list Term.t =
  let info =
    Arg.info [] ~docv:"STRINGS" ~doc:{|Directories containing tests.|}
  in
  Arg.value (Arg.pos_all Arg.string [] info)

(*************************************************************************)
(* Command-line parsing: turn argv into conf *)
(*************************************************************************)
let target_kind_of_roots_and_config target_roots config =
  match (target_roots, config) with
  | [ x ], [ config ] ->
      let file_str = Fpath.to_string x in
      if Sys.file_exists file_str && Sys.is_directory file_str then
        Dir (x, Some config)
      else File (x, config)
  | [ x ], [] ->
      let file_str = Fpath.to_string x in
      if Sys.is_directory file_str then Dir (x, None)
      else
        (* was raise Exception but cleaner abort I think *)
        Error.abort "--config is required when running a test on single file"
  | _ :: _ :: _, _ ->
      (* stricter: better error message '(directory or file)' *)
      Error.abort "only one target (directory or file) allowed for tests"
  | _, _ :: _ :: _ ->
      (* stricter: removed 'config directory' *)
      Error.abort "only one config allowed for tests"
  | [], _ -> Error.abort "no valid target roots"

let cmdline_term : conf Term.t =
  (* !The parameters must be in alphabetic orders to match the order
   * of the corresponding '$ o_xx $' further below! *)
  let combine args common config json strict test_ignore_todo =
    let target =
      target_kind_of_roots_and_config (Fpath_.of_strings args) config
    in
    {
      target;
      strict;
      json;
      ignore_todo = test_ignore_todo;
      common;
      optimizations = true;
    }
  in
  Term.(
    const combine $ o_args $ CLI_common.o_common $ o_config $ o_json $ o_strict
    $ o_test_ignore_todo)

let doc = "testing the rules"

let man : Cmdliner.Manpage.block list =
  [
    `S Cmdliner.Manpage.s_description;
    `P "See https://semgrep.dev/docs/writing-rules/testing-rules/";
  ]
  @ CLI_common.help_page_bottom

let cmdline_info : Cmd.info = Cmd.info "semgrep test" ~doc ~man

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let parse_argv (argv : string array) : conf =
  let cmd : conf Cmd.t = Cmd.v cmdline_info cmdline_term in
  CLI_common.eval_value ~argv cmd
