module Arg = Cmdliner.Arg
module Term = Cmdliner.Term
module Cmd = Cmdliner.Cmd
module H = Cmdliner_
open Fpath_.Operators

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
   'semgrep test' command-line arguments processing.
*)

(*****************************************************************************)
(* Types and constants *)
(*****************************************************************************)

(* The result of parsing a 'semgrep test' command. This is also used in
 * Scan_CLI.ml to transform legacy commands such as 'semgrep scan --tests <dir>'
 * into the new 'semgrep test <dir>'
 *)
type conf = {
  target : target_kind;
  (* LATER: separate pro_language: bool; pro_intrafile: bool.
   * TODO: for now it just gives access to proprietary parsers in
   * osemgrep-pro so one can run tests on semgrep-rules/elixir/
   *)
  pro : bool;
  (* ??? *)
  ignore_todo : bool;
  (* TODO? do we need those options? people use the JSON output?
   * the playground? and the optimizations and strict?
   *)
  json : bool;
  (* take the whole core_runner_conf? like for validate? *)
  optimizations : bool;
  strict : bool;
  matching_diagnosis : bool;
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

(* coupling: similar to Scan_CLI.o_pro but currently has a different meaning
 * alt: move those options to CLI_common.ml at some point
 *)
let o_pro : bool Term.t =
  let info =
    Arg.info [ "pro" ]
      ~doc:
        (" support pro languages (currently Apex and Elixir)"
       ^ CLI_common.blurb_pro)
  in
  Arg.value (Arg.flag info)

(* coupling: similar to Scan_CLI.o_strict? *)
(* TODO: be stricter when parsing target files; reject files that partially
 * parse.
 *)
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
|}
  in
  Arg.value (Arg.opt_all Arg.string [] info)

(* osemgrep-only: brandon's experiment *)
let o_matching_diagnosis : bool Term.t =
  let info =
    Arg.info [ "matching-diagnosis" ]
      ~doc:
        {|Whether to emit "matching diagnosis", which analyzes failing
test annotation cases and matching explanations to determine
why a rule did or did not match.|}
  in
  Arg.value (Arg.flag info)

(* ------------------------------------------------------------------ *)
(* Positional arguments *)
(* ------------------------------------------------------------------ *)

(* TODO: we accept just one elt here, so why not use just Arg.pos? *)
let o_args : string list Term.t =
  let info =
    Arg.info [] ~docv:"STRINGS" ~doc:{|Directory or file containing tests.|}
  in
  Arg.value (Arg.pos_all Arg.string [] info)

(*************************************************************************)
(* Command-line parsing: turn argv into conf *)
(*************************************************************************)
let target_kind_of_roots_and_config target_roots config =
  match (target_roots, config) with
  | [ file ], [ config ] ->
      if Sys.file_exists !!file && Sys.is_directory !!file then
        Dir (file, Some config)
      else File (file, config)
  | [ file ], [] ->
      if Sys.is_directory !!file then Dir (file, None)
      else
        (* was raise Exception but cleaner abort I think *)
        Error.abort "--config is required when running a test on single file"
  | _ :: _ :: _, _ ->
      (* stricter: better error message '(directory or file)' *)
      Error.abort "only one target (directory or file) allowed for tests"
  | _, _ :: _ :: _ ->
      (* stricter: removed 'config directory' *)
      Error.abort "only one config allowed for tests"
  | [], _ -> Error.abort "bug: no valid target roots"

let cmdline_term : conf Term.t =
  (* !The parameters must be in alphabetic orders to match the order
   * of the corresponding '$ o_xx $' further below! *)
  let combine args common config json matching_diagnosis pro strict
      test_ignore_todo =
    let target =
      target_kind_of_roots_and_config (Fpath_.of_strings args) config
    in
    {
      target;
      pro;
      strict;
      json;
      ignore_todo = test_ignore_todo;
      common;
      optimizations = true;
      matching_diagnosis;
    }
  in
  Term.(
    const combine $ o_args $ CLI_common.o_common $ o_config $ o_json
    $ o_matching_diagnosis $ o_pro $ o_strict $ o_test_ignore_todo)

let doc = "testing the rules (EXPERIMENTAL improvements over scan --test)"

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
