open Common
module Arg = Cmdliner.Arg
module Term = Cmdliner.Term
module Cmd = Cmdliner.Cmd

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
   'semgrep install' command-line arguments processing.
*)

(*****************************************************************************)
(* Types and constants *)
(*****************************************************************************)
type ci_env_flavor = Github [@@deriving show]

type conf = { ci_env : ci_env_flavor; logging_level : Logs.level option }
[@@deriving show]

(*****************************************************************************)
(* Manpage Documentation *)
(*****************************************************************************)
let doc = "Install semgrep in CI environment"

let man : Cmdliner.Manpage.block list =
  [ `S Cmdliner.Manpage.s_description; `P "Install semgrep in CI environment" ]
  @ CLI_common.help_page_bottom

let cmdline_info : Cmd.info = Cmd.info "semgrep install" ~doc ~man

(*****************************************************************************)
(* Flags *)
(*****************************************************************************)
let o_ci_env : string Term.t =
  let doc = "Specify a CI environment." in
  Arg.(value & opt string "Github" & info [ "env" ] ~docv:"CI_ENV" ~doc)

(*************************************************************************)
(* Command-line parsing: turn argv into conf *)
(*************************************************************************)
let cmdline_term =
  let combine logging_level v =
    let ci_env =
      let v = String.lowercase_ascii v in
      match v with
      | "github" -> Github
      | _ -> Error.abort (spf "ci env not supported: %s" v)
    in
    { logging_level; ci_env }
  in
  Term.(const combine $ CLI_common.o_logging $ o_ci_env)

let parse_argv (argv : string array) : conf =
  let cmd : conf Cmd.t = Cmd.v cmdline_info cmdline_term in
  CLI_common.eval_value ~argv cmd
