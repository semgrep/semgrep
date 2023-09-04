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
type repo_kind = Dir of Fpath.t (* local directory *) [@@deriving show]

type conf = {
  ci_env : ci_env_flavor;
  logging_level : Logs.level option;
  repo : repo_kind;
}
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

let o_repo_kw : string Term.t =
  let doc = "Specify a repository path." in
  Arg.(value & opt string "." & info [ "r"; "repo" ] ~docv:"REPO_PATH" ~doc)

let o_repo_pos : string Term.t =
  let doc = "Specify a repository path (via positional argument)." in
  Arg.(value & pos 0 string "." & info [] ~docv:"REPO_PATH" ~doc)

(*************************************************************************)
(* Command-line parsing: turn argv into conf *)
(*************************************************************************)
let cmdline_term =
  let combine logging_level repo_kw provider repo_pos =
    let repo_arg = if repo_kw = "." then repo_pos else repo_kw in
    let repo = Dir (Fpath.v repo_arg) in
    let ci_env =
      let provider = String.lowercase_ascii provider in
      match provider with
      | "github" -> Github
      | _ -> Error.abort (spf "ci env not supported: %s" provider)
    in
    { logging_level; ci_env; repo }
  in
  Term.(
    const combine $ CLI_common.o_logging $ o_repo_kw $ o_ci_env $ o_repo_pos)

let parse_argv (argv : string array) : conf =
  let cmd : conf Cmd.t = Cmd.v cmdline_info cmdline_term in
  CLI_common.eval_value ~argv cmd
