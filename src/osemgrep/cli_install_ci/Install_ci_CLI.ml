open Common
module Arg = Cmdliner.Arg
module Term = Cmdliner.Term
module Cmd = Cmdliner.Cmd
module H = Cmdliner_

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
   'semgrep install-ci ...' command-line arguments processing.
*)

(*****************************************************************************)
(* Types and constants *)
(*****************************************************************************)
type ci_env_flavor = Github [@@deriving show]

type repo_kind =
  | Dir of Fpath.t (* local directory *)
  | Repository of string * string (* owner, repo *)
[@@deriving show]

type conf = {
  ci_env : ci_env_flavor;
  repo : repo_kind;
  update : bool;
  dry_run : bool;
  common : CLI_common.conf;
}
[@@deriving show]

(*****************************************************************************)
(* Manpage Documentation *)
(*****************************************************************************)
let doc = "Install semgrep as a workflow within a CI environment."

let long_desc =
  Printf.sprintf
    {|%s Specify a git repository path (e.g. ".") or qualified repo name (e.g. "owner/repo"),
and this command will add the semgrep actions workflow. The workflow file will run `semgrep ci`
on every pull request opened on a main branch.

NOTE: Currently, this command only supports Github Actions.
|}
    doc

let man : Cmdliner.Manpage.block list =
  [ `S Cmdliner.Manpage.s_description; `P long_desc ]
  @ CLI_common.help_page_bottom

let cmdline_info : Cmd.info = Cmd.info "semgrep install-ci" ~doc ~man

(*****************************************************************************)
(* Flags *)
(*****************************************************************************)
(* TODO: Maybe support other CI environments? *)
let o_ci_env : string Term.t =
  let doc = "Specify a CI environment." in
  Arg.(value & opt string "Github" & info [ "env" ] ~docv:"CI_ENV" ~doc)

(* NOTE: Takes precedence over o_repo_pos *)
let o_repo_kw : string Term.t =
  let doc = "Specify a repository path or repo name." in
  Arg.(value & opt string "." & info [ "r"; "repo" ] ~docv:"REPO_PATH" ~doc)

let o_repo_pos : string Term.t =
  let doc = "Specify a repository path or repo name." in
  Arg.(value & pos 0 string "." & info [] ~docv:"REPO_PATH" ~doc)

(* NOTE: Should only be required when previous attempts only partially succeeded *)
let o_update : bool Term.t =
  let doc = "Update any existing workflow and secrets (absent=false)." in
  Arg.(value & flag & info [ "u"; "update" ] ~doc)

(* NOTE: Should only be required when previous attempts only partially succeeded *)
let o_dry_run : bool Term.t =
  let doc =
    "Simulate outputs without actually running any system commands \
     (absent=false)."
  in
  Arg.(value & flag & info [ "n"; "dryrun"; "dry-run" ] ~doc)

(*************************************************************************)
(* Command-line parsing: turn argv into conf *)
(*************************************************************************)
let cmdline_term =
  (* coupling: the parameters must be in alphabetic order which makes
   * it easier to add new flags as the order must be the same further
   * below after 'const combine'
   *)
  let combine ci_env common dry_run repo_kw repo_pos update =
    let repo_arg = if repo_kw = "." then repo_pos else repo_kw in
    let repo =
      match repo_arg with
      | "." -> Dir (Fpath.v ".")
      | _ when UFile.is_dir ~follow_symlinks:true (Fpath.v repo_arg) ->
          Dir (Fpath.v repo_arg)
      | _ ->
          let owner, repo =
            match String.split_on_char '/' repo_arg with
            | [ owner; repo ] -> (owner, repo)
            | _ -> Error.abort (spf "invalid repo: %s" repo_arg)
          in
          Repository (owner, repo)
    in
    let ci_env =
      match String.lowercase_ascii ci_env with
      | "github" -> Github
      | s -> Error.abort (spf "CI_ENV '%s' not supported!" s)
    in
    { ci_env; common; dry_run; repo; update }
  in
  Term.(
    const combine $ o_ci_env $ CLI_common.o_common $ o_dry_run $ o_repo_kw
    $ o_repo_pos $ o_update)

let parse_argv (argv : string array) : conf =
  let cmd : conf Cmd.t = Cmd.v cmdline_info cmdline_term in
  CLI_common.eval_value ~argv cmd
