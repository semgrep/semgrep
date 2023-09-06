(*
   'semgrep install-ci ...' command-line parsing.
*)

(* we only support Github Actions (GHA) for now *)
type ci_env_flavor = Github [@@deriving show]

type repo_kind =
  | Dir of Fpath.t (* local directory, usually simply "." *)
  | Repository of string * string (* owner, repo *)
[@@deriving show]

(* The result of parsing a 'semgrep install-ci ...' command *)
type conf = {
  ci_env : ci_env_flavor;
  repo : repo_kind;
  (* To update an existing workflow (default to false).
   * Should only be required when previous attempts only partially succeeded.
   *)
  update : bool;
  dry_run : bool;
      (* simulate outputs without actually running any system commands *)
  common : CLI_common.conf;
}
[@@deriving show]

(* entry point *)
val parse_argv : string array -> conf
