(*
   'semgrep install' command-line parsing.
*)

(*
   The result of parsing a 'semgrep install' command.
*)

type ci_env_flavor = Github [@@deriving show]

type repo_kind =
  | Dir of Fpath.t (* local directory *)
  | Repository of string * string (* owner, repo *)
[@@deriving show]

type conf = {
  ci_env : ci_env_flavor;
  logging_level : Logs.level option;
  repo : repo_kind;
  update : bool;
}
[@@deriving show]

val get_repo : repo_kind -> string
val parse_argv : string array -> conf
