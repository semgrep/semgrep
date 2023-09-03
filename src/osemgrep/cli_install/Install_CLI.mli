(*
   'semgrep install' command-line parsing.
*)

(*
   The result of parsing a 'semgrep install' command.
*)

type ci_env_flavor = Github [@@deriving show]
type conf = { ci_env : ci_env_flavor } [@@deriving show]

val parse_argv : string array -> conf
