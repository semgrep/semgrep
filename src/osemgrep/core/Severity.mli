type t = [ `Error | `Warning | `Info ] [@@deriving show]

(* for CLI Json output of semgrep errors *)
val to_string : t -> string

(* LATER: get rid off at some point *)
val of_rule_severity_opt : Rule.severity -> t option

(* for CLI --severity xxx parsing *)
val converter : t Cmdliner.Arg.conv
