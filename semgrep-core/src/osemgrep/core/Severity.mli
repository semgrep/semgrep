type basic_severity = [ `Error | `Warning ] [@@deriving show]
type rule_severity = [ basic_severity | `Info ] [@@deriving show]

type extended_severity = [ rule_severity | `Inventory | `Experiment ]
[@@deriving show]

(* for CLI Json output of semgrep errors *)
val string_of_basic_severity : basic_severity -> string

(* LATER: get rid off at some point *)
val rule_severity_of_rule_severity_opt : Rule.severity -> rule_severity option

(* for CLI --severity xxx parsing *)
val converter : rule_severity Cmdliner.Arg.conv
