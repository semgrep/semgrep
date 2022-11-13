type basic_severity = [ `Error | `Warning ]
type rule_severity = [ basic_severity | `Info ]
type extended_severity = [ rule_severity | `Inventory | `Experiment ]

(* for CLI Json output of semgrep errors *)
val string_of_basic_severity : basic_severity -> string

(* LATER: get rid off at some point *)
val rule_severity_of_rule_severity_opt : Rule.severity -> rule_severity option

(* for CLI --severity xxx parsing *)
val converter : rule_severity Cmdliner.Arg.conv
