type conf = { exclude_rule_ids : Rule_ID.t list; severity : Severity.t list }
[@@deriving show]

val filter_rules : conf -> Rule.rules -> Rule.rules
