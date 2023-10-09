type conf = { exclude_rule_ids : Rule_ID.t list; severity : Rule.severity list }
[@@deriving show]

val filter_rules : conf -> Rule.rules -> Rule.rules
