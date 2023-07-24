type conf = {
  exclude_rule_ids : Rule_ID.t list;
  severity : Severity.rule_severity list;
}
[@@deriving show]

val filter_rules : conf -> Rule.rules -> Rule.rules
