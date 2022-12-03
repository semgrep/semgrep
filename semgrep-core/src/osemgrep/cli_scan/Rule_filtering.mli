type conf = {
  exclude_rule_ids : Rule.rule_id list;
  severity : Severity.rule_severity list;
}
[@@deriving show]

val filter_rules : conf -> Rule.rules -> Rule.rules
