type products = SAST | SCA | Secrets | Interfile [@@deriving show]

type conf = {
  exclude_rule_ids : Rule_ID.t list;
  severity : Rule.severity list;
  exclude_products : products list;
}
[@@deriving show]

val get_rule_product : Rule.rule -> products
val filter_rules : conf -> Rule.rules -> Rule.rules
