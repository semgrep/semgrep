type product = SAST | SCA | Secrets | Interfile [@@deriving show]

type conf = {
  exclude_rule_ids : Rule_ID.t list;
  severity : Rule.severity list;
  exclude_products : product list;
}
[@@deriving show]

val get_rule_product_from_metadata : Rule.rule -> product
val filter_rules : conf -> Rule.rules -> Rule.rules
