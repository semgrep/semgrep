type conf = {
  exclude_rule_ids : Rule_ID.t list;
  severity : Rule.severity list;
  exclude_products : Semgrep_output_v1_t.product list;
}
[@@deriving show]

val get_rule_product_from_metadata : Rule.rule -> Semgrep_output_v1_t.product
val filter_rules : conf -> Rule.rules -> Rule.rules
