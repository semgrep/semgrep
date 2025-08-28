(*
   Copyright (c) 2022-2024 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
type conf = {
  exclude_rule_ids : Rule_ID.t list;
  severity : Rule.severity list;
  exclude_products : Semgrep_output_v1_t.product list;
}
[@@deriving show]

val get_rule_product_from_metadata : Rule.rule -> Semgrep_output_v1_t.product
val filter_rules : conf -> Rule.rules -> Rule.rules
