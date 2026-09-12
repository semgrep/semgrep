(*
   Assign good names to productions in the grammar, based on the
   enclosing rule name and the contents of the production.
*)

(*
   Produce a somewhat human-readable name from a rule body.
   The resulting name may be long and not unique.
   A hash can be appended to minimize conflicts, see 'hash_rule_body' below.
*)
val name_ts_rule_body : Tree_sitter_grammar.Rule_body.t -> string
val name_rule_body : CST_grammar.rule_body -> string

(*
   Produce a hexadecimal hash of the rule body like 'd41d8cd'.
   These are short and unlikely to conflict with the hash of another rule.
*)
val hash_rule_body : CST_grammar.rule_body -> string

(*
   Assign constructor names suitable for classic variants and polymorphic
   variants, from rule bodies. The containing rule name is only for
   better error messages.
*)
val assign_case_names :
  ?rule_name: string ->
  CST_grammar.rule_body list ->
  (string * CST_grammar.rule_body) list
