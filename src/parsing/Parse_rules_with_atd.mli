(* You should probably not use this file. Use instead Parse_rule.mli *)

(* This supports just rules using the new (v2) Semgrep syntax *)
val parse_rules_v2 : Fpath.t -> Rule_schema_v2_t.rules
