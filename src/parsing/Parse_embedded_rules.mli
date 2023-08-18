(* check whether the target file has the special embedded semgrep tag *)
val has_embedded_rules : Fpath.t -> bool

(* extract the rules in the target file; returns empty lists
 * if the file has no embedded rules.
 *)
val extract_rules : Fpath.t -> Rule.rules * Rule.invalid_rule_error list
