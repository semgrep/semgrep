(* Parse a rule file.
 *
 * The parser accepts invalid rules, skip them, and return them in
 * the list of errors.
 * This will not raise Rule.InvalidRule exceptions.
 *
 * This function may also raise the other exns in Rule (e.g., InvalidYaml).
 *)
val parse_and_filter_invalid_rules :
  Common.filename -> Rule.rules * Rule.invalid_rule_error list

(* This should be used in testing code. Otherwise you should
 * use parse_and_filter_invalid_rules.
 * This function may raise the exns in Rule (e.g., InvalidRule).
 *)
val parse : Common.filename -> Rule.rules

(* this can be used for parsing -e/-f extended patterns in Run_semgrep.ml *)
val parse_xpattern : Xlang.t -> string Rule.wrap -> Rule.xpattern
