
(*
 * The pattern_from_code feature allows users to highlight code and
 * autogenerate patterns that match
 *
 * There are three parts to pattern-from-code: taking a range and turning it
 * into an AST, taking an AST and modifying parts to make it a pattern,
 * and taking a pattern’s AST and outputting it.
 *
 * A highlighted section is turned into a range of the form row:col-row:col,
 * which is fed as input into semgrep-core. If you’ve cloned semgrep,
 * you can actually run it locally in semgrep/semgrep-core.
 * An example run looks like:
 * semgrep-core  -synthesize_patterns 8:5-8:21 tests/SYNTHESIZING/password.go
 * The range is turned into a character position, and then the AST is visited
 * to find the first expression in the range.
 *
 * Once we have an expression, we feed it into Pattern_from_code.from_expr,
 * which makes modifications to the AST so that it’s more general.
 * This is broken down based on what kind of expression is given.
 * Function calls and variables are kept self contained, as the smallest
 * “blocks” of expressions, but other expressions, like assignments,
 * will recursively generalize their subexpressions. The guiding philosophy
 * behind these decisions is that a pattern is only useful if it suggests
 * a possibility for a user.
 *
 * Finally, we output the pattern by giving it to Pretty_print_generic.pattern_to_string,
 * a language-aware AST-to-string converter.
 *)

(* ex:
 *  ["exact match", <metrics.send('my-report-id')>;
 *   "one argument", <metrics.send($X)>;
 *   "zero or more arguments", <metrics.send(...)>;
 *  ]
*)
type named_variants =
  (string * Pattern.t) list

(* limited to expressions for now *)
val from_any:
  Config_semgrep.t -> AST_generic.any -> named_variants
