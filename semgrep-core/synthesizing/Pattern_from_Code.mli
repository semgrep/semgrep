
(* ex:
 *  ["exact match", <metrics.send('my-report-id')>;
 *   "one argument", <metrics.send($X)>;
 *   "zero or more arguments", <metrics.send(...)>;
 *  ]
 *)
type named_variants =
  (string * Pattern.t) list

(* limited to expressions for now *)
val patterns_from_code:
  AST_generic.expr -> named_variants
