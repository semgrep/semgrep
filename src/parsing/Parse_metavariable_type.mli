(* Wrap and unwrap type expressions for metavariable type rule.
 *
 * Language parsers typically do not support multiple top-level entry points,
 * and there can be ambiguity when a single identifier is interpreted as both
 * an expression and a type. To avoid unexpected behavior caused by parsing
 * conflicts, we wrap the input string into an expression to explicitly extract
 * the type and later unwrap it after parsing. For example, in Java, we wrap
 * the type in the casting expression `([type] x)` to capture the parsed type.
 * The variable name `x` is chosen arbitrarily and ignored during unwrapping.
 *)

val wrap_type_expr : Language.t -> string -> string option
val unwrap_type_expr : Language.t -> AST_generic.any -> AST_generic.type_ option
