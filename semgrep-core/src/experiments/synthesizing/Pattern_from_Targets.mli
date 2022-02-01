(*
 * The pattern_from_targets feature allows users to highlight separate code
 * snipppets and autogenerate patterns that match all snippets
 *
 * Consider if you have foo(3) and bar(3, m). We would like to
 * generate the pattern $X(3, ...), which is the "most specific" pattern
 * that matches both
 *
 * To do so, we follow a breadth first tree comparison. At each level, we
 * will produce all patterns that could match the tree if only that much
 * was revealed. In the above example, we would first generate
 *
 * ... and ...
 *
 * We then intersect the two sets and proceed with the patterns that appear
 * in both. Since ... is in both, we continue with that on each side
 *
 * The next pattern that matches both is
 *
 * $X(...) and $X(...)
 *
 * Once again, this appears in both. Now, we want to try exploring both
 * $X and ... further. First, we try $X
 *
 * foo(...) and bar(...)
 *
 * The intersection is now empty, so we try substituting into the ... instead
 *
 * $X(..., $Y, ...) and $X(..., $Y, ...), $X(..., $Y, ...)
 *
 * The latter pattern appears repeated, but actually the $X represents two
 * different potential arguments: 3 and m. This will be apparent in the next
 * generation
 *
 * $X(..., 3, ...) and $X(..., 3, ...), $X(..., m, ...)
 *
 * This will then proceed to explore the $X(..., 3, ....), and so on, always
 * going from a more general pattern to a more specific pattern
 *
 * When the intersection is empty and there are no more possible routes to try
 * (at least one target is out of replacement functions), return the last set
 * of patterns that was in the intersection of all the targets
 *
 * To implement this algorithm, for each target, we generate a list of
 * possible patterns. These patterns need to not only save the current
 * pattern (e.g. $X(...)) but also how to fill each hole. Filling a hole
 * requires instructions for where the hole is, but also what to use to
 * generate replacement patterns.
 *
 * These instructions need to be as general as possible, since they essentially
 * only indicate the position of the hole. Therefore, the function takes
 * instructions for how to convert the hole (e.g. fun x -> x just replaces it)
 * and then the actual pattern to convert. This allows us to recurse without
 * worrying in the child about the parent's pattern
 *
 * The type also includes an environment, since each pattern must keep track of
 * its individual state, such as how many metavariables have already been used.
 *
 * Handling targets with multiple statements:
 * -----------------------------------------
 * A user might highlight a snippet that contains multiple statements.
 *
 * For example the target snippets might be:
 * // target 1
 * x = a
 * foo(x)
 *
 * // target 2
 * y = a
 * foo(y)
 *
 * Here, the targets each have two top-level ExprStmt nodes, the assignment and
 * the call to foo. Each target is parsed into a list, in this case two targets
 * each of length 2.
 *
 * The main intersection algorithm is run on the first element of each target,
 * then the second element of each target, and so on.. A pattern is first
 * generated for [x = a; y = a] yielding $X = a, then for [foo(x); foo(y)]
 * yielding foo($X).
 *
 * In this example we want the metavariable to be re-used within a target.
 * $X = a     is better than    $X = a
 * foo($X)                      foo($Y)
 *
 * To accomplish this, the metavariable environment is retained between
 * target sets. The metavariable count and mappings established when
 * the pattern $X = a is generated for for [x = a; y = a] is passed on to the
 * starting patterns when the pattern exec($X) is being generated from
 * [foo(x); foo(y)].
 *
 * For now each target must be the same length (no ellipsis support).
 *)

val generate_patterns :
  Config_semgrep.t -> AST_generic.any list -> Lang.t -> Pattern.t option
