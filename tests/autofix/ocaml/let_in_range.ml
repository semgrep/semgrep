(* bugfix: `let` expressions previously had the wrong range, which led to
 * autofix bugs *)
(* MATCH: *)
let x = old_value in x + 1
