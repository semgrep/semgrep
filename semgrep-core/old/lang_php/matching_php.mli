(* If the list returned is empty, then no match was found.
 * Note that [[]] means one match was found but the environment
 * is empty (because your pattern didn't contain any metavariable for
 * instance).
 *)
type ('a, 'b) matcher = 'a -> 'b -> Metavars_php.metavars_binding list

(* right now it does not do side effects on the first argument
 * (as we do in coccinelle), but it could at some point
 *)
val match_e_e : (Cst_php.expr, Cst_php.expr) matcher

val match_st_st : (Cst_php.stmt, Cst_php.stmt) matcher

val match_xhp_xhp : (Cst_php.xhp_html, Cst_php.xhp_html) matcher

val match_hint_hint : (Cst_php.hint_type, Cst_php.hint_type) matcher
