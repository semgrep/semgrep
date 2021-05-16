type ('a, 'b) transformer = 'a -> 'b -> Metavars_php.metavars_binding list

(* this works by side effect on the second argument and its .transfo field *)
val transform_e_e :
  Cst_php.expr -> Cst_php.expr -> Metavars_php.metavars_binding -> unit

val transform_st_st :
  Cst_php.stmt -> Cst_php.stmt -> Metavars_php.metavars_binding -> unit

val transform_xhp_xhp :
  Cst_php.xhp_html -> Cst_php.xhp_html -> Metavars_php.metavars_binding -> unit

val transform_hint_hint :
  Cst_php.hint_type ->
  Cst_php.hint_type ->
  Metavars_php.metavars_binding ->
  unit
