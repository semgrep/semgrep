
val filter_for_typedef:
  Token_views_cpp.multi_grouped list -> Token_views_cpp.token_extended list list

(* We use a list list because the template arguments are passed separately
 * TODO: right now we actually skip template arguments ...
*)
val find_typedefs:
  Token_views_cpp.token_extended list list -> unit
