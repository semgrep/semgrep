
val find_template_inf_sup:
  Token_views_cpp.token_extended list -> unit

val find_template_commentize:
  Token_views_cpp.multi_grouped list -> unit
val find_qualifier_commentize:
  Token_views_cpp.token_extended list -> unit

val find_constructor_outside_class:
  Token_views_cpp.token_extended list -> unit
val find_constructor:
  Token_views_cpp.token_extended list -> unit
val find_constructed_object_and_more:
  Token_views_cpp.token_extended list -> unit
val reclassify_tokens_before_idents_or_typedefs:
  Token_views_cpp.multi_grouped list -> unit
