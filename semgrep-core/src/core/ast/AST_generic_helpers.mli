val str_of_ident : AST_generic.ident -> string

val expr_to_pattern : AST_generic.expr -> AST_generic.pattern

val expr_to_type : AST_generic.expr -> AST_generic.type_

exception NotAnExpr

val pattern_to_expr : AST_generic.pattern -> AST_generic.expr

val name_or_dynamic_to_expr :
  AST_generic.name_or_dynamic -> AST_generic.id_info option -> AST_generic.expr

val vardef_to_assign :
  AST_generic.entity * AST_generic.variable_definition -> AST_generic.expr

val funcdef_to_lambda :
  AST_generic.entity * AST_generic.function_definition ->
  AST_generic.resolved_name option ->
  AST_generic.expr

val name_of_entity :
  AST_generic.entity -> (AST_generic.ident * AST_generic.id_info) option

val name_of_ids :
  ?name_typeargs:AST_generic.type_arguments option ->
  AST_generic.dotted_ident ->
  AST_generic.name

val opt_to_label_ident : AST_generic.ident option -> AST_generic.label_ident

val gensym_counter : int ref

val gensym : unit -> int

val has_keyword_attr :
  AST_generic.keyword_attribute -> AST_generic.attribute list -> bool

val abstract_for_comparison_any : AST_generic.any -> AST_generic.any
(** Abstract away position and constness for comparison
 * with polymorphic operators.
*)

val conv_op : AST_generic_.operator -> AST_generic.operator

val conv_incr : AST_generic_.incr_decr -> AST_generic.incr_decr

val conv_prepost : AST_generic_.prefix_postfix -> AST_generic.prefix_postfix

val conv_incdec :
  AST_generic_.incr_decr * AST_generic_.prefix_postfix ->
  AST_generic.incr_decr * AST_generic.prefix_postfix

val conv_class_kind :
  AST_generic_.class_kind * Parse_info.t ->
  AST_generic.class_kind * Parse_info.t

val conv_function_kind :
  AST_generic_.function_kind * Parse_info.t ->
  AST_generic.function_kind * Parse_info.t
