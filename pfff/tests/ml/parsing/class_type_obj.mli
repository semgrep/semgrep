class type cfg_visitor = object

  method visit_stmt : stmt Visitor.visit_method

  method visit_id : identifier Visitor.visit_method
  method visit_literal : literal Visitor.visit_method
  method visit_expr : expr Visitor.visit_method
  method visit_lhs : lhs Visitor.visit_method
  method visit_tuple : tuple_expr Visitor.visit_method
  method visit_rescue_guard : rescue_guard Visitor.visit_method
  method visit_def_name : def_name Visitor.visit_method
  method visit_class_kind : class_kind Visitor.visit_method
  method visit_method_param : method_formal_param Visitor.visit_method
  method visit_msg_id : msg_id Visitor.visit_method
  method visit_block_param : block_formal_param Visitor.visit_method
end
