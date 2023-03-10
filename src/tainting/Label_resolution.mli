module LabelSet : Set.S with type elt = string

val needed_labels_of_expr : AST_generic.expr -> LabelSet.t option
