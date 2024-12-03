(**
Fold over the variables bound by the parameters of a function definition.

Used e.g. to construct taint environments, see 'Taint_input_env'.
*)

val fold :
  ('acc ->
  AST_generic.ident ->
  AST_generic.id_info ->
  AST_generic.expr option (** default value *) ->
  'acc) ->
  'acc ->
  IL.param list ->
  'acc
