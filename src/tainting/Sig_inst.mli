(** Instantiation of taint signatures *)

val instantiate_function_signature :
  Taint_lval_env.t ->
  check_lval:(IL.lval -> Taint.Taint_set.t * Shape_and_sig.Shape.shape) ->
  AST_generic.parameters ->
  Shape_and_sig.Signature.t ->
  IL.exp ->
  AST_generic.expr ->
  IL.exp IL.argument list ->
  (Taint.Taint_set.t * Shape_and_sig.Shape.shape) IL.argument list ->
  [ `ToSink of
    Shape_and_sig.Effect.taint_to_sink_item list * Shape_and_sig.Effect.sink
  | `ToReturn of
    Taint.Taint_set.t * Shape_and_sig.Shape.shape * Taint.Taint_set.t * Tok.t
  | `ToLval of Taint.Taint_set.t * IL.lval ]
  list
  option
(** Instantiation is meant to replace the taint and shape variables in the
 * signature of a callee function, with the taints and shapes of the parameters
 * at the call site.
 *)
