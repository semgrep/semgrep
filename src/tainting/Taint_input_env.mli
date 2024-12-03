(** Input environments for taint analysis. *)

val mk_fun_input_env :
  Taint_rule_inst.t ->
  ?glob_env:Taint_lval_env.t ->
  IL.param list ->
  Taint_lval_env.t * Shape_and_sig.Effects.t
(** Constructs the initial taint environment for a given function definition.
    Essentially, it records the parameters that are taint sources, or whose
    default value is a taint source.
    It is exposed to be used by inter-file taint analysis in Pro.  *)

val mk_file_env :
  Taint_rule_inst.t ->
  AST_generic.program ->
  Taint_lval_env.t * Shape_and_sig.Effects.t
(** Constructs the global taint environment for a given file: it finds global-
  and class- variable definitions that are final, and checks if they are tainted. *)
