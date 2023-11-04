type eval_strategy = EvalSubst | EvalEnvir [@@deriving show]

val default_strategy : eval_strategy

val eval_program :
  ?strategy:eval_strategy -> Core_jsonnet.program -> Value_jsonnet.t

val manifest_value : ?strategy:eval_strategy -> Value_jsonnet.t -> JSON.t
