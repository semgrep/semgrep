type eval_strategy = EvalSubst | EvalEnvir [@@deriving show]

let default_strategy = EvalEnvir

let eval_program ?(strategy = default_strategy) prog =
  match strategy with
  | EvalSubst -> Eval_jsonnet_subst.eval_program prog
  | EvalEnvir -> Eval_jsonnet_envir.eval_program prog

let manifest_value ?(strategy = default_strategy) v =
  match strategy with
  | EvalSubst -> Eval_jsonnet_subst.manifest_value v
  | EvalEnvir -> Eval_jsonnet_envir.manifest_value v
