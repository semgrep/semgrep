let eval_program prog =
  match !Conf_ojsonnet.eval_strategy with
  | EvalSubst -> Eval_jsonnet_subst.eval_program prog
  | EvalEnvir -> Eval_jsonnet_envir.eval_program prog
(*  | EvalStrict -> Eval_jsonnet_strict.eval_program prog *)

let manifest_value v =
  match !Conf_ojsonnet.eval_strategy with
  | EvalSubst -> Eval_jsonnet_subst.manifest_value v
  | EvalEnvir -> Eval_jsonnet_envir.manifest_value v
(*  | EvalStrict -> Eval_jsonnet_strict.manifest_value v *)
