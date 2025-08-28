(*
   Copyright (c) 2022-2024 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
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
