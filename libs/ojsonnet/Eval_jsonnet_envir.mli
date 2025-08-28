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
(* TODO: at some point we probably would prefer an AST_generic.program
 * which could track the origin of tokens through import, eval, and
 * manifestation and that we could pass to osemgrep to evaluate the rules.
 *)

(* entry point; may raise Eval_jsonnet_common.Error *)
val eval_program : Core_jsonnet.program -> Value_jsonnet.t
val manifest_value : Value_jsonnet.t -> JSON.t

val eval_program_with_env :
  Value_jsonnet.env -> Core_jsonnet.program -> Value_jsonnet.t
