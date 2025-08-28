(*
   Copyright (c) 2024 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
type env

val mk_env : Lang.t -> AST_generic.svalue Dataflow_var_env.t -> env
val eval : env -> IL.exp -> AST_generic.svalue

(* internals used also in Dataflow_svalue.ml *)
val eval_concat : env -> IL.exp list -> AST_generic.svalue

(* lattice ops *)
val refine : AST_generic.svalue -> AST_generic.svalue -> AST_generic.svalue
val union : AST_generic.svalue -> AST_generic.svalue -> AST_generic.svalue
val eq : AST_generic.svalue -> AST_generic.svalue -> bool
