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
type var = string * AST_generic.sid

type env = {
  lang : Lang.t option;
  constants : (var, AST_generic.svalue) Hashtbl.t;
  attributes : (var, AST_generic.attribute list) Hashtbl.t;
}

val default_env : Lang.t option -> env

(* Partially evaluate a Generic expression *)
val eval : env -> AST_generic.expr -> AST_generic.svalue option

(* helpers reused in Constant_propagation.ml *)
val is_lang : env -> Lang.t -> bool
val is_js : env -> bool

val find_id :
  env -> AST_generic.ident -> AST_generic.id_info -> AST_generic.svalue option
