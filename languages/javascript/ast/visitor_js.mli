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
open Ast_js

(* the hooks *)
type visitor_in = {
  kexpr : (expr -> unit) * visitor_out -> expr -> unit;
  kstmt : (stmt -> unit) * visitor_out -> stmt -> unit;
  ktop : (a_toplevel -> unit) * visitor_out -> a_toplevel -> unit;
  kprop : (property -> unit) * visitor_out -> property -> unit;
  kparam :
    (parameter_classic -> unit) * visitor_out -> parameter_classic -> unit;
  kinfo : (tok -> unit) * visitor_out -> tok -> unit;
}

and visitor_out = any -> unit

val default_visitor : visitor_in
val mk_visitor : visitor_in -> visitor_out

(* poor's man fold *)
val do_visit_with_ref : ('a list ref -> visitor_in) -> any -> 'a list
