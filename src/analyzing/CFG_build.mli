(*
   Copyright (c) 2021-2024 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
val cfg_of_stmts : ?tok:Tok.t -> IL.stmt list -> IL.cfg * IL.lambdas_cfgs
(** Compute the control flow graph of a sequence of statements.
 *
 * This is useful in cases such as handling top-level instructions that
 * do not constitute actual function definitions.
 *)

val cfg_of_fdef : IL.function_definition -> IL.fun_cfg
(** Compute the control flow graph of an IL function definition. *)

val cfg_of_gfdef :
  Lang.t -> ?ctx:AST_to_IL.ctx -> AST_generic.function_definition -> IL.fun_cfg
(** Same as 'cfg_of_fdef' but takes a Generic function definition. *)
