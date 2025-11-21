(* Copyright (C) 2025 Semgrep Inc.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * LICENSE for more details.
 *)

(** Human-friendly pretty-printer for IL that outputs familiar-ish syntax.
    Origin information is shown as [<orig> @l.N] where available and useful.

    Example output:
    {[
      x = foo(y) + 1;  // [Call @l.42]
      if (cond) {
        return x;
      }
    ]}
*)

(** {1 Pretty-Printing Functions} *)

val name : IL.name -> string
(** Format a name as "ident:sid" *)

val lval : IL.lval -> string
(** Format an lvalue (e.g., "x", "x.field", "x[i]", "*ptr") *)

val exp : IL.exp -> string
(** Format an expression *)

val instr : ?indent:int -> IL.instr -> string
(** Format an instruction *)

val stmt : ?indent:int -> IL.stmt -> string
(** Format a statement (default identation: 0) *)

val stmts : ?indent:int -> IL.stmt list -> string
(** Format a list of statements *)

(** {1 Definition Pretty-Printing} *)

val function_definition :
  ?name:string ->
  ?indent:int ->
  ?inline:bool ->
  IL.function_definition ->
  string
(** Format a function definition *)

val class_definition :
  ?name:string -> ?indent:int -> ?inline:bool -> IL.class_definition -> string
(** Format a class definition *)

val definition : ?indent:int -> IL.definition -> string
(** Format any definition (function, class, etc.) *)

val program : IL.program -> string
(** Format a complete IL program *)
