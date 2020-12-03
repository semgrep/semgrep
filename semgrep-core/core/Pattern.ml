(*s: semgrep/core/Pattern.ml *)
(*s: pad/r2c copyright *)
(* Yoann Padioleau
 *
 * Copyright (C) 2019-2020 r2c
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file license.txt.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * license.txt for more details.
*)
(*e: pad/r2c copyright *)

(*s: type [[Pattern.t]] *)
(* right now Expr/Stmt/Stmts/Types/Patterns/Partial and probably
 * more are supported *)
type t = AST_generic.any
(*e: type [[Pattern.t]] *)

(*e: semgrep/core/Pattern.ml *)
