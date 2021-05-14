(*s: semgrep/engine/Transform_rule.ml *)
(*s: pad/r2c copyright *)
(* Yoann Padioleau
 *
 * Copyright (C) 2019-2021 r2c
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

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* The goal of this module is to transform rules mainly for optimization
 * purpose, a bit like a database engine can "compile" SQL queries
 * in the best execution plan.
 *
 * See also Optimizing/Analyze_rule.ml
 *
 *)

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

(*e: semgrep/engine/Transform_rule.ml *)
