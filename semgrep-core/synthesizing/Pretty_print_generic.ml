(* Emma Jin
 *
 * Copyright (C) 2020 r2c
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License (GPL)
 * version 2 as published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * file license.txt for more details.
 *)
open Common

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Pretty print AST generic code (can also be a semgrep pattern).
 *
 * This will be useful for the pattern-from-code synthesizing project but
 * also for correct autofixing.
 *
 * Pretty printing library to use?
 *  - OCaml Format lib
 *  - Martin's easy-format?
 *  - Wadler's pretty-printer combinators?
 *)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)
let expr_to_string _lang _mvars _e =
  raise Todo

let pattern_to_string _lang _any =
  let _mvars = [] in
  raise Todo
