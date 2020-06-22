(* Yoann Padioleau
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
(* Locate AST fragments given a code range.
 *)

(*****************************************************************************)
(* Visitor *)
(*****************************************************************************)
(* This could probably be implemented more efficiently ... but should be
 * good enough in practice.
 * todo? ideally every expression nodes in the AST would have range field
 * associated with it, or at least a special id so we could memoize
 * range for subexpressions.
 *)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let expr_at_range _r _ast =
  raise Todo
